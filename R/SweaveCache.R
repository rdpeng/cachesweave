######################################################################
## Copyright (C) 2006-2010, Roger D. Peng <rpeng@jhsph.edu>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
#####################################################################

######################################################################
## Taken/adapted from Sweave code by Friedrich Leisch, along the lines
## of 'weaver' from Bioconductor, but more naive and we use 'stashR'
## databases for the backend.  

cacheSweaveDriver <- function() {
        list(
             setup = cacheSweaveSetup,
	     runcode = makeCacheSweaveCodeRunner(cacheSweaveEvalWithOpt),
             writedoc = utils::RweaveLatexWritedoc,
             finish = utils::RweaveLatexFinish,
             checkopts = cacheSweaveLatexOptions
             )
}

cacheSweaveLatexOptions <- function(options) {
	moreoptions <- c('dependson','cache')
	oldoptions <- options[setdiff(names(options),moreoptions)]
	newoptions <- options[intersect(names(options),moreoptions)]
	Rweaveoptions <- utils::RweaveLatexOptions(oldoptions)
	options <- unlist(list(Rweaveoptions,newoptions),recursive=F)
}

cacheTangleDriver <- function() {
	list(setup = utils::RtangleSetup,
	     runcode = utils:::RtangleRuncode,
	     writedoc = utils::RtangleWritedoc,
	     finish = utils:::RtangleFinish,
	     checkopts = cacheSweaveLatexOptions)
}

######################################################################
## Take a 'filehash' database and insert a bunch of key/value pairs

dumpToDB <- function(db, list = character(0), envir = parent.frame()) {
        if(!is(db, "filehash"))
                stop("'db' should be a 'filehash' database")
        for(i in seq(along = list))
                dbInsert(db, list[i], get(list[i], envir, inherits = FALSE))
        invisible(db)
}

copy2env <- function(keys, fromEnv, toEnv) {
        for(key in keys) {
                assign(key, get(key, fromEnv, inherits = FALSE), toEnv)
        }
}

## Take an environment and return a copy.  Not an exact copy because
## we don't get all keys (not sure why, but for some reason I remember
## that getting all the keys caused problems.

copyEnv <- function(from) {
        env <- new.env(parent = parent.env(from))
        keys <- ls(from, all.names = FALSE)

        for(key in keys) 
                assign(key, get(key, from, inherits = FALSE), env)
        env
}

isNewOrModified <- function(specials, e1, e2) {
        sapply(specials, function(s) {
                in1 <- exists(s, e1, inherits = FALSE)
                in2 <- exists(s, e2, inherits = FALSE)
                is.new <- !in1 && in2
                is.deleted <- in1 && !in2
                
                if((!in1 && !in2) || is.deleted)
                        FALSE
                else if(is.new)
                        TRUE
                else 
                        !identical(get(s, e1, inherits = FALSE),
                                   get(s, e2, inherits = FALSE))
        })
}

## Check for new symbols in 'e2' that are not in 'e1'; doesn't check
## for modified symbols.

## If 'source()' was used, there may be new symbols in the global
## environment, unless 'source(local = TRUE)' was used.  Also applies
## for 'set.seed()'.

checkNewSymbols <- function(e1, e2) {
        if(identical(e1, e2))
                return(character(0))
        specials <- c(".Random.seed")

        ## Don't check for names beginning with '.' for now
        sym1 <- ls(e1)
        sym2 <- ls(e2)
        newsym <- setdiff(sym2, sym1)

        use <- isNewOrModified(specials, e1, e2)
        c(newsym, specials[use])
}

## Take an expression, evaluate it in a local environment and dump the
## results to a database.  Associate the names of the dumped objects
## with a digest of the expression.  Return a character vector of keys
## that were dumped

evalAndDumpToDB <- function(db, expr, exprDigest, chunkDigest) {
        env <- new.env(parent = globalenv())
        global1 <- copyEnv(globalenv())
        
        eval(expr, env)

        global2 <- copyEnv(globalenv())

        ## Functions like 'source' and 'set.seed' alter the global
        ## environment, so check after evaluation
        new.global <- checkNewSymbols(global1, global2)
        copy2env(new.global, globalenv(), env)

        ## Get newly assigned object names
        keys <- ls(env, all.names = TRUE)
	newkey <- paste('.cacheSweave.creation.time.',chunkDigest,sep='')
	keys <- c(keys, newkey)
	assign(newkey, Sys.time(), envir=env)

        ## Associate the newly created keys with the digest of
        ## the expression
        dbInsert(db, exprDigest, keys)

        ## Dump the values of the keys to the database
        dumpToDB(db, list = keys, envir = env)

	if(length(keys) > 0)
		copy2env(keys, env, globalenv())
        keys
}

makeChunkDatabaseName <- function(cachedir, options, chunkDigest) {
        file.path(cachedir, paste('cacheSweaveStorage',options$label, chunkDigest, sep = "_"))
}

mangleDigest <- function(x) {
        paste(".__", x, "__", sep = "")
}

hash <- function(object) {
	digest(object, algo = "sha1")
}

hashExpr <- function(expr) {
	expr <- deparse(expr, width.cutoff = 60)
	hash(expr)
}

################################################################################
## The major addition is here: Rather than evaluate expressions and
## leave them in the global environment, we evaluate them in a local
## environment (that has globalenv() as the parent) and then store the
## assignments in a 'stashR' database.  If an expression does not give
## rise to new R objects, then nothing is saved.
##
## For each expression ('expr'), we compute a digest and associate
## with that digest the names of the objects that were created by
## evaluating the expression.  That way, for a given cached
## expression, we know which keys to lazy-load from the cache when
## evaluation is skipped.
################################################################################

cacheSweaveEvalWithOpt <- function (expr, options) {
        chunk <- get("chunk", parent.frame())
        chunkDigest <- hashExpr(parse(text = chunk, srcfile = NULL))
        
        ## 'expr' is a single expression, so something like 'a <- 1'
        res <- NULL

        if(!options$eval)
		return(res)

	cachedir <- getCacheDir()
	## Create database name from chunk label and MD5
	## digest
	dbName <- makeChunkDatabaseName(cachedir, options, chunkDigest)
	exprDigest <- mangleDigest(hashExpr(expr))

	## Create 'stashR' database
	db <- new("localDB", dir = dbName, name = basename(dbName))

	trace <- options$trace & as.logical(options$trace)

	chunkName <- metaChunkName(options)
	dbMetaName <- makeMetaDatabaseName(cachedir)
	dbMeta <- new("localDB", dir = dbMetaName, name = basename(dbMetaName))
	creationTimes <- metaGetCreationTime(dbMeta)
	fresh = dbExists(db, exprDigest)

	if (!is.null(creationTimes[[chunkName]])) {
		chunkCreationTime <- creationTimes[[chunkName]]
		if(trace) cat("%",chunkName,"has creationTime:",format.Date(chunkCreationTime),"\n")
	} else
		chunkCreationTime <- NULL

	fresh = fresh & !is.null(chunkCreationTime)
	if (!is.null(options$dependson)){
		depends <- unlist(strsplit(options$dependson,';'))
		if (fresh)
			for (dep in depends) {
				dirty1 = is.null(creationTimes[[dep]])
			        if(!dirty1)
					dirty1 = creationTimes[[dep]] > chunkCreationTime
				if (trace)
					if (dirty1)
						cat("% in",chunkName,format.Date(chunkCreationTime),"dependency",dep,"is newer",format.Date(creationTimes[[dep]]),"\n")
					else
						cat("% in",chunkName,format.Date(chunkCreationTime),"dependency",dep,"is older",format.Date(creationTimes[[dep]]),"\n")
				fresh = fresh & !dirty1
			}
	} else {
		depends = NULL
	}
	if (trace) {
		if (fresh) {
			cat("%",chunkName,"is fresh\n")
		} else {
			cat("%",chunkName,"is dirty\n")
		}
        }
	updated <- FALSE

        if(options$cache) {
                ## If the current expression is not cached, then
                ## evaluate the expression and dump the resulting
                ## objects to the database.  Otherwise, just read the
                ## vector of keys from the database

                if(!fresh) {
                        keys <- try({
                                evalAndDumpToDB(db, expr, exprDigest, chunkDigest)
                        }, silent = TRUE)
			if(trace) {
				out <- "% evaluating and storing"
				out <- paste(out,chunkName)
				if (!is.null(depends)) {
					out <- paste(out," depends (",sep='')
					out <- paste(out, paste(depends,collapse=', '),sep='')
					out <- paste(out,")",sep='')
				}
				out <- paste(out,Sys.time())
				cat(out,"\n")
			}

			## If there was an error then just return the
			## condition object and let Sweave deal with it.
			if(inherits(keys, "try-error"))
				return(keys)
			updated <- TRUE
		}
		else {
       			if(trace) {
				cat("% fetching object",chunkName)
			}
			keys <- dbFetch(db, exprDigest)
			dbLazyLoad(db, globalenv(), keys)
       			if(trace) {
                                if (!is.null(depends)) {
                                        cat(" depending on",paste(depends,collapse=', '))
                                }
                                cat("\n")
			}
		}
		res <- keys
        }
        else {
                ## If caching is turned off, just evaluate the expression
                ## in the global environment
                res <- utils::RweaveEvalWithOpt(expr, options)
        }
	if (updated)
		assign("updatedChunk", TRUE, parent.frame(n=2))
	res
}

makeMetaDatabaseName <- function(cachedir) {
	file.path(cachedir, "cacheSweaveStorage_metadata")
}

metaGetCreationTime <- function(dbMeta) {
	if(dbExists(dbMeta,"creationTimes")) {
		creationTimes <- dbFetch(dbMeta, "creationTimes")
	} else {
		creationTimes <- list()
	}
	creationTimes
}

metaSetCreationTime <- function(label) {
	cachedir <- getCacheDir()
	dbMetaName <- makeMetaDatabaseName(cachedir)
	dbMeta <- new("localDB", dir = dbMetaName, name = basename(dbMetaName))
	creationTimes <- metaGetCreationTime(dbMeta)
	creationTimes[[label]] =Sys.time()
	dbInsert(dbMeta, "creationTimes", creationTimes)
	creationTimes
}
metaChunkName <- function(options) {
	if(!is.null(options$label))
		chunkName <- options$label
	else
		chunkName <- paste("c",options$chunkDigest,sep='')
	chunkName
}

## Need to add the 'cache', 'filename', 'trace' and 'dependson' options to the list
cacheSweaveSetup <- function(..., cache = FALSE, trace=F, dependson=NULL) {
        out <- utils::RweaveLatexSetup(...)

        ## Add the (non-standard) options for code chunks with caching
        out$options[["cache"]] <- cache
	out$options[["dependson"]] <- dependson
	out$options[["trace"]] <- trace
        out
}

makeCacheSweaveCodeRunner <- function(evalFunc = cacheSweaveEvalWithOpt) {
	runner <- makeRweaveLatexCodeRunner(evalFunc)
	function(object, chunk, options) {
		updatedChunk <- FALSE
		e <- runner(object, chunk, options)
		flag <- 'L'
		if(updatedChunk) {
			chunkName <- metaChunkName(options)
			metaSetCreationTime(chunkName)
			flag <- 'S'
		}
		n <- nchar(as.character(options$chunknr))
		# overwrites the : on preceding row with flag using ANSI
		cat("[F[",n+2,"C",flag,"\n",sep='')
		e
	}

}




































