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
## databases for the backend.  We also don't check dependencies on
## previous chunks.

cacheSweaveDriver <- function() {
        list(
             setup = cacheSweaveSetup,
             runcode = cacheSweaveRuncode,
             writedoc = utils::RweaveLatexWritedoc,
             finish = utils::RweaveLatexFinish,
             checkopts = utils::RweaveLatexOptions
             )
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

evalAndDumpToDB <- function(db, expr, exprDigest) {
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
        file.path(cachedir, paste(options$label, chunkDigest, sep = "_"))
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
## The major modification is here: Rather than evaluate expressions
## and leave them in the global environment, we evaluate them in a
## local environment (that has globalenv() as the parent) and then
## store the assignments in a 'stashR' database.  If an expression
## does not give rise to new R objects, then nothing is saved.
##
## For each expression ('expr'), we compute a digest and associate
## with that digest the names of the objects that were created by
## evaluating the expression.  That way, for a given cached
## expression, we know which keys to lazy-load from the cache when
## evaluation is skipped.
################################################################################

cacheSweaveEvalWithOpt <- function (expr, options) {
        chunkDigest <- options$chunkDigest
        
        ## 'expr' is a single expression, so something like 'a <- 1'
        res <- NULL

        if(!options$eval)
                return(res)
        if(options$cache) {
                cachedir <- getCacheDir()

                ## Create database name from chunk label and MD5
                ## digest
                dbName <- makeChunkDatabaseName(cachedir, options, chunkDigest)
                exprDigest <- mangleDigest(hashExpr(expr))

                ## Create 'stashR' database
                db <- new("localDB", dir = dbName, name = basename(dbName))

                ## If the current expression is not cached, then
                ## evaluate the expression and dump the resulting
                ## objects to the database.  Otherwise, just read the
                ## vector of keys from the database

                if(!dbExists(db, exprDigest)) {
                        keys <- try({
                                evalAndDumpToDB(db, expr, exprDigest)
                        }, silent = TRUE)

			## If there was an error then just return the
			## condition object and let Sweave deal with it.
			if(inherits(keys, "try-error"))
				return(keys)
		}
		else {
                        keys <- dbFetch(db, exprDigest)
			dbLazyLoad(db, globalenv(), keys)
		}
		keys
        }
        else {
                ## If caching is turned off, just evaluate the expression
                ## in the global environment
                res <- try(withVisible(eval(expr, .GlobalEnv)),
                           silent=TRUE)
                if(inherits(res, "try-error"))
                        return(res)
                if(options$print | (options$term & res$visible))
                        print(res$value)
        }
        res
}

## Need to add the 'cache', 'filename' option to the list
cacheSweaveSetup <- function(..., cache = FALSE) {

        out <- utils::RweaveLatexSetup(...)

######################################################################
        ## Additions here [RDP]
        ## Add the (non-standard) options for code chunks with caching
        out$options[["cache"]] <- cache

        ## End additions [RDP]
######################################################################
        out
}


## This function is essentially unchanged from the original Sweave
## version, except I use 'cacheSweaveEvalWithOpt' instead.  Note that
## everything in this function operates at the chunk level.  The code
## has been copied from R 2.5.0.

cacheSweaveRuncode <- function(object, chunk, options) {
        if(!(options$engine %in% c("R", "S"))){
                return(object)
        }

        if(!object$quiet){
                cat(formatC(options$chunknr, width=2), ":")
                if(options$echo) cat(" echo")
                if(options$keep.source) cat(" keep.source")
                if(options$eval){
                        if(options$print) cat(" print")
                        if(options$term) cat(" term")
                        cat("", options$results)
                        if(options$fig){
                                if(options$eps) cat(" eps")
                                if(options$pdf) cat(" pdf")
                        }
                }
                if(!is.null(options$label))
                        cat(" (label=", options$label, ")", sep="")
                cat("\n")
        }

        chunkprefix <- RweaveChunkPrefix(options)

        if(options$split){
                ## [x][[1L]] avoids partial matching of x
                chunkout <- object$chunkout[chunkprefix][[1L]]
                if(is.null(chunkout)){
                        chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
                        if(!is.null(options$label))
                                object$chunkout[[chunkprefix]] <- chunkout
                }
        }
        else
                chunkout <- object$output

        srcfile <- object$srcfile
        SweaveHooks(options, run=TRUE)

        ## Note that we edit the error message below, so change both
        ## if you change this line:
        chunkexps <- try(parse(text=chunk, srcfile=srcfile), silent=TRUE)

        if (inherits(chunkexps, "try-error"))
                chunkexps[1L] <- sub(" parse(text = chunk, srcfile = srcfile) : \n ",
                                     "", chunkexps[1L], fixed = TRUE)

        RweaveTryStop(chunkexps, options)

        ## A couple of functions used below...

        putSinput <- function(dce){
                if(!openSinput){
                        if(!openSchunk){
                                cat("\\begin{Schunk}\n",
                                    file=chunkout, append=TRUE)
                                linesout[thisline + 1L] <<- srcline
                                thisline <<- thisline + 1L
                                openSchunk <<- TRUE
                        }
                        cat("\\begin{Sinput}",
                            file=chunkout, append=TRUE)
                        openSinput <<- TRUE
                }
                cat("\n", paste(getOption("prompt"), dce[1L:leading], sep="", collapse="\n"),
                    file=chunkout, append=TRUE, sep="")
                if (length(dce) > leading)
                        cat("\n", paste(getOption("continue"), dce[-(1L:leading)], sep="", collapse="\n"),
                            file=chunkout, append=TRUE, sep="")
                linesout[thisline + seq_along(dce)] <<- srcline
                thisline <<- thisline + length(dce)
        }	  
        
        trySrcLines <- function(srcfile, showfrom, showto, ce) {
                lines <- try(suppressWarnings(getSrcLines(srcfile, showfrom, showto)), silent=TRUE)
                if (inherits(lines, "try-error")) {
                        if (is.null(ce)) lines <- character(0)
                        else lines <- deparse(ce, width.cutoff=0.75*getOption("width"))
                }
                lines
        }          

        chunkregexp <- "(.*)#from line#([[:digit:]]+)#"

        ## Additions here [RDP]
        options$chunkDigest <- hashExpr(parse(text = chunk, srcfile = NULL))
        ## [RDP]
        
        openSinput <- FALSE
        openSchunk <- FALSE

        srclines <- attr(chunk, "srclines")
        linesout <- integer(0L) # maintains concordance
        srcline <- srclines[1L] # current input line
        thisline <- 0L          # current output line
        lastshown <- srcline    # last line already displayed;
                                        # at this point it's the <<>>= line
        leading <- 1L		  # How many lines get the user prompt
        
        srcrefs <- attr(chunkexps, "srcref")
        
        for(nce in seq_along(chunkexps)) {
 		ce <- chunkexps[[nce]]
                if (options$keep.source && nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
                        srcfile <- attr(srcref, "srcfile")
                        showfrom <- srcref[1L]
                        showto <- srcref[3L]
                        refline <- srcfile$refline
                        if (is.null(refline)) {
                                if (grepl(chunkregexp, srcfile$filename)) {
                                        refline <- as.integer(sub(chunkregexp, "\\2", srcfile$filename))
                                        srcfile$filename <- sub(chunkregexp, "\\1", srcfile$filename)
                                } else 
                                refline <- NA
                                srcfile$refline <- refline
                        }
                        if (!options$expand && !is.na(refline)) 
                                showfrom <- showto <- refline
                    	
                        if (!is.na(refline) || is.na(lastshown)) { 
                                ## We expanded a named chunk for this
                                ## expression or the previous one
                                dce <- trySrcLines(srcfile, showfrom, showto, ce)
                                leading <- 1L
                                if (!is.na(refline))
                                        lastshown <- NA
                                else
                                        lastshown <- showto
                        } else {
                                dce <- trySrcLines(srcfile, lastshown+1L, showto, ce)
                                leading <- showfrom-lastshown
                                lastshown <- showto
                        }
                        srcline <- showto
                        while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
                                dce <- dce[-1L]
                                leading <- leading - 1L
                        }
	    	} else {
                        dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
                        leading <- 1L
                }
                if(object$debug)
                        cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
                
                if(options$echo && length(dce))
                        putSinput(dce)
                
                ## tmpcon <- textConnection("output", "w")
                ## avoid the limitations (and overhead) of output text connections
                tmpcon <- file()
                sink(file=tmpcon)
                err <- NULL

                ## [RDP] change this line to use my EvalWithOpt function
                if(options$eval) err <- cacheSweaveEvalWithOpt(ce, options)
                ## [RDP] end change

                cat("\n") # make sure final line is complete
                sink()
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if(length(output) == 1L & output[1L] == "") output <- NULL

                RweaveTryStop(err, options)

                if(object$debug)
                        cat(paste(output, collapse="\n"))

                if(length(output) & (options$results != "hide")){

                        if(openSinput){
                                cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                                linesout[thisline + 1L:2L] <- srcline
                                thisline <- thisline + 2L
                                openSinput <- FALSE
                        }
                        if(options$results=="verbatim"){
                                if(!openSchunk){
                                        cat("\\begin{Schunk}\n",
                                            file=chunkout, append=TRUE)
                                        linesout[thisline + 1L] <- srcline
                                        thisline <- thisline + 1L
                                        openSchunk <- TRUE
                                }
                                cat("\\begin{Soutput}\n",
                                    file=chunkout, append=TRUE)
                                linesout[thisline + 1L] <- srcline
                                thisline <- thisline + 1L
                        }

                        output <- paste(output,collapse="\n")
                        if(options$strip.white %in% c("all", "true")){
                                output <- sub("^[[:space:]]*\n", "", output)
                                output <- sub("\n[[:space:]]*$", "", output)
                                if(options$strip.white=="all")
                                        output <- sub("\n[[:space:]]*\n", "\n", output)
                        }
                        cat(output, file=chunkout, append=TRUE)
                        count <- sum(strsplit(output, NULL)[[1L]] == "\n")
                        if (count > 0L) {
                                linesout[thisline + 1L:count] <- srcline
                                thisline <- thisline + count
                        }

                        remove(output)

                        if(options$results=="verbatim"){
                                cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
                                linesout[thisline + 1L:2L] <- srcline
                                thisline <- thisline + 2L
                        }
                }
        }

        if(options$echo && options$keep.source 
           && !is.na(lastshown) 
           && lastshown < (showto <- srclines[length(srclines)])) {
                dce <- trySrcLines(srcfile, lastshown+1L, showto, NULL)
                putSinput(dce)
        }
        
        if(openSinput){
                cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                linesout[thisline + 1L:2L] <- srcline
                thisline <- thisline + 2L
        }

        if(openSchunk){
                cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
                linesout[thisline + 1L] <- srcline
                thisline <- thisline + 1L
        }

        if(is.null(options$label) & options$split)
                close(chunkout)

        if(options$split & options$include){
                cat("\\input{", chunkprefix, "}\n", sep="",
                    file=object$output, append=TRUE)
                linesout[thisline + 1L] <- srcline
                thisline <- thisline + 1L
        }

        if(options$fig && options$eval){
                if(options$eps){
                        grDevices::postscript(file=paste(chunkprefix, "eps", sep="."),
                                              width=options$width, height=options$height,
                                              paper="special", horizontal=FALSE)

                        err <- try({SweaveHooks(options, run=TRUE)
                                    eval(chunkexps, envir=.GlobalEnv)})
                        grDevices::dev.off()
                        if(inherits(err, "try-error")) stop(err)
                }
                if(options$pdf){
                        grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
                                       width=options$width, height=options$height,
                                       version=options$pdf.version,
                                       encoding=options$pdf.encoding)

                        err <- try({SweaveHooks(options, run=TRUE)
                                    eval(chunkexps, envir=.GlobalEnv)})
                        grDevices::dev.off()
                        if(inherits(err, "try-error")) stop(err)
                }
                if(options$include) {
                        cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                            file=object$output, append=TRUE)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                }
        }
        object$linesout <- c(object$linesout, linesout)
        return(object)
}



































