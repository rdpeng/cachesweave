######################################################################
## Copyright (C) 2006, Roger D. Peng <rpeng@jhsph.edu>
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

        keys
}

makeChunkDatabaseName <- function(cachedir, options, chunkDigest) {
        file.path(cachedir, paste(options$label, chunkDigest, sep = "_"))
}

mangleDigest <- function(x) {
        paste(".__", x, "__", sep = "")
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
                exprDigest <- mangleDigest(digest(expr))

                ## Create 'stashR' database
                db <- new("localDB", dir = dbName, name = basename(dbName))

                ## If the current expression is not cached, then
                ## evaluate the expression and dump the resulting
                ## objects to the database.  Otherwise, just read the
                ## vector of keys from the database

                keys <- if(!dbExists(db, exprDigest)) 
                        try({
                                evalAndDumpToDB(db, expr, exprDigest)
                        }, silent = TRUE)
                else 
                        dbFetch(db, exprDigest)

                ## If there was an error then just return the
                ## condition object and let Sweave deal with it.
                if(inherits(keys, "try-error"))
                        return(keys)

                dbLazyLoad(db, globalenv(), keys)
        }
        else {
                ## If caching is turned off, just evaluate the expression
                ## in the global environment
                res <- try(.Internal(eval.with.vis(expr, .GlobalEnv,
                                                   baseenv())),
                           silent=TRUE)
                if(inherits(res, "try-error"))
                        return(res)
                if(options$print | (options$term & res$visible))
                        print(res$value)
        }
        res
}

## Need to add the 'cache', 'filename' option to the list
cacheSweaveSetup <- function(file, syntax,
                             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
                             eval=TRUE, split=FALSE, stylepath=TRUE, pdf=TRUE,
                             eps=TRUE, cache = FALSE) {

        out <- utils::RweaveLatexSetup(file, syntax, output=NULL, quiet=FALSE,
                                       debug=FALSE, echo=TRUE, eval=TRUE,
                                       split=FALSE, stylepath=TRUE, pdf=TRUE,
                                       eps=TRUE)

######################################################################
        ## Additions here [RDP]
        ## Add the (non-standard) options for code chunks with caching
        out$options[["cache"]] <- cache

        ## We assume that each .Rnw file gets its own map file
        out[["mapFile"]] <- makeMapFileName(file)
        file.create(out[["mapFile"]])  ## Overwrite an existing file

        ## End additions [RDP]
######################################################################
        out
}


makeMapFileName <- function(Rnwfile) {
        mapfile <- sub("\\.Rnw$", "\\.map", Rnwfile)

        ## Don't clobber
        if(identical(mapfile, Rnwfile))
                mapfile <- paste(Rnwfile, "map", sep = ".")
        mapfile
}

writeChunkMetadata <- function(object, chunk, options) {
        chunkprefix <- utils::RweaveChunkPrefix(options)
        chunkexps <- parse(text = chunk)
        chunkDigest <- digest(chunkexps)

        options$chunkDigest <- chunkDigest
        
        ## If there's a data map file then write the chunk name and the
        ## directory of the chunk database to the map file (in DCF format)
        dbName <- if(isTRUE(options$cache))
                makeChunkDatabaseName(getCacheDir(), options, chunkDigest)
        else
                ""
        ## Capture figure filenames; default to PDF, otherwise use EPS.
        ## Filenames are <chunkprefix>.<extenstion>, which could change in
        ## the future depending on Sweave implementation details
        figname <- ""
        if(options$fig && options$eval) {
                figname <- if(options$pdf)
                        paste(chunkprefix, "pdf", sep = ".")
                else if(options$eps)
                        paste(chunkprefix, "eps", sep = ".")
                else
                        ""
        }
        ## Write out map file entry
        mapFile <- object[["mapFile"]]
        mapEntry <- data.frame(chunk = options$label,
                               chunkprefix = chunkprefix,
                               fig = figname,
                               cacheDB = dbName,
                               time = Sys.time())
        write.dcf(mapEntry, file = mapFile, append = TRUE, width = 2000)
        options
}

## This function is essentially unchanged from the original Sweave
## version, except I compute the digest of the entire chunk, write out
## information to the map file, and use 'cacheSweaveEvalWithOpt'
## instead.  Note that everything in this function operates at the
## chunk level.  The code has been copied from R 2.5.0.

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
                ## [x][[1]] avoids partial matching of x
                chunkout <- object$chunkout[chunkprefix][[1]]
                if(is.null(chunkout)){
                        chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
                        if(!is.null(options$label))
                                object$chunkout[[chunkprefix]] <- chunkout
                }
        }
        else
                chunkout <- object$output

        saveopts <- options(keep.source=options$keep.source)
        on.exit(options(saveopts))

        SweaveHooks(options, run=TRUE)

        ## parse entire chunk block
        chunkexps <- try(parse(text=chunk), silent=TRUE)
        RweaveTryStop(chunkexps, options)

        ## Adding my own stuff here [RDP]
        ## Add 'chunkDigest' to 'options'
        options <- writeChunkMetadata(object, chunk, options)
        ## End adding my own stuff [RDP]

        openSinput <- FALSE
        openSchunk <- FALSE

        if(length(chunkexps)==0)
                return(object)

        srclines <- attr(chunk, "srclines")
        linesout <- integer(0)
        srcline <- srclines[1]

        srcrefs <- attr(chunkexps, "srcref")
        if (options$expand)
                lastshown <- 0
        else
                lastshown <- srcline - 1
        thisline <- 0
        for(nce in 1:length(chunkexps))
        {
                ce <- chunkexps[[nce]]
                if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
                        if (options$expand) {
                                srcfile <- attr(srcref, "srcfile")
                                showfrom <- srcref[1]
                                showto <- srcref[3]
                        } else {
                                srcfile <- object$srcfile
                                showfrom <- srclines[srcref[1]]
                                showto <- srclines[srcref[3]]
                        }
                        dce <- getSrcLines(srcfile, lastshown+1, showto)
                        leading <- showfrom-lastshown
                        lastshown <- showto
                        srcline <- srclines[srcref[3]]
                        while (length(dce) && length(grep("^[ \\t]*$", dce[1]))) {
                                dce <- dce[-1]
                                leading <- leading - 1
                        }
                } else {
                        dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
                        leading <- 1
                }
                if(object$debug)
                        cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
                if(options$echo && length(dce)){
                        if(!openSinput){
                                if(!openSchunk){
                                        cat("\\begin{Schunk}\n",
                                            file=chunkout, append=TRUE)
                                        linesout[thisline + 1] <- srcline
                                        thisline <- thisline + 1
                                        openSchunk <- TRUE
                                }
                                cat("\\begin{Sinput}",
                                    file=chunkout, append=TRUE)
                                openSinput <- TRUE
                        }
                        cat("\n", paste(getOption("prompt"), dce[1:leading], sep="", collapse="\n"),
                            file=chunkout, append=TRUE, sep="")
                        if (length(dce) > leading)
                                cat("\n", paste(getOption("continue"), dce[-(1:leading)], sep="", collapse="\n"),
                                    file=chunkout, append=TRUE, sep="")
                        linesout[thisline + 1:length(dce)] <- srcline
                        thisline <- thisline + length(dce)
                }

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
                if(length(output)==1 & output[1]=="") output <- NULL

                RweaveTryStop(err, options)

                if(object$debug)
                        cat(paste(output, collapse="\n"))

                if(length(output)>0 & (options$results != "hide")){

                        if(openSinput){
                                cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                                linesout[thisline + 1:2] <- srcline
                                thisline <- thisline + 2
                                openSinput <- FALSE
                        }
                        if(options$results=="verbatim"){
                                if(!openSchunk){
                                        cat("\\begin{Schunk}\n",
                                            file=chunkout, append=TRUE)
                                        linesout[thisline + 1] <- srcline
                                        thisline <- thisline + 1
                                        openSchunk <- TRUE
                                }
                                cat("\\begin{Soutput}\n",
                                    file=chunkout, append=TRUE)
                                linesout[thisline + 1] <- srcline
                                thisline <- thisline + 1
                        }

                        output <- paste(output,collapse="\n")
                        if(options$strip.white %in% c("all", "true")){
                                output <- sub("^[[:space:]]*\n", "", output)
                                output <- sub("\n[[:space:]]*$", "", output)
                                if(options$strip.white=="all")
                                        output <- sub("\n[[:space:]]*\n", "\n", output)
                        }
                        cat(output, file=chunkout, append=TRUE)
                        count <- sum(strsplit(output, NULL)[[1]] == "\n")
                        if (count > 0) {
                                linesout[thisline + 1:count] <- srcline
                                thisline <- thisline + count
                        }

                        remove(output)

                        if(options$results=="verbatim"){
                                cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
                                linesout[thisline + 1:2] <- srcline
                                thisline <- thisline + 2
                        }
                }
        }

        if(openSinput){
                cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                linesout[thisline + 1:2] <- srcline
                thisline <- thisline + 2
        }

        if(openSchunk){
                cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
                linesout[thisline + 1] <- srcline
                thisline <- thisline + 1
        }

        if(is.null(options$label) & options$split)
                close(chunkout)

        if(options$split & options$include){
                cat("\\input{", chunkprefix, "}\n", sep="",
                    file=object$output, append=TRUE)
                linesout[thisline + 1] <- srcline
                thisline <- thisline + 1
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
                        linesout[thisline + 1] <- srcline
                        thisline <- thisline + 1
                }
        }
        object$linesout <- c(object$linesout, linesout)
        return(object)
}
































