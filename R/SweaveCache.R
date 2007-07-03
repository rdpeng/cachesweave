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
## of 'weaver' from Bioconductor, but more naive and we use a
## different database format for storing the cached computations.  We
## also don't check dependencies on previous chunks.

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

        for(key in keys) {
                obj <- get(key, from, inherits = FALSE)
                assign(key, obj, env, inherits = FALSE)
        }
        env
}

## Check for new symbols in 'e2' that are not in 'e1'

isNewOrModified <- function(symbolnames, e1, e2) {
        sapply(symbolnames, function(s) {
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

## If 'source()' was used, there may be new symbols in the global
## environment, unless 'source(local = TRUE)' was used.  Also applies
## for 'set.seed()'.
        
checkNewSymbols <- function(e1, e2) {
        if(identical(e1, e2))
                return(character(0))
        specials <- c(".Random.seed")

        ## Don't check for names beginning with '.' for now
        allsym <- unique(c(ls(e1), ls(e2), specials))

        use <- isNewOrModified(allsym, e1, e2)
        allsym[use]
}

## Take an expression, evaluate it in a local environment and dump the
## results to a database.  Associate the names of the dumped objects
## with a digest of the expression.  Return a character vector of keys
## that were dumped

## I *think* the 'copyEnv' stuff is okay w.r.t efficiency because the
## objects in 'global1' and 'global2' are never modified and therefore
## do not end up using extra memory.

evalAndCache <- function(expr, exprFile, cache = TRUE) {
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

        if(length(keys) == 0 && !checkForceEvalList(expr)) {
                ## message("expression has side effect: ", digest(expr))
                updateForceEvalList(expr)
        }
        if(cache) 
                saveWithIndex(keys, exprFile, env)
        env
}

exprFileName <- function(cachedir, options, exprDigest) {
        chunkdir <- makeChunkDirName(cachedir, options)
        file.path(chunkdir, exprDigest)
}

makeChunkDirName <- function(cachedir, options) {
        file.path(cachedir, paste(options$label, options$chunkDigest,
                                  sep = "_"))
}

################################################################################
## Handling expressions with side effects

sideEffectListFile <- function() {
        file.path(getCacheDir(), ".ForceEvalList")
}

updateForceEvalList <- function(expr) {
        exprDigest <- digest(expr)
        con <- file(sideEffectListFile(), "a")
        on.exit(close(con))
        
        writeLines(exprDigest, con)
}

initForceEvalList <- function() {
        file <- sideEffectListFile()

        ## This is probably not necessary....
        if(!file.exists(file))
                file.create(file)
        invisible(file)
}

checkForceEvalList <- function(expr) {
        exprDigest <- digest(expr)
        exprList <- readLines(sideEffectListFile())
        exprDigest %in% exprList
}

################################################################################
## The major modification is here: Rather than evaluate expressions
## and leave them in the global environment, we evaluate them in a
## local environment (that has globalenv() as the parent) and then
## store the assignments in a database.  If an expression does not
## give rise to new R objects, then nothing is saved.

cacheSweaveEvalWithOpt <- function (expr, options) {
        ## 'expr' is a single expression, so something like 'a <- 1'
        res <- NULL
        
        if(!options$eval)
                return(res)
        if(options$cache && !checkForceEvalList(expr)) {
                cachedir <- getCacheDir()
                chunkdir <- makeChunkDirName(cachedir, options)

                if(!file.exists(chunkdir))
                        dir.create(chunkdir, recursive = TRUE)
                exprDigest <- digest(expr, algo = "md5")
                exprFile <- exprFileName(cachedir, options, exprDigest)

                ## If the current expression is not cached, then
                ## evaluate the expression and dump the resulting
                ## objects to the database.
                res <- if(!file.exists(exprFile)) {
                        try({
                                withVisible({
                                        evalAndCache(expr, exprFile)
                                })
                        }, silent = TRUE)
                }
                else  
                        NULL  ## load from cache

                ## (If there was an error then just return the
                ## condition object and let Sweave deal with it.)
                if(inherits(res, "try-error"))
                        return(res)
                
                lazyLoad(exprFile, globalenv())
        }
        else {
                res <- try({
                        withVisible({
                                eval(expr, globalenv(), baseenv())
                        })
                }, silent = TRUE)
                if(inherits(res, "try-error"))
                        return(res)
                ## copy2env(ls(env, all.names = TRUE), env, globalenv())
        }
        if(!is.null(res) && (options$print | (options$term & res$visible)))
                print(res$value)
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

        cachedir <- getCacheDir()

        if(!file.exists(cachedir))
                dir.create(cachedir)
        
        ## We assume that each .Rnw file gets its own map file
        out[["mapFile"]] <- makeMapFileName(file)
        file.create(out[["mapFile"]])  ## Overwrite an existing file

        initForceEvalList()
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
        chunkDigest <- digest(chunkexps, algo = "md5")
        options$chunkDigest <- chunkDigest
        
        ## If there's a data map file then write the chunk name and the
        ## directory of the chunk database to the map file (in DCF format)
        dbName <- if(options$cache)
                makeChunkDirName(getCacheDir(), options)
        else
                ""
        ## Capture figure filenames; default to PDF, otherwise use EPS.
        ## Filenames are <chunkprefix>.<extension>, which could change in
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
                               chunkDigest = chunkDigest,
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
































