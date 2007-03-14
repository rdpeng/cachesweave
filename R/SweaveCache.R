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

setBaseURL <- function(URL) {
    assign("baseURL", URL, .cacheEnv)
}

getBaseURL <- function() {
    get("baseURL", .cacheEnv, inherits = FALSE)
}

setCacheDir <- function(path) {
    assign("cacheDir", path, .cacheEnv)
    dir.create(path, showWarnings = FALSE)
}

getCacheDir <- function() {
    get("cacheDir", .cacheEnv, inherits = FALSE)
}

######################################################################
######################################################################
## Taken/adapted from Sweave code by Friedrich Leisch, along the lines
## of 'weaver' from Bioconductor, but more naive and we use 'stashR'
## databases for the backend.  We also don't check dependencies on
## previous chunks.

cacheSweaveDriver <- function() {
    list(
         setup = cacheSweaveSetup,
         runcode = cacheSweaveRuncode,
         writedoc = utils:::RweaveLatexWritedoc,
         finish = utils:::RweaveLatexFinish,
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

## Take an expression, evaluate it in a local environment and dump the
## results to a database.  Associate the names of the dumped objects
## with a digest of the expression.

evalAndDumpToDB <- function(db, expr, exprDigest) {
    env <- new.env(parent = globalenv())
    keys.global0 <- ls(globalenv())

    ## Evaluate the expression
    eval(expr, env)

    ## If 'source()' was used, there may be new symbols in the global
    ## environment, unless 'source(local = TRUE)' was used
    keys.global1 <- ls(globalenv())
    new.global <- setdiff(keys.global1, keys.global0)

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

cacheSweaveEvalWithOpt <- function (expr, options, chunkDigest){
    ## 'expr' is a single expression, so something like 'a <- 1'
    res <- NULL

    if(options$eval){
        if(options$cache) {
            cachedir <- getCacheDir()

            ## Create database name from chunk label and chunk MD5
            ## digest
            dbName <- makeChunkDatabaseName(cachedir, options, chunkDigest)

            ## Take an MD5 digest of the expression; mangle the name
            ## of the digest so it doesn't show up with 'ls()'
            exprDigest <- mangleDigest(digest(expr))

            ## Use 'localDB' database from 'stashR' package; all
            ## necessary directories are created by the 'initialize()'
            ## method.  If directories already exist, nothing is done.
            db <- new("localDB", dir = dbName, name = basename(dbName))

            ## Now that we have a database, check to see if the
            ## current expression has been evaluated already (and
            ## therefore is cached)            
            keys <- if(!dbExists(db, exprDigest)) {
                ## Evaluate the expression for the first time and dump
                ## the resulting objects to the database; return a
                ## character vector containing the names of the
                ## objects created on evaluation
                try({
                    evalAndDumpToDB(db, expr, exprDigest)
                }, silent = TRUE)
            }
            else {
                ## Retrieve character vector of keys (object names)
                ## from the database
                dbFetch(db, exprDigest)
            }
            ## If there was an error trying to evaluate the
            ## expression, then just return the error/condition object
            ## and let the Sweave driver deal with it.            
            if(inherits(keys, "try-error"))
                return(keys)

            ## Given the vector of keys, lazy-load them into the
            ## global environment and replace the actual objects with
            ## promises
            dbLazyLoad(db, globalenv(), keys)
        }
        else {
            ## If caching is turned off, just evaluate the expression
            ## in the global environment            
            res <- try(.Internal(eval.with.vis(expr, .GlobalEnv, baseenv())),
                       silent=TRUE)
            if(inherits(res, "try-error"))
                return(res)
            if(options$print | (options$term & res$visible))
                print(res$value)
        }
    }
    res
}

## Need to add the 'cache', 'filename' option to the list
cacheSweaveSetup <- function(file, syntax,
                             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
                             eval=TRUE, split=FALSE, stylepath=TRUE, pdf=TRUE,
                             eps=TRUE, cache = FALSE) {
    
    out <- utils:::RweaveLatexSetup(file, syntax, output=NULL, quiet=FALSE,
                                    debug=FALSE, echo=TRUE, eval=TRUE, split=FALSE,
                                    stylepath=TRUE, pdf=TRUE, eps=TRUE)

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
    sub("\\.Rnw$", "\\.map", Rnwfile)
}

## This function is essentially unchanged from the original Sweave
## version, except I compute the digest of the entire chunk, write out
## information to the map file, and use 'cacheSweaveEvalWithOpt'
## instead.  Note that everything in this function operates at the
## chunk level.

cacheSweaveRuncode <- function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))){
        return(object)
    }

    if(!object$quiet){
        cat(formatC(options$chunknr, width=2), ":")
        if(options$echo) cat(" echo")
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
        chunkout <- object$chunkout[[chunkprefix]]
        if(is.null(chunkout)){
            chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    SweaveHooks(options, run=TRUE)

    ## parse entire chunk block
    chunkexps <- try(parse(text=chunk), silent=TRUE)
    RweaveTryStop(chunkexps, options)

######################################################################
    ## Adding my own stuff here [RDP]
    
    chunkDigest <- digest(chunkexps)
    mapFile <- object[["mapFile"]]

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
    mapEntry <- data.frame(chunk = options$label,
                           chunkprefix = chunkprefix,
                           fig = figname,
                           cacheDB = dbName,
                           time = Sys.time())
    write.dcf(mapEntry, file = mapFile, append = TRUE, width = 2000)
    
    ## End adding my own stuff [RDP]
######################################################################
    
    openSinput <- FALSE
    openSchunk <- FALSE

    if(length(chunkexps)==0)
        return(object)

    ## Cycle over individual expressions
    for(nce in 1:length(chunkexps))
    {
        ce <- chunkexps[[nce]]
        dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
        if(object$debug)
            cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
        if(options$echo){
            if(!openSinput){
                if(!openSchunk){
                    cat("\\begin{Schunk}\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("\\begin{Sinput}",
                    file=chunkout, append=TRUE)
                openSinput <- TRUE
            }
            cat("\n", getOption("prompt"),
                paste(dce,
                      collapse=paste("\n", getOption("continue"), sep="")),
                file=chunkout, append=TRUE, sep="")
        }

        ## tmpcon <- textConnection("output", "w")
        ## avoid the limitations (and overhead) of output text connections
        tmpcon <- file()
        sink(file=tmpcon)
        err <- NULL
        if(options$eval) err <- cacheSweaveEvalWithOpt(ce, options, chunkDigest)
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
                openSinput <- FALSE
            }
            if(options$results=="verbatim"){
                if(!openSchunk){
                    cat("\\begin{Schunk}\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("\\begin{Soutput}\n",
                    file=chunkout, append=TRUE)
            }

            output <- paste(output,collapse="\n")
            if(options$strip.white %in% c("all", "true")){
                output <- sub("^[[:space:]]*\n", "", output)
                output <- sub("\n[[:space:]]*$", "", output)
                if(options$strip.white=="all")
                    output <- sub("\n[[:space:]]*\n", "\n", output)
            }
            cat(output, file=chunkout, append=TRUE)
            remove(output)

            if(options$results=="verbatim"){
                cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
            }
        }
    }

    if(openSinput){
        cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
    }

    if(openSchunk){
        cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
    }

    if(is.null(options$label) & options$split)
        close(chunkout)

    if(options$split & options$include)
        cat("\\input{", chunkprefix, "}\n", sep="",
            file=object$output, append=TRUE)

    if(options$fig && options$eval){
        if(options$eps){
            grDevices::postscript(file=paste(chunkprefix, "eps", sep="."),
                                  width=options$width, height=options$height,
                                  paper="special", horizontal=FALSE)

            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            grDevices::dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$pdf){
            grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
                           width=options$width, height=options$height,
                           version=options$pdf.version,
                           encoding=options$pdf.encoding)

            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            grDevices::dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$include)
            cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                file=object$output, append=TRUE)
    }
    return(object)
}

































######################################################################
## Old version that uses R workspaces instead of filehash databases

## cacheSweaveOld <- function(name, expr, envir = parent.frame()) {
##     if(!file.exists(name)) {
##         env <- new.env()
##         local(eval(expr), env)
##         save(list = ls(env, all.names = TRUE), file = name, compress = TRUE,
##              envir = env)
##         for(n in ls(env, all.names = TRUE))
##             assign(n, get(n, env), envir)
##     }
##     else
##         load(name, envir)
## }

## NOTE: This function uses 'DB1' format without asking.  

## cacheSweave <- function(expr, prefix = NULL, envir = parent.frame(), keys = NULL) {
##     expr <- substitute(expr)
##     cachedir <- getCacheDir()
## 
##     if(is.null(cachedir))
##         stop("need to set cache directory with 'setCacheDir'")
##     dbName <- file.path(cachedir, paste(prefix, digest(expr), sep = "_"))
## 
##     if(!file.exists(dbName)) {
##         env <- new.env(parent = globalenv())
##         eval(expr, env)
## 
##         ## Create/initialize caching database
##         dbCreate(dbName)
##         db <- dbInit(dbName)
## 
##         ## Only save objects specified by 'keys'
##         if(is.null(keys))
##             keys <- ls(env, all.names = TRUE)
##         dumpToDB(db, list = keys, envir = env)
##     }
##     db <- dbInit(dbName)
##     dbLazyLoad(db, envir, keys)
## }

