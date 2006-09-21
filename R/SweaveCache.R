setCacheDir <- function(path) {
    assign("cacheDir", path, cacheEnv)
    dir.create(path, showWarnings = FALSE)
}

getCacheDir <- function() {
    get("cacheDir", cacheEnv)
}

## NOTE: This function uses 'DB1' format without asking.  Eventually
## we will switch to the filehashRemote/Local stuff.

cacheSweave <- function(expr, prefix = NULL, envir = parent.frame(), keys = NULL) {
    expr <- substitute(expr)
    cachedir <- getCacheDir()

    if(is.null(cachedir))
        stop("need to set cache directory with 'setCacheDir'")
    dbName <- file.path(cachedir, paste(prefix, digest(expr), sep = "_"))

    if(!file.exists(dbName)) {
        env <- new.env(parent = globalenv())
        eval(expr, env)

        ## Create/initialize caching database
        dbCreate(dbName)
        db <- dbInit(dbName)

        ## Only save objects specified by 'keys'
        if(is.null(keys))
            keys <- ls(env, all.names = TRUE)
        dumpToDB(db, list = keys, envir = env)
    }
    db <- dbInit(dbName)
    dbLazyLoad(db, envir, keys)
}

dumpToDB <- function(db, list = character(0), envir = parent.frame()) {
    if(!is(db, "filehash"))
        stop("'db' should be a 'filehash' database")
    for(i in seq(along = list))
        dbInsert(db, list[i], get(list[i], envir))
    invisible(db)
}











######################################################################
######################################################################
## Taken/adapted from Sweave code by Friedrich Leisch

cacheSweaveDriver <- function() {
    list(
         setup = cacheSweaveSetup,
         runcode = cacheSweaveRuncode,
         writedoc = utils:::RweaveLatexWritedoc,
         finish = utils:::RweaveLatexFinish,
         checkopts = utils:::RweaveLatexOptions
         )
}

## Need to add the 'cache' option to the list
cacheSweaveSetup <- function(file, syntax,
                             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
                             eval=TRUE, split=FALSE, stylepath=TRUE, pdf=TRUE, eps=TRUE,
                             cache = TRUE) {
    
    out <- utils:::RweaveLatexSetup(file, syntax, output=NULL, quiet=FALSE,
                                    debug=FALSE, echo=TRUE, eval=TRUE, split=FALSE,
                                    stylepath=TRUE, pdf=TRUE, eps=TRUE)
    out$options[["cache"]] <- cache
    out
}
    

## This function is essentially unchanged, except I compute the digest
## of the entire chunk and also use 'cacheSweaveEvalWithOpt' instead.

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

    chunkHash <- digest(chunkexps)
    
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

        # tmpcon <- textConnection("output", "w")
        # avoid the limitations (and overhead) of output text connections
        tmpcon <- file()
        sink(file=tmpcon)
        err <- NULL
        if(options$eval) err <- cacheSweaveEvalWithOpt(ce, options, chunkHash)
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

evalAndDumpToDB <- function(db, expr) {
    env <- new.env(parent = globalenv())
    eval(expr, env)
    
    ## Get newly assigned object names
    keys <- ls(env, all.names = TRUE)

    ## Associate the newly created keys with the digest of
    ## the expression
    dbInsert(db, digest(expr), keys)
    
    ## Dump the values of the keys to the database
    dumpToDB(db, list = keys, envir = env)

    keys
}

cacheSweaveEvalWithOpt <- function (expr, options, blockhash){
    ## 'expr' is a single expression, so something like 'a <- 1'
    res <- NULL

    if(options$eval){
        if(options$cache) {
            cachedir <- getCacheDir()
            dbName <- file.path(cachedir, paste(options$label, blockhash, sep = "_"))
            digestExpr <- digest(expr)
            
            if(!file.exists(dbName)) 
                dbCreate(dbName)  ## Database doesn't exist yet, so create it
            db <- dbInit(dbName)

            ## Database exists; check to see if this expression
            ## has been evaluated already
            if(!dbExists(db, digestExpr)) {
                keys <- try({
                    evalAndDumpToDB(db, expr)
                }, silent = TRUE)
            }
            else 
                keys <- dbFetch(db, digestExpr)
            if(inherits(keys, "try-error"))
                return(keys)
            dbLazyLoad(db, globalenv(), keys)
        }
        else {
            res <- try(.Internal(eval.with.vis(expr, .GlobalEnv, baseenv())),
                       silent=TRUE)
            if(inherits(res, "try-error")) return(res)
            if(options$print | (options$term & res$visible))
                print(res$value)
        }
    }
    res
}
































######################################################################
## Old version that uses R workspaces instead of filehash databases

cacheSweaveOld <- function(name, expr, envir = parent.frame()) {
    if(!file.exists(name)) {
        env <- new.env()
        local(eval(expr), env)
        save(list = ls(env, all.names = TRUE), file = name, compress = TRUE,
             envir = env)
        for(n in ls(env, all.names = TRUE))
            assign(n, get(n, env), envir)
    }
    else
        load(name, envir)
}
