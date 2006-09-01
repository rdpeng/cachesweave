setCacheDir <- function(path) {
    assign("cacheDir", path, cacheEnv)
    dir.create(path, showWarnings = FALSE)
}

getCacheDir <- function() {
    get("cacheDir", cacheEnv)
}

cacheSweave <- function(prefix, expr, envir = parent.frame(), keys = NULL) {
    expr <- substitute(expr)
    cachedir <- getCacheDir()

    if(is.null(cachedir))
        stop("need to set cache directory with 'setCacheDir'")
    dbName <- file.path(cachedir, paste(prefix, digest(expr), sep = "-"))

    if(!file.exists(dbName)) {
        env <- new.env()
        eval(expr, env)

        dumpObjects(list = ls(env, all.names = TRUE), dbName = dbName,
                    envir = env)
    }
    db <- dbInit(dbName)

    if(is.null(keys))
        keys <- dbList(db)

    ## Need to clear the environment because 'dbLoad' will not
    ## overwrite existing keys
    suppressWarnings({
        rm(list = keys, pos = envir)
    })
    dbLoad(db, envir, keys = keys)
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
