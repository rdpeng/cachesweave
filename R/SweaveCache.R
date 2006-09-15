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

        ## Create/initialize caching database
        dbCreate(dbName, "DB1")
        db <- dbInit(dbName)
        
        dumpToDB(db, list = ls(env, all.names = TRUE), envir = env)

        if(is.null(keys))
            keys <- ls(env, all.names = TRUE)
        for(key in keys)
            assign(key, get(key, env), envir)
    }
    else {
        db <- dbInit(dbName)
        
        if(is.null(keys))
            keys <- dbList(db)
        for(key in keys) 
            assign(key, dbFetch(db, key), envir)
    }
}

dumpToDB <- function(db, list = character(0), envir = parent.frame()) {
    if(!is(db, "filehash"))
        stop("'db' should be a 'filehash' database")
    for(i in seq(along = list))
        dbInsert(db, list[i], get(list[i], envir))
    invisible(db)
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
