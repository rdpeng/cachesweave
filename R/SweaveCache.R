setCacheDir <- function(path) {
    assign("cacheDir", path, cacheEnv)
    dir.create(path, showWarnings = FALSE)
}

getCacheDir <- function() {
    get("cacheDir", cacheEnv)
}

## NOTE: This function uses 'DB1' format without asking.  Eventually
## we will switch to the filehashRemote/Local stuff.

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
        db <- dbInit(dbName, "DB1")

        ## Only save objects specified by 'keys'
        if(is.null(keys))
            keys <- ls(env, all.names = TRUE)
        dumpToDB(db, list = keys, envir = env)
    }
    db <- dbInit(dbName, "DB1")
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
