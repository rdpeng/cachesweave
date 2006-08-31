cacheSweaveDB <- function(prefix, expr, envir = parent.frame(), cachedir = "cache") {
    library(filehash)
    library(digest)
    expr <- substitute(expr)
    dbName <- file.path(cachedir, paste(prefix, digest(expr), sep = "-"))

    if(!file.exists(dbName)) {
        env <- new.env()
        eval(expr, env)

        dumpObjects(list = ls(env, all.names = TRUE), dbName = dbName, envir = env)
    }
    db <- dbInit(dbName)
    keys <- dbList(db)

    ## Need to clear the environment because 'dbLoad' will not
    ## overwrite existing keys
    suppressWarnings({
        rm(list = keys, pos = envir)
    })
    dbLoad(db, envir)
}

cacheSweave <- function(name, expr, envir = parent.frame()) {
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
