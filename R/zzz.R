.onLoad <- function(lib, pkg) {
    if(!require(digest))
        stop("'digest' package required")
    assign("cacheDir", NULL, cacheEnv)
}

cacheEnv <- new.env(parent = emptyenv())
