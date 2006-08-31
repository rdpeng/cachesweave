.onLoad <- function(lib, pkg) {
    if(!require(filehash, quietly = TRUE))
        stop("'filehash' package required")
    assign("cacheDir", NULL, cacheEnv)
}

cacheEnv <- new.env(parent = emptyenv())
