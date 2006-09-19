.onLoad <- function(lib, pkg) {
    if(!require(digest, quietly = TRUE))
        stop("'digest' package required")
    assign("cacheDir", ".", cacheEnv)
}

cacheEnv <- new.env(parent = emptyenv())
