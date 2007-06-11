.onLoad <- function(lib, pkg) {
        assign("cacheDir", "cache", .cacheEnv)
}

.onAttach <- function(lib, pkg) {
        if(!require(utils))
                stop("'utils' package required to use 'Sweave'")
}

.cacheEnv <- new.env(parent = emptyenv())
