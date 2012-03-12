.onLoad <- function(lib, pkg) {
        assign("cacheDir", ".", .cacheEnv)
}

.cacheEnv <- new.env(parent = emptyenv())
