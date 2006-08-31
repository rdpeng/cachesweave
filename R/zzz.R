.onLoad <- function(lib, pkg) {
    assign("cacheDir", NULL, cacheEnv)
}

cacheEnv <- new.env(parent = emptyenv())
