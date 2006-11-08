.onLoad <- function(lib, pkg) {
    pkgList <- c("stashR", "digest")
    
    for(pkg in pkgList) {
        if(!require(pkg, quietly = TRUE, character.only = TRUE))
            stop(gettextf("'%s' package required", pkg))
    }
    assign("cacheDir", ".", cacheEnv)
}

cacheEnv <- new.env(parent = emptyenv())
