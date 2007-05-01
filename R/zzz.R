.onLoad <- function(lib, pkg) {
    pkgList <- c("filehash", "stashR")
    
    for(pkg in pkgList) {
        if(!require(pkg, quietly = TRUE, character.only = TRUE))
            stop(gettextf("'%s' package required", pkg))
    }
    assign("cacheDir", ".", .cacheEnv)
}

.cacheEnv <- new.env(parent = emptyenv())
