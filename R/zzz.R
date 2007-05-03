.onLoad <- function(lib, pkg) {
        pkgList <- c("filehash", "stashR")
        
        for(pkg in pkgList) {
                if(!require(pkg, quietly = TRUE, character.only = TRUE))
                        stop(gettextf("'%s' package required", pkg))
        }
        assign("cacheDir", ".", .cacheEnv)
}

.onAttach <- function(lib, pkg) {
        if(!require(utils))
                stop("'utils' package required to use 'Sweave'")
}

.cacheEnv <- new.env(parent = emptyenv())
