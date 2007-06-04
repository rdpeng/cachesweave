showcache <- function(cachedir = NULL) {
        if(is.null(cachedir))
                cachedir <- getCacheDir()
        dirs <- dir(cachedir, full.names = TRUE)

        ## If 'cachedir' is not "." then we assume that everything in
        ## 'cachedir' is a cache database directory

        if(cachedir == ".") {
                use <- sapply(basename(dirs), isCacheDir)
                dirs <- dirs[use]
        }
        if(length(dirs) == 0)
                return(NULL)
        cacheInfo <- lapply(dirs, function(cdir) {
                exprFiles <- dir(cdir, full.names = TRUE)
                keys <- readKeys(exprFiles)
                list(dir = basename(cdir), keys = keys)
        })
        names(cacheInfo) <- chunkName(basename(dirs))
        cacheInfo
}

chunkName <- function(x) {
        sub("_\\S+$", "", x, perl = TRUE)
}

readKeys <- function(exprFiles) {
        keys <- lapply(exprFiles, function(f) {
                index <- getIndex(f)
                names(index)
        })
        unique(unlist(keys))
}


## Heuristic: [something]_[something] is a cache database directory

isCacheDir <- function(name) {
        v <- grep("\\S+_\\S+", name, perl = TRUE)
        length(v) > 0
}
