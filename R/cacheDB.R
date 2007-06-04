## Simple database implementation for cacheSweave

saveWithIndex <- function(list = character(0), file, envir = parent.frame()) {
        con <- file(file, "wb")
        on.exit(close(con))
        
        byteList <- lapply(list, function(symname) {
                x <- get(symname, envir, inherits = FALSE)
                b <- serialize(x, connection = NULL)
                list(key = symname, bytes = b)
        })
        writeIndex(byteList, con)
        writeData(byteList, con)
}

writeIndex <- function(byteList, con) {
        lens <- sapply(byteList, function(x) length(x$bytes))
        index <- c(0, cumsum(lens)[-length(byteList)])
        names(index) <- sapply(byteList, "[[", "key")

        serialize(index, con)
}

writeData <- function(byteList, con) {
        for(entry in byteList) {
                writeBin(entry$bytes, con)
        }
}

lazyLoad <- function(file, envir = parent.frame()) {
        dbcon <- file(file, "rb")
        tryCatch({
                index <- unserialize(dbcon)
                offset <- seek(dbcon)
        }, finally = {
                if(isOpen(dbcon))
                        close(dbcon)
        })
        wrap <- function(x, pos, env) {
                force(x)
                force(pos)
                delayedAssign(x, {
                        con <- file(file, "rb")
                        tryCatch({
                                seek(con, pos + offset)
                                unserialize(con)
                        }, finally = {
                                close(con)
                        })
                }, eval.env = environment(), assign.env = env)
        }
        keys <- names(index)
        
        for(i in seq_along(index)) {
                wrap(keys[i], index[i], envir)
        }
        invisible(keys)
}
