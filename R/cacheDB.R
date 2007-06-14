## Simple database implementation for cacheSweave

saveWithIndex <- function(list = character(0), file, envir = parent.frame()) {
        con <- file(file, "wb")
        on.exit(close(con))
        
        byteList <- lapply(list, function(symname) {
                x <- get(symname, envir, inherits = FALSE)

                if(is.environment(x))
                        warning(gettextf("saving of environments not supported"))
                list(key = symname,
                     bytes = serialize(x, connection = NULL))
        })
        writeIndex(byteList, con)
        writeData(byteList, con)
}

writeIndex <- function(byteList, con) {
        if(length(byteList) > 0) {
                lens <- sapply(byteList, function(x) length(x$bytes))
                index <- c(0, cumsum(lens)[-length(byteList)])
                names(index) <- sapply(byteList, "[[", "key")
                
        }
        else
                index <- integer(0)
        serialize(index, con)
}

writeData <- function(byteList, con) {
        for(entry in byteList) {
                writeBin(entry$bytes, con)
        }
}

isEmptyIndex <- function(idx) {
        isTRUE(length(idx) == 0)
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
        if(isEmptyIndex(index))
                return(character(0))
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

        if(is.null(keys))
                stop("problem with lazy-load database index")        
        for(i in seq_along(index)) {
                wrap(keys[i], index[i], envir)
        }
        invisible(keys)
}

getIndex <- function(file) {
        con <- file(file, "rb")
        on.exit(close(con))

        index <- unserialize(con)
        index
}
