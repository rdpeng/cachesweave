getExpressions <- function(file) {
        fullpath <- normalizePath(file)
        tmpdir <- tempdir()
        wd <- getwd()
        on.exit(setwd(wd))

        setwd(tmpdir)
        output <- "StangleTmp.R"
        Stangle(fullpath, output = output, split = FALSE, quiet = TRUE)
        parse(output, srcfile = NULL)
}

findExprGlobals <- function(expr) {
        f <- function() {}
        body(f) <- expr
        findGlobals(f)
}

findExprLocals <- function(expr) {
        globals <- findExprGlobals(expr)
        isLocal <- sapply(globals, function(sym) {
                pos <- find(sym)
                length(pos) == 0
        })
        globals[isLocal]
}


matchList <- function(x, listTab) {
        m <- sapply(listTab, function(tab) match(x, tab))
        if(all(is.na(m)))
                NA
        else
                which(!is.na(m))[1]
}

buildDepTree <- function(i, exprList, edep, eobj) {
        if(is.na(i) || i < 2)
                return(list(origin = NA, origin.idx = NA, depends = NA))
        exprList <- exprList[1:i]
        edep <- edep[1:i]
        eobj <- eobj[1:i]

        if(length(edep[[i]]) == 0)  ## no dependencies
                return(list(origin = NA, origin.idx = NA, depends = NA))
        created <- lapply(edep[[i]], function(x) {
                as.integer( i - matchList(x, eobj[(i-1):1]) )
        })
        names(created) <- edep[[i]]
        lapply(created, function(j) {
                if(is.na(j)) 
                        list(origin = NA,
                             origin.idx = NA,
                             depends = NA)
                else
                        list(origin = exprList[j],
                             origin.idx = j,
                             depends = buildDepTree(j, exprList, edep, eobj))
        })
}

runTree <- function(exprList) {
        with(exprList, {
                lapply(seq_along(expr), function(i) {
                        buildDepTree(i, expr, dep, obj)
                })
        })
}

runFile <- function(file) {
        message("getting expressions")
        e <- getExpressions(file)
        message("finding expression locals")
        edep <- lapply(e, findExprLocals)
        message("getting expression objects")
        eobj <- lapply(e, getExprObjects)
        list(expr = e, dep = edep, obj = eobj)
}

################################################################################

## In order to figure out what objects an expression gives rise to, we
## have to evaluate it.

getExprObjects <- function(expr) {
        env <- evalAndCache(expr, exprFile = NULL, cache = FALSE)
        keys <- ls(env, all.names = TRUE)
        copy2env(keys, env, globalenv())
        keys
}
