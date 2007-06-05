cacheEval <- function(Rnwfile, cache = TRUE) {
        message("reading expressions")
        exprList <- getExpressions(Rnwfile)

        message("getting expression objects")
        exprObj <- lapply(exprList, getExprObjects)

        message("finding expression dependencies")
        exprDep <- lapply(exprList, findExprLocals)

        message("building expression dependency trees")
        exprTreeList <- lapply(seq_along(exprList), function(i) {
                buildDepTree(i, exprList, exprDep, exprObj)
        })
        exprTreeList
}

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
        searchList <- search()[-1]  ## exclude global env
        isLocal <- sapply(globals, function(sym) {
                pos <- find(sym)  ## could be length > 1
                inSearchList <- all(pos %in% searchList)
                length(pos) == 0 || !inSearchList
        })
        globals[isLocal]
}

## In order to figure out what objects an expression gives rise to, we
## have to evaluate it.

getExprObjects <- function(expr) {
        env <- evalAndCache(expr, NULL, FALSE)
        keys <- ls(env, all.names = TRUE)
        copy2env(keys, env, globalenv())
        keys
}

matchList <- function(x, listTab) {
        m <- sapply(listTab, function(tab) match(x, tab))
        if(all(is.na(m)))
                NA
        else
                which(!is.na(m))[1]
}

buildDepTree <- function(i, exprList, edep, eobj) {
        leaf <- list(origin = as.integer(NA),
                     origin.idx = as.integer(NA),
                     depends = as.integer(NA))
        if(is.na(i) || i < 2)
                return(leaf)
        exprList <- exprList[1:i]
        edep <- edep[1:i]
        eobj <- eobj[1:i]

        if(length(edep[[i]]) == 0)  ## no dependencies
                return(leaf)
        created <- lapply(edep[[i]], function(x) {
                ## Find the index of the first expression (going
                ## backwards) where object 'x' is created                
                as.integer( i - matchList(x, eobj[(i-1):1]) )
        })
        names(created) <- edep[[i]]
        lapply(created, function(j) {
                if(is.na(j)) 
                        leaf
                else
                        list(origin = exprList[j],
                             origin.idx = j,
                             depends = buildDepTree(j, exprList, edep, eobj))
        })
}

################################################################################

runTree <- function(exprList) {
        with(exprList, {
                lapply(seq_along(expr), function(i) {
                        buildDepTree(i, expr, dep, obj)
                })
        })
}

showcode <- function(exprTree, exprList) {
        s <- codeseq(exprTree)
        if(length(s) == 0)
                return(NULL)
        as.list(exprList$expr[rev(s)])
}

codeseq <- function(exprTree) {
        r <- rapply(exprTree, function(x) x, classes = "integer")
        s <- unique(unlist(r))
        sort(s, decreasing = TRUE)
}

################################################################################

