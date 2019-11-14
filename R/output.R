
## Functions for generating graphical output
generateOutput <- function(session, code, dir, device, clean, ncpu) {
    UseMethod("generateOutput")
}

## Useful function for generateOutput() methods
gdiffGenerateOutput <- function(codeFun, dir, device, clean, ncpu) {
    ## 'codeFun' can be NULL
    ## (in which case, do nothing)
    if (!is.null(codeFun)) {
        if (length(dir) > 1) {
            dir <- dir[1]
            warning("Multiple output directories specified: only using the first")
        }
        createDir(dir, clean)
        sessionFile <- file.path(dir, gdiffSessionFile)
        if (file.exists(sessionFile)) {
            stop("Directory already contains 'gdiff' output")
        }
        info <- sessionInfo()
        ## Use version 2 for maximum compatiblity across R sessions
        saveRDS(info, file=sessionFile, version=2)
        codeList <- codeFun()
        f <- function(fun, name) {
            ## Allow function to be generated as NULL
            ## (in which case, do nothing)
            if (!is.null(fun)) {
                for (d in device) {
                    filename <- file.path(dir, name) 
                    d$open(filename)
                    fun()
                    d$close(dir, name)
                }
            }
        }
        if (length(codeList)) {
            if (ncpu > 1) {
                cl <- parallel::makePSOCKcluster(ncpu)
                parallel::clusterMap(cl, f, codeList, names(codeList))
                parallel::stopCluster(cl)
            } else {
                invisible(mapply(f, codeList, names(codeList)))
            }
        }
    }
}


