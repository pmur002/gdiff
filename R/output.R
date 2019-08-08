
## Functions for generating graphical output
generateOutput <- function(session, code, dir, name, device, clean, ncpu) {
    UseMethod("generateOutput")
}

## Useful function for generateOutput() methods
gdiffGenerateOutput <- function(codeFun, dir, device, clean, ncpu) {
    ## 'codeFun' can be NULL
    ## (in which case, do nothing)
    if (!is.null(codeFun)) {
        createDir(dir, clean)
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


