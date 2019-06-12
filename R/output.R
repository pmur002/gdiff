
## Functions for generating graphical output
generateOutput <- function(session, code, dir, name, device, clean) {
    UseMethod("generateOutput")
}

## Useful function for generateOutput() methods
gdiffGenerateOutput <- function(codeGenerator, dir, device, clean) {
    createDir(dir, clean)
    codeList <- codeGenerator()
    f <- function(fun, name) {
        ## Allow function to be generated as NULL (in which case, do nothing)
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
        invisible(parallel::mcmapply(f, codeList, names(codeList)))
    }
}


