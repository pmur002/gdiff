
## Functions for generating graphical output
generateOutput <- function(session, code, dir, name, device, clean) {
    UseMethod("generateOutput")
}

generatePkgOutput <- function(session, pkg, dir, device, clean) {
    UseMethod("generatePkgOutput")
}

## Useful function for generateOutput() methods
gdiffOutput <- function(code, dir, name, device, clean) {
    createDir(dir, clean)
    if (!is.null(code)) {
        filename <- file.path(dir, name) 
        for (d in device) {
            d$open(filename)
            code()
            d$close(dir, name)
        }
    }
}

gdiffPkgOutput <- function(pkg, dir, device, clean) {
    createDir(dir, clean)
    require(pkg, character.only=TRUE)
    fnames <- ls(paste0("package:", pkg))
    f <- function(fun) {
        for (d in device) {
            filename <- file.path(dir, fun) 
            d$open(filename)
            example(fun,
                    character.only=TRUE, setRNG=TRUE, echo=FALSE, ask=FALSE)
            d$close(dir, fun)
        }
    }
    if (length(fnames)) {
        invisible(parallel::mclapply(fnames, f))
    } else {
        warning(paste0("No functions found in package ", pkg))
    }
}

