
## Functions for generating graphical output

generateOutput <- function(session, code, dir, name, suffix, device, clean) {
    UseMethod("generateOutput")
}

## Useful function for generateOutput() methods
gdiffOutput <- function(code, dir, name, suffix, device, clean) {
    createDir(dir, clean)
    if (!is.null(code)) {
        for (d in device) {
            d$open(file.path(dir, paste0(name, suffix)))
            code()
            d$close()
        }
    }
}

