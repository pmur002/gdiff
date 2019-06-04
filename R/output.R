
## Functions for generating graphical output

generateOutput <- function(session, code, dir, name, device, clean) {
    UseMethod("generateOutput")
}

## Useful function for generateOutput() methods
gdiffOutput <- function(code, dir, name, device, clean) {
    createDir(dir, clean)
    if (!is.null(code)) {
        for (d in device) {
            d$open(file.path(dir, name))
            code()
            d$close()
        }
    }
}

