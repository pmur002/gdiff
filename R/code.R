
gdiffCodeGenerator <- function(fun, class=NULL, ...) {
    class(fun) <- c(class, "gdiffCodeGenerator")
    fun
}

packageCode <- function(pkg) {
    f <- function() {
        require(pkg, character.only=TRUE)
        fnames <- ls(paste0("package:", pkg))
        code <- lapply(fnames,
                       function(fname) {
                           function()
                               ## Need echo=TRUE to print things like
                               ## 'lattice' plots
                               ## BUT do not want it spewed on screen
                               ## Need to specify 'package' in case
                               ## this function is run in another session
                               ## (that does not have the package loaded)
                               capture.output(example(fname,
                                                      package=pkg,
                                                      character.only=TRUE,
                                                      setRNG=TRUE,
                                                      echo=TRUE,
                                                      ask=FALSE))
                       })
        names(code) <- fnames
        code
    }
    gdiffCodeGenerator(f)
}
        
