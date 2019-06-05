
codeGenerator <- function(fun, class=NULL, ...) {
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
                               example(fname,
                                       character.only=TRUE,
                                       setRNG=TRUE, echo=FALSE, ask=FALSE)
                       })
        names(code) <- fnames
        code
    }
    codeGenerator(f)
}
        
