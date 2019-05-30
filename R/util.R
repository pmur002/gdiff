
createDir <- function(dir, clean=TRUE) {
    if (dir.exists(dir)) {
        if (clean) {
            unlink(dir, recursive=TRUE)
            dir.create(dir)
        }
    } else {
        dir.create(dir)
    }
}

underline <- function(char="-") {
    paste(rep(char, options("width")), collapse="")
}

## Does 'x' contain components named "control" and "test"
checkList <- function(x, compare=FALSE, class=NULL, allowNull=FALSE) {
    requiredNames <- c("control", "test")
    if (compare) {
        requiredNames <- c(requiredNames, "compare")
    }
    names <- names(x)
    if (is.null(names) ||
        !all(requiredNames %in% names))
        stop(paste0("List requires components named: ",
                    paste0(requiredNames, collapse=", ")))
    if (!is.null(class)) 
        if (!((inherits(x$control, class) ||
               (allowNull && is.null(x$control))) &&
              (inherits(x$test, class) ||
               (allowNull && is.null(x$test)))))
            stop(paste0("List components must have class '", class, "'"))
}

