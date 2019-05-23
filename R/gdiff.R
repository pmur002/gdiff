
## The core function
gdiff <- function(code, ...) {
    UseMethod("gdiff")
}

gdiffFunction <- function(code,
                          name,
                          controlDir="Control",
                          testDir="Test",
                          compareDir="Compare",
                          clean=TRUE,
                          device=pngDevice(),
                          Rcmd=NULL,
                          libPaths=NULL,
                          host=NULL) {
    ## Argument checks
    if (controlDir == testDir ||
        controlDir == compareDir ||
        testDir == compareDir) {
        stop("Control, test, and compare directories MUST be distinct")
    }
    if (is.list(code)) {
        checkList(code)
    } else {
        code <- list(control=code, test=code)
    }
    if (is.list(clean)) {
        checkList(clean, compare=TRUE)
    } else {
        clean <- list(control=clean, test=clean, compare=clean)
    }
    device <- checkDevice(device)
    ## Generate control output
    createDir(controlDir, clean$control)
    for (d in device$control) {
        d$open(file.path(controlDir, paste0(name, "-CONTROL")))
        code$control()
        d$close()
    }
    ## Generate test output
    createDir(testDir, clean$test)
    for (d in device$test) {
        d$open(file.path(testDir, paste0(name, "-TEST")))
        code$test()
        d$close()
    }
    ## Generate comparisons
    createDir(compareDir, clean$compare)
    performComparison(controlDir, testDir, compareDir)
}

gdiff.function <- function(code, name=deparse(substitute(code)), ...) {
    gdiffFunction(code, name, ...)
}

gdiff.list <- function(code, name, ...) {
    checkList(code)
    gdiffFunction(code, name, ...)
}
