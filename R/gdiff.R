
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
                          session=currentSession()) {
    ## Argument checks
    if (controlDir == testDir ||
        controlDir == compareDir ||
        testDir == compareDir) {
        stop("Control, test, and compare directories MUST be distinct")
    }
    if (is.list(code)) {
        checkList(code, class="function")
    } else {
        code <- list(control=code, test=code)
    }
    if (is.list(clean)) {
        checkList(clean, compare=TRUE, class="logical")
    } else {
        clean <- list(control=clean, test=clean, compare=clean)
    }
    device <- checkDevice(device)
    if (inherits(session, "gdiff.session")) {
        session <- list(control=session, test=session)
    } else {
        checkList(session, class="gdiff.session")
    }
    ## Generate control output
    generateOutput(session$control, code$control,
                   controlDir, name, "-CONTROL",
                   device$control,
                   clean$control)
    ## Generate test output
    generateOutput(session$test, code$test,
                   testDir, name, "-TEST",
                   device$test,
                   clean$test)
    ## Generate comparisons
    createDir(compareDir, clean$compare)
    performComparison(controlDir, testDir, compareDir)
}

gdiff.function <- function(code, name=deparse(substitute(code)), ...) {
    gdiffFunction(code, name, ...)
}

gdiff.list <- function(code, name, ...) {
    checkList(code, class="function")
    gdiffFunction(code, name, ...)
}
