
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
        checkList(code, class="function", allowNull=TRUE)
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
                   controlDir, name, 
                   device$control,
                   clean$control)
    ## Generate test output
    generateOutput(session$test, code$test,
                   testDir, name, 
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
    checkList(code, class="function", allowNull=TRUE)
    gdiffFunction(code, name, ...)
}

gdiffExamples <- function(fun, ...) {
    UseMethod("gdiffExamples")
}

gdiffExamples.character <- function(fun, name=fun, ...) {
    f <- function() {
        example(fun, character.only=TRUE, setRNG=TRUE, echo=FALSE, ask=FALSE)
    }
    gdiff(f, name=name, ...)
}

gdiffExamples.function <- function(fun, name=NULL, ...) {
    fun <- deparse(substitute(fun))
    if (is.null(name)) {
        name <- fun
    }
    f <- function() {
        example(fun, character.only=TRUE, setRNG=TRUE, echo=FALSE, ask=FALSE)
    }
    gdiff(f, name=name, ...)
}

gdiffPackage <- function(pkg, 
                         controlDir="Control",
                         testDir="Test",
                         compareDir="Compare",
                         clean=TRUE,
                         device=pngDevice(),
                         session=currentSession(),
                         ncpu=detectCores()) {
    ## Argument checks
    if (!is.character(pkg)) {
        stop("Invalid package name")
    }
    if (controlDir == testDir ||
        controlDir == compareDir ||
        testDir == compareDir) {
        stop("Control, test, and compare directories MUST be distinct")
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
    generatePkgOutput(session$control, pkg,
                      controlDir, 
                      device$control,
                      clean$control)
    ## Generate test output
    generatePkgOutput(session$test, pkg,
                      testDir, 
                      device$test,
                      clean$test)
    ## Generate comparisons
    createDir(compareDir, clean$compare)
    performComparison(controlDir, testDir, compareDir)
}
