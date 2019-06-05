
## The core function
gdiff <- function(x, ...) {
    UseMethod("gdiff")
}

gdiffCore <- function(codeFun,
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
    if (inherits(codeFun, "gdiffCodeGenerator")) {
        codeFun <- list(control=codeFun, test=codeFun)
    } else {
        checkList(codeFun, class="gdiffCodeGenerator", allowNull=TRUE)
    }
    if (is.logical(clean)) {
        clean <- list(control=clean, test=clean, compare=clean)
    } else {
        checkList(clean, compare=TRUE, class="logical")
    }
    device <- checkDevice(device)
    if (inherits(session, "gdiffSession")) {
        session <- list(control=session, test=session)
    } else {
        checkList(session, class="gdiffSession")
    }
    ## Generate control output
    generateOutput(session$control, codeFun$control,
                   controlDir, device$control, clean$control)
    ## Generate test output
    generateOutput(session$test, codeFun$test,
                   testDir, device$test, clean$test)
    ## Generate comparisons
    createDir(compareDir, clean$compare)
    performComparison(controlDir, testDir, compareDir)
}

gdiff.function <- function(x, name=deparse(substitute(x)), ...) {
    f <- function() {
        code <- list(x)
        names(code) <- name
        code
    }
    codeFun <- codeGenerator(f)
    gdiffCore(codeFun, ...)
}

gdiff.list <- function(x, name, ...) {
    checkList(x, class="function", allowNull=TRUE)
    codeFun <- lapply(x,
                      function(fun) {
                          f <- function() {
                              code <- list(fun)
                              names(code) <- name
                              code
                          }
                          codeGenerator(f)
                      })
    gdiffCore(codeFun, ...)
}

gdiffExamples <- function(fun, ...) {
    UseMethod("gdiffExamples")
}

gdiffExamples.character <- function(fun, name=fun, ...) {
    f <- function() {
        code <- list(
            function() {
                example(fun,
                        character.only=TRUE, setRNG=TRUE, echo=FALSE, ask=FALSE)
            })
        names(code) <- name
        code
    }
    codeFun <- codeGenerator(f)
    gdiffCore(codeFun, ...)
}

gdiffExamples.function <- function(fun, name=NULL, ...) {
    fun <- deparse(substitute(fun))
    gdiffExamples(fun)
}

gdiffPackage <- function(pkg, ..., 
                         ncpu=detectCores()) {
    codeFun <- packageCode(pkg)
    gdiffCore(codeFun, ...)
}
