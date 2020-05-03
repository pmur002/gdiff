
## The core function
gdiffCore <- function(codeFun,
                      controlDir=getOption("gdiff.controlDir"),
                      testDir=getOption("gdiff.testDir"),
                      compareDir=getOption("gdiff.compareDir"),
                      clean=TRUE, compare=TRUE,
                      device=getOption("gdiff.device"),
                      session=currentSession(),
                      ncpu=1) {
    ## Argument checks
    if (length(compareDir) > 1) {
        stop("There can only be one compare directory")
    }
    ## gdiffCompare() allows multiple controlDir and/or testDir
    if (any(controlDir %in% testDir) ||
        any(testDir %in% controlDir) ||
        any(controlDir %in% compareDir) ||
        any(testDir %in% compareDir)) {
        stop("Control, test, and compare directories MUST be distinct")
    }
    if (inherits(codeFun, "gdiffCodeGenerator") || is.null(codeFun)) {
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
                   controlDir, device$control, clean$control, ncpu)
    ## Generate test output
    generateOutput(session$test, codeFun$test,
                   testDir, device$test, clean$test, ncpu)
    if (compare) {
        ## Generate comparisons
        createDir(compareDir, clean$compare)
        performComparison(controlDir, testDir, compareDir)
    } else {
        invisible()
    }
}

################################################################################
## Generate control and test output and compare
gdiff <- function(x, ...) {
    UseMethod("gdiff")
}

gdiff.function <- function(x, name=deparse(substitute(x)), ...) {
    f <- function() {
        code <- list(x)
        names(code) <- name
        code
    }
    codeFun <- gdiffCodeGenerator(f)
    gdiffCore(codeFun, ...)
}

gdiff.list <- function(x, name, ...) {
    checkList(x, class="function", allowNull=TRUE)
    codeFun <- lapply(x,
                      function(fun) {
                          if (is.null(fun)) {
                              fun
                          } else {
                              f <- function() {
                                  code <- list(fun)
                                  names(code) <- name
                                  code
                              }
                              gdiffCodeGenerator(f)
                          }
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
                ## Need echo=TRUE to print things like 'lattice' plots
                ## BUT do not want it spewed on screen
                capture.output(example(fun,
                                       character.only=TRUE,
                                       setRNG=TRUE,
                                       echo=FALSE,
                                       ask=FALSE))
            })
        names(code) <- name
        code
    }
    codeFun <- gdiffCodeGenerator(f)
    gdiffCore(codeFun, ...)
}

gdiffExamples.function <- function(fun, name=NULL, ...) {
    fun <- deparse(substitute(fun))
    gdiffExamples(fun, fun, ...)
}

gdiffPackage <- function(pkg, ...) {
    codeFun <- packageCode(pkg)
    gdiffCore(codeFun, ...)
}

################################################################################
## Just generate output
gdiffOutput <- function(x, dir, ...) {
    UseMethod("gdiffOutput")
}

gdiffOutput.function <- function(x, dir, name=deparse(substitute(x)), ...) {
    f <- function() {
        code <- list(x)
        names(code) <- name
        code
    }
    codeFun <- gdiffCodeGenerator(f)
    gdiffCore(list(control=codeFun, test=NULL), controlDir=dir,
              testDir="", compare=FALSE, ...)
    list.files(dir, full.names=TRUE)
}

gdiffExamplesOutput <- function(fun, dir, ...) {
    UseMethod("gdiffExamplesOutput")
}

gdiffExamplesOutput.character <- function(fun, dir, name=fun, ...) {
    f <- function() {
        code <- list(
            function() {
                ## Need echo=TRUE to print things like 'lattice' plots
                ## BUT do not want it spewed on screen
                capture.output(example(fun,
                                       character.only=TRUE,
                                       setRNG=TRUE,
                                       echo=TRUE,
                                       ask=FALSE))
            })
        names(code) <- name
        code
    }
    codeFun <- gdiffCodeGenerator(f)
    gdiffCore(list(control=codeFun, test=NULL), controlDir=dir,
              testDir="", compare=FALSE, ...)
    list.files(dir, full.names=TRUE)
}

gdiffExamplesOutput.function <- function(fun, dir, name=NULL, ...) {
    name <- deparse(substitute(fun))
    gdiffExamplesOutput(name, dir, ...)
}

gdiffPackageOutput <- function(pkg, dir, ...) {
    codeFun <- packageCode(pkg)
    gdiffCore(list(control=codeFun, test=NULL), controlDir=dir,
              testDir="", compare=FALSE,
              ...)
    list.files(dir, full.names=TRUE)
}

################################################################################
## Just compare
gdiffCompare <- function(controlDir="Control", testDir="Test",
                         compareDir="Compare", ...) {
    ## controlDir and testDir can be vectors
    gdiffCore(codeFun=NULL, controlDir, testDir, compareDir, ...)
}
