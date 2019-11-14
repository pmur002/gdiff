
compare <- function(controlFile, testFile, diffFile) {
    control <- magick::image_read(controlFile)
    test <-  magick::image_read(testFile)
    controlPNG <- magick::image_convert(control, "png")
    testPNG <- magick::image_convert(test, "png")
    diffPNG <- magick::image_compare(testPNG, controlPNG, metric="AE")
    magick::image_write(diffPNG, diffFile)
    attr(diffPNG, "distortion")
}

performComparison <- function(controlDir, testDir, compareDir) {
    controlFiles <- list.files(controlDir, full.names=TRUE)
    controlBland <- basename(controlFiles)
    if (anyDuplicated(controlBland)) {
        stop("Multiple control files with the same name")
    }
    testFiles <- list.files(testDir, full.names=TRUE)
    testBland <- basename(testFiles)
    if (anyDuplicated(testBland)) {
        stop("Multiple test files with the same name")
    }
    controlInTest <- controlBland %in% testBland
    testInControl <- testBland %in% controlBland
    compareFiles <- list(control=sort(controlFiles[controlInTest]),
                         test=sort(testFiles[testInControl]))
    diffFiles <- file.path(compareDir,
                           paste0(basename(compareFiles$control), ".png"))
    ## TODO
    ## parallelise this step
    if (length(compareFiles$control) &&
        length(compareFiles$test)) {
        diffs <- mapply(compare,
                        compareFiles$control, compareFiles$test, diffFiles,
                        SIMPLIFY=TRUE)
    } else {
        diffs <- numeric()
    }
    ## Load session info from Control and Test directories
    getInfo <- function(dir, files) {
        infoFile <- file.path(dir, gdiffSessionFile)
        numOutput <- sum(dirname(files) == dir)
        ## Directory can contain output that is NOT from 'gdiff'
        if (file.exists(infoFile)) {
            list(info=readRDS(infoFile), count=numOutput)
        } else {
            list(info=NULL, count=numOutput)
        }
    }
    controlInfo <- lapply(controlDir, getInfo, controlFiles)
    testInfo <- lapply(testDir, getInfo, testFiles)
    result <- list(controlFiles=controlFiles,
                   testFiles=testFiles,
                   diffFiles=diffFiles,
                   diffs=diffs,
                   controlInTest=controlInTest,
                   testInControl=testInControl,
                   controlInfo=controlInfo,
                   testInfo=testInfo)
    class(result) <- "gdiffComparison"
    result
}

print.gdiffComparison <- function(x, ..., detail=TRUE) {
    result <- NULL
    same <- x$diffs == 0
    different <- !same
    compared <- sum(x$controlInTest)
    if (any(same)) {
        header <- paste0("Identical files [", sum(same), "/", compared, "]")
        if (detail) {
            result <- c(result, "", header, underline())
            result <-c(result,
                       ## Use format() to line things up
                       paste0(format(x$controlFiles[x$controlInTest][same]),
                              " matches ",
                              x$testFiles[x$testInControl][same]))
        } else {
            result <- c(result, header)
        }
    }
    if (any(different)) {
        header <- paste0("Files that differ [", sum(different),
                           "/", compared, "]")
        if (detail) {
            result <- c(result, "", header, underline())
            result <-
                c(result,
                  paste0(format(x$controlFiles[x$controlInTest][different]),
                         " differs from ",
                         x$testFiles[x$testInControl][different],
                         " (", x$diffFiles[different], " [",
                         x$diffs[different], "])"))
        } else {
            result <- c(result, header)
        }
    }
    if (any(!x$controlInTest) || any(!x$testInControl)) {
        header <- paste0("Mismatched files [",
                         sum(!x$controlInTest, !x$testInControl),
                         "]")
        if (detail) {
            result <- c(result, "", header, underline())
        } else {
            result <- c(result, header)
        }
    }
    if (detail) {
        if (any(!x$controlInTest)) {
            result <- c(result,
                        paste0(x$controlFiles[!x$controlInTest],
                               " had no matching test output"))
        }
        if (any(!x$testInControl)) {
            result <- c(result,
                        paste0(x$testFiles[!x$testInControl],
                               " had no matching control output"))
        }
    }
    cat(result, "", sep="\n")
}
