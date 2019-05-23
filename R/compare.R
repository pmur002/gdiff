
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
    controlBland <- gsub("-CONTROL", "", basename(controlFiles))
    testFiles <- list.files(testDir, full.names=TRUE)
    testBland <- gsub("-TEST", "", basename(testFiles))
    controlInTest <- controlBland %in% testBland
    testInControl <- testBland %in% controlBland
    compareFiles <- list(control=sort(controlFiles[controlInTest]),
                         test=sort(testFiles[testInControl]))
    diffFiles <- file.path(compareDir,
                           gsub("[.].+$", ".png",
                                gsub("-CONTROL", "-DIFF",
                                     basename(compareFiles$control))))
    diffs <- mapply(compare,
                    compareFiles$control, compareFiles$test, diffFiles,
                    SIMPLIFY=TRUE)
    result <- list(controlFiles=controlFiles,
                   testFiles=testFiles,
                   diffFiles=diffFiles,
                   diffs=diffs,
                   controlInTest=controlInTest,
                   testInControl=testInControl)
    class(result) <- "gcomparison"
    result
}

print.gcomparison <- function(x, ..., detail=TRUE) {
    result <- NULL
    same <- x$diffs == 0
    different <- !same
    compared <- sum(x$controlInTest)
    if (any(same)) {
        header <- paste0("Identical files [", sum(same), "/", compared, "]")
        if (detail) {
            result <- c(result, "", header, underline())
            result <-c(result,
                       paste0(x$controlFiles[same],
                              " matches ", x$testFiles[same]))
        } else {
            result <- c(result, header)
        }
    }
    if (any(different)) {
        header <- paste0("Files that differ [", sum(different),
                           "/", compared, "]")
        if (detail) {
            result <- c(result, "", header, underline())
            result <- c(result,
                        paste0(x$controlFiles[different],
                               " differs from ", x$testFiles[different],
                               " (", x$diffs[different], ")"))
        } else {
            result <- c(result, header)
        }
    }
    if (any(!x$controlInTest | !x$testInControl) ) {
        header <- paste0("Mismatched files [",
                         sum(!x$controlInTest, !x$testInControl),
                         "]")
        if (detail) {
            result <- c(result, "", header, underline)
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
