
generatePNG <- function(infile, type) {
    pdftoppm <- getOption("gdiff.pdftoppm")
    pdfFile <- grepl("[.]pdf$", infile)
    img <- magick::image_read(infile)
    if (length(img)) {
        png <- magick::image_convert(img, "png")
        if (length(png)) {
            return(png)
        } else {
            return(paste0("convert", type))
        }
    } else if (pdfFile) {
        ## Try 'pdftoppm'
        if (nchar(pdftoppm)) {
            outfile <- tempfile(fileext="png")
            result <- system2("pdftoppm",
                              c("-png", infile, ">", outfile))
            if (result) { ## error
                return(paste0("convert", type))
            } else {
                png <- magick::image_read(outfile)
                if (length(png)) {
                    return(png)
                } else {
                    return(paste0("read", type))
                }
            }
        } else {
            return(paste0("read", type))
        }
    }
    "generatePNG"
}

compare <- function(controlFile, testFile, diffFile) {
    pdfFile <- grepl("[.]pdf$", controlFile)
    ## Shortcut: Check PDF files for "identical" PDF code 
    if (pdfFile && samePDF(controlFile, testFile))
        return(0)
    controlPNG <- generatePNG(controlFile, "Control")
    if (is.character(controlPNG)) ## error
        return(controlPNG)
    testPNG <- generatePNG(testFile, "Test")
    if (is.character(testPNG)) ## error
        return(testPNG)
    diffPNG <- magick::image_compare(testPNG, controlPNG, metric="AE")
    magick::image_write(diffPNG, diffFile)
    distortion <- attr(diffPNG, "distortion")
    if (!length(distortion))
        return("compare")
    ## Return size of difference 
    distortion
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
        results <- mapply(compare,
                          compareFiles$control, compareFiles$test, diffFiles,
                          SIMPLIFY=FALSE)
        diffs <- sapply(results, function(x) if (is.character(x)) NA else x)
        errors <- sapply(results,
                         function(x) {
                             if (is.character(x))
                                 paste(x, collapse=", ")
                             else
                                 NA
                         })
    } else {
        diffs <- numeric()
        errors <- character()
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
                   errors=errors,
                   controlInTest=controlInTest,
                   testInControl=testInControl,
                   controlInfo=controlInfo,
                   testInfo=testInfo)
    class(result) <- "gdiffComparison"
    result
}

diffFiles <- function(x) {
    same <- x$diffs == 0
    different <- !same
    x$diffFiles[different]
}

## Little hidden utility to ease my use on Linux
## Call as gdiff:::eogDiffs(result)
eogDiffs <- function(x) {
    system(paste("eog", paste(diffFiles(x), collapse=" ")))
}

print.gdiffComparison <- function(x, ..., detail=TRUE) {
    result <- NULL
    broken <- is.na(x$diffs)
    same <- !broken & x$diffs == 0
    different <- !broken & !same
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
    if (any(broken)) {
        header <- paste0("Comparisons that are broken [", sum(broken),
                         "/", compared, "]")
        if (detail) {
           result <- c(result, "", header, underline())
           brokenRead <- broken & grepl("readControl", x$errors)
           if (any(brokenRead))
               result <-
                   c(result,
                     paste0(format(x$controlFiles[x$controlInTest][brokenRead]),
                            " could not be read"))
           brokenRead <- broken & grepl("readTest", x$errors)
           if (any(brokenRead))
               result <-
                   c(result,
                     paste0(format(x$testFiles[x$testInControl][brokenRead]),
                            " could not be read"))
           brokenConvert <- broken & grepl("convertControl", x$errors)
           if (any(brokenConvert))
               result <-
                   c(result,
                     paste0(format(x$controlFiles[x$controlInTest][brokenConvert]),
                            " could not be converted"))
           brokenConvert <- broken & grepl("convertTest", x$errors)
           if (any(brokenConvert))
               result <-
                   c(result,
                     paste0(format(x$testFiles[x$testInControl][brokenConvert]),
                            " could not be converted"))
           brokenCompare <- broken & grepl("compare", x$errors)
           if (any(brokenCompare))
               result <-
                   c(result,
                     paste0(format(x$controlFiles[x$controlInTest][brokenCompare]),
                            " could not be compared with ",
                            format(x$testFiles[x$testInControl][brokenConvert])))
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
