
gdiffDevice <- function(name, suffix=name, open,
                        close=function(dir, name) dev.off()) {
    d <- list(name=name, suffix=suffix, open=open, close=close)
    class(d) <- "gdiffDevice"
    d
}

## Conversion for function names that do not translate to file names
safeName <- function(name) {
    gsub("%", "percent", name)
}

pngDevice <- function(...) {
    gdiffDevice("png",
                open=function(name) {
                    png(paste0(safeName(name), "-%03d.png"), ...)
                })
}

postscriptDevice <- function(...) {
    gdiffDevice("postscript",
                open=function(name) {
                    postscript(paste0(safeName(name), "-%03d.ps"),
                               onefile=FALSE, ...)
                })
}

pdfDevice <- function(...) {
    gdiffDevice("pdf",
                open=function(name) {
                    pdf(paste0(safeName(name), "-%03d.pdf"),
                        onefile=FALSE, ...)
                },
                ## Remove files with no pages
                close=function(dir, name) {
                    dev.off()
                    files <- list.files(dir,
                                        pattern=paste0(safeName(name),
                                                       "-[0-9]+[.]pdf"),
                                        full.names=TRUE)
                    for (i in files) {
                        if (pdf_info(i)$pages == 0) {
                            unlink(i)
                        }
                    }
                })
}

cairo_pdf_device <- function(suffix=".cairo.pdf", ...) {
    gdiffDevice("cairo_pdf",
                open=function(name) {
                    cairo_pdf(paste0(safeName(name), "-%03d", suffix),
                              onefile=FALSE, ...)
                })
}

checkDevice <- function(device) {
    ## 'device' can be single device, or list of devices,
    ## or list with 'control' and 'test' (which can be single device or list)
    if (inherits(device, "gdiffDevice")) {
        device <- list(control=list(device), test=list(device))
    } else {
        if (is.null(names(device))) {
            device <- list(control=device, test=device)
        } else {
            checkList(device)
            if (inherits(device$control, "gdiffDevice")) {
                device$control <- list(device$control)
            } 
            if (inherits(device$test, "gdiffDevice")) {
                device$test <- list(device$test)
            } 
        }
    }
    device
}
