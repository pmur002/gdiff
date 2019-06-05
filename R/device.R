
device <- function(name, suffix=name, open,
                   close=function(dir, name) dev.off()) {
    d <- list(name=name, suffix=suffix, open=open, close=close)
    class(d) <- "gdiffDevice"
    d
}

pngDevice <- function(...) {
    device("png",
           open=function(name) {
               png(paste0(name, "-%03d.png"), ...)
           })
}

postscriptDevice <- function(...) {
    device("postscript",
           open=function(name) {
               postscript(paste0(name, "-%03d.ps"),
                          onefile=FALSE, ...)
           })
}

pdfDevice <- function(...) {
    device("pdf",
           open=function(name) {
               pdf(paste0(name, "-%03d.pdf"),
                   onefile=FALSE, ...)
           },
           ## Remove files with no pages
           close=function(dir, name) {
               dev.off()
               files <- list.files(dir,
                                   pattern=paste0(name, "-[0-9]+[.]pdf"),
                                   full.names=TRUE)
               for (i in files) {
                   if (pdf_info(i)$pages == 0) {
                       unlink(i)
                   }
               }
           })
}

cairo_pdf_device <- function(suffix=".cairo.pdf", ...) {
    device("cairo_pdf",
           open=function(name) {
               cairo_pdf(paste0(name, "-%03d", suffix),
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
