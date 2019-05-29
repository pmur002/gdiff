
device <- function(name, suffix=name, open, close=dev.off) {
    d <- list(name=name, suffix=suffix, open=open, close=close)
    class(d) <- "gdiff.device"
    d
}

pngDevice <- function(...) {
    device("png",
           open=function(name) {
               png(paste0(name, "-%03d.png"), ...)
           })
}

pdfDevice <- function(...) {
    device("pdf",
           open=function(name) {
               pdf(paste0(name, "-%03d.pdf"),
                   onefile=FALSE, ...)
           })
}

cairo_pdf_device <- function(...) {
    device("cairo_pdf",
           "pdf",
           open=function(name) {
               cairo_pdf(paste0(name, "-%03d.cairo.pdf"),
                         onefile=FALSE, ...)
           })
}

checkDevice <- function(device) {
    ## 'device' can be single device, or list of devices,
    ## or list with 'control' and 'test' (which can be single device or list)
    if (inherits(device, "gdiff.device")) {
        device <- list(control=list(device), test=list(device))
    } else {
        if (is.null(names(device))) {
            device <- list(control=device, test=device)
        } else {
            checkList(device)
            if (inherits(device$control, "gdiff.device")) {
                device$control <- list(device$control)
            } 
            if (inherits(device$test, "gdiff.device")) {
                device$test <- list(device$test)
            } 
        }
    }
    device
}
