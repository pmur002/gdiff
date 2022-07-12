
library(gdiff)

## Check samePDF() works

pdf1 <- tempfile(fileext=".pdf")
pdf2 <- tempfile(fileext=".pdf")
pdf3 <- tempfile(fileext=".pdf")
pdf4 <- tempfile(fileext=".pdf")
pdf5 <- tempfile(fileext=".pdf")

pdf(pdf1)
plot(1)
dev.off()

pdf(pdf2)
plot(2)
dev.off()

pdf(pdf3, compress=FALSE)
plot(1)
dev.off()

pdf(pdf4, compress=FALSE)
plot(2)
dev.off()

pdf(pdf5)
plot(1)
dev.off()

## Compare file against itself, should be the same
stopifnot(samePDF(pdf1, pdf1))
## Two different files should be different
stopifnot(!samePDF(pdf1, pdf2))
## Compare (uncompressed) file against itself, should be the same
stopifnot(samePDF(pdf3, pdf3))
## Two different (uncompressed) files should be different
stopifnot(!samePDF(pdf3, pdf4))
## Compare file against file with same content, but different creation date,
## should be the same
stopifnot(samePDF(pdf1, pdf5))

