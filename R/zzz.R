
.onLoad <- function(libname, pkgname) {
    options("gdiff.controlDir"="Control",
            "gdiff.testDir"="Test",
            "gdiff.compareDir"="Compare")
}
