
.onLoad <- function(libname, pkgname) {
    options("gdiff.controlDir"="Control",
            "gdiff.testDir"="Test",
            "gdiff.compareDir"="Compare",
            "gdiff.device"=pngDevice())
}

gdiffSessionFile <- ".gdiffSession"

