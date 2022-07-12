
library(gdiff)

options(gdiff.controlDir=file.path(tempdir(), "Control"),
        gdiff.testDir=file.path(tempdir(), "Test"),
        gdiff.compareDir=file.path(tempdir(), "Compare"))
        
library(gridBezier)
f1 <- function() {
    grid.bezier(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0))
}
f2 <- function() {
    grid.Bezier(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0))
}

## compare function against itself
gdiff(f1)

## generate PDF output
gdiff(f1, device=pdfDevice())

## generate PNG and PDF output
gdiff(f1, device=list(pngDevice(), pdfDevice()))

## compare different functions
## (provide 'name' so file names match)
gdiff(list(control=f1, test=f2), name="f")

## compare output between different devices
## (provide 'suffix' for cairo_pdf device so file names match)
gdiff(f1, device=list(control=pdfDevice(), test=cairo_pdf_device(suffix=".pdf")))

## compare PDF with Cairo PDF with transparent background
gdiff(f1,
      device=list(control=pdfDevice(),
                  test=cairo_pdf_device(suffix=".pdf", bg="transparent")))

## compare output between different devices, some match, some don't
gdiff(f1, device=list(control=list(pngDevice(), pdfDevice()),
                      test=list(pngDevice(), cairo_pdf_device(suffix=".pdf"))))

## same compare, less detail shown
result <- gdiff(f1, device=list(control=list(pngDevice(), pdfDevice()),
                                test=list(pngDevice(),
                                          cairo_pdf_device(suffix=".pdf"))))
print(result, detail=FALSE)

## Can no longer run these tests because ghostscript > 9.5 has -dSAFER
## by default so these versions of 'grImport', which do not add -DNOSAFER,
## fail
notrun <- function() {
    ## Different libPaths (different package versions)
    oldPkgDir <- file.path(tempdir(), "oldPackages")
    dir.create(oldPkgDir)
    newPkgDir <- file.path(tempdir(), "newPackages")
    dir.create(newPkgDir)
    ## To avoid "cannot open file 'startup.Rs'" error
    ## https://github.com/r-lib/testthat/issues/144
    Sys.setenv("R_TESTS" = "")
    install.packages("grImport_0.9-1.tar.gz", repos=NULL, lib=oldPkgDir)
    install.packages("grImport_0.9-2.tar.gz", repos=NULL, lib=newPkgDir)
    f <- function() {
        ## grImport_0.9-2 added ability to import 'skewed' pictures
        library(grImport)
        PostScriptTrace("transform.ps", "transform.xml")
        img <- readPicture("transform.xml")
        grid.picture(img)
        detach("package:grImport")
    }
    gdiff(f, session=list(control=currentSession(libPaths=oldPkgDir),
                          test=currentSession(libPaths=newPkgDir)))

    ## Run normal R, but in different session (so no need to detach packages)
    f <- function() {
        ## grImport_0.9-2 added ability to import 'skewed' pictures
        library(grImport)
        PostScriptTrace("transform.ps", "transform.xml")
        img <- readPicture("transform.xml")
        grid.picture(img)
    }
    gdiff(f, session=list(control=localSession(libPaths=oldPkgDir),
                          test=localSession(libPaths=newPkgDir)))
}

## Pre-prepared output (do not clean controlDir)
## metaplot output vs 'metaplot' output
library(metapost)
options(metapost.units="in")
scurve <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1)
metapost(scurve, "scurve.mp")
mpost("scurve.mp", template="%j.ps")
MetaPostDir <- file.path(tempdir(), "MetaPost")
dir.create(MetaPostDir)
system(paste0("gs -o ", file.path(MetaPostDir, "scurve-001.pdf"),
              " -sDEVICE=pdfwrite -g720x720 -dPDFFitPage scurve.ps "))
library(grid)
f <- function() {
    grid.metapost(scurve)
}
gdiff(list(control=NULL, test=f),
      name="scurve",
      controlDir=MetaPostDir,
      device=pdfDevice(width=1, height=1),
      clean=list(control=FALSE, test=TRUE, compare=TRUE))

## Different Rpath (different R versions)
## Requires 'gdiff' installed on each R version
f <- function() {
    ## R 3.6.0 introduced 'gap.axis' argument to axis(), with example
    example(axis, setRNG=TRUE, echo=FALSE, ask=FALSE)
}
## Allow for different R versions to correspond
rPath <- Sys.getenv("GDIFF_RPATH")
if (nchar(rPath)) {
    Sys.setenv("R_DEFAULT_SAVE_VERSION"=2,
               "R_DEFAULT_SERIALIZE_VERSION"=2)
    gdiff(f,
          session=list(control=currentSession(),
                       test=localSession(Rpath=rPath)))
}

f <- function() plot(1)
## Different host (different platform/OS)
## Hardware/VM version (other office machine)
## (NOTE that even this very simple example still requires install of
##  'gdiff' package on remote machine)
## (With ssh keys set up this requires no interaction)
remoteHost <- Sys.getenv("GDIFF_REMOTE_HOST")
remoteUser <- Sys.getenv("GDIFF_REMOTE_USER")
if (nchar(remoteHost) && nchar(remoteUser)) {
    gdiff(f,
          session=list(control=localSession(),
                       test=remoteSession(remoteHost, user=remoteUser)))
} else {
    warning("remoteSession() tests NOT run
(remote host and/or user not defined)")
}

## Different host (different platform/OS)
## Hardware/VM version (other office machine)
## Use existing cluster; make cluster, possibly provision, then call gdiff()
## (NOTE that remoteSession() must be given user name as well
##  [because cluster object does not retain that information])
## (With ssh keys set up this requires no interaction)
if (nchar(remoteHost) && nchar(remoteUser)) {
    library(parallel)
    cl <- makeCluster(remoteHost, user=remoteUser)
    gdiff(f,
          session=list(control=localSession(),
                       test=remoteSession(cl, user=remoteUser)))
    stopCluster(cl)
} else {
    warning("remoteSession() tests NOT run
(remote host and/or user not defined)")
}

## No longer run by default because it needs Docker set up
notrun <- function() {
    ## Docker version
    ## Has to be run from 'sudo R' session (if user not in docker group)
    ## Generate image with R and gdiff installed
    f <- function() plot(1)
    ## NOTE that PDF same but Cairo different (text diffs)
    gdiff(f,
          device=list(pngDevice(), pdfDevice(useDingbats=TRUE),
                      cairo_pdf_device()),
          session=list(control=localSession(),
                       test=dockerSession("pmur002/gdiff-test",
                                          network="host")))
}

## Test function help examples
gdiffExamples("plot")
## Actual function rather than just function name
gdiffExamples(plot)
## Different R versions
if (nchar(rPath)) {
    gdiffExamples("axis",
                  session=list(control=currentSession(),
                               test=localSession(Rpath=rPath)))
}

## Test entire package
## currentsession
gdiffPackage("gridBezier")
## localsession
if (nchar(rPath)) {
    gdiffPackage("gridBezier",
                 session=list(control=currentSession(),
                              test=localSession(Rpath=rPath)))
}
if (nchar(remoteHost) && nchar(remoteUser)) {
    ## remotesession
    gdiffPackage("gridBezier",
                 session=list(control=currentSession(),
                              test=remoteSession(remoteHost, user=remoteUser)))
    ## clustersession
    cl <- makeCluster(remoteHost, user=remoteUser)
    gdiffPackage("gridBezier",
                 session=list(control=currentSession(),
                              test=remoteSession(cl, user=remoteUser)))
    stopCluster(cl)
} else {
    warning("remoteSession() tests NOT run
(remote host and/or user not defined)")
}
## No longer run by default because it needs Docker set up
notrun <- function() {
    ## dockersession
    gdiffPackage("gridBezier",
                 device=list(pngDevice(), pdfDevice(useDingbats=TRUE),
                             cairo_pdf_device()),
                 session=list(control=localSession(),
                              test=dockerSession("pmur002/gdiff-test",
                                                 network="host")))
}

## Docker1 vs Docker2 ?
