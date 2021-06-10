\name{diffFiles}
\alias{diffFiles}
\title{
  Names of Files Showing Differences
}
\description{
  List all files that show differences between control and test output
  (as red pixels).
}
\usage{
diffFiles(x)
}
\arguments{
  \item{x}{
    A \code{"gdiffComparison"} object, as created by
    \code{\link{gdiff}} or \code{\link{gdiffCompare}}.
  }
}
\value{
  A character vector of file names (with paths).  
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{gdiff}} and
  \code{\link{gdiffCompare}}.
}
\examples{
f1 <- function() plot(1)
f2 <- function() plot(2)
\donttest{
result <- gdiff(list(control=f1, test=f2), name="f",
                controlDir=file.path(tempdir(), "Control"),
                testDir=file.path(tempdir(), "Test"),
                compareDir=file.path(tempdir(), "Compare"))
result
diffFiles(result)
}
}
\keyword{ dplot }
\concept{ visual testing }
