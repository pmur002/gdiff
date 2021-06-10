\name{gdiffExamples}
\alias{gdiffExamples}
\alias{gdiffExamples.function}
\alias{gdiffExamples.character}
\title{
  Generate and Compare Control and Test Output for Function Examples
}
\description{
  Generate a set of "control" graphical output files in one directory,
  a set of "test" graphical output files in another directory,
  and compare the two sets of output files (possibly generating
  "compare" graphical output of the differences in a third directory).
}
\usage{
gdiffExamples(fun, ...)
\method{gdiffExamples}{function}(fun, name=NULL, ...)
\method{gdiffExamples}{character}(fun, name=fun, ...)
}
\arguments{
  \item{fun}{
    Either a function or the name of a function.
  }
  \item{name}{
    A name to be used for output files.
  }
  \item{\dots}{
    Further arguments; see \code{\link{gdiff}}.
  }
}
\value{
  A \code{"gdiffComparison"} object, which is a list containing
  information about the output files generated and the differences
  detected.  
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{gdiff}} and
  \code{\link{gdiffPackage}}.
}
\examples{
\donttest{
gdiffExamples(plot,
              controlDir=file.path(tempdir(), "Control"),
              testDir=file.path(tempdir(), "Test"),
              compareDir=file.path(tempdir(), "Compare"))
}
}
\keyword{ dplot }
\concept{ visual testing }
