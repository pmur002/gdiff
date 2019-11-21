\name{gdiffPackage}
\alias{gdiffPackage}
\title{
  Generate and Compare Control and Test Output for a Package
}
\description{
  Generate a set of "control" graphical output files in one directory,
  a set of "test" graphical output files in another directory,
  and compare the two sets of output files (possibly generating
  "compare" graphical output of the differences in a third directory).
}
\usage{
gdiffPackage(pkg, ...)
}
\arguments{
  \item{pkg}{
    The name of a package.
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
  \code{\link{gdiffExamples}}.
}
\keyword{ dplot }
\concept{ visual testing }
