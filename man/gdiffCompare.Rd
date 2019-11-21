\name{gdiffCompare}
\alias{gdiffCompare}
\title{
  Compare Control and Test Output
}
\description{
  Compare a set of "control" graphical output files in one directory
  with 
  a set of "test" graphical output files in another directory
  (possibly generating
  "compare" graphical output of the differences in a third directory).
}
\usage{
gdiffCompare(controlDir, testDir, compareDir, ...)
}
\arguments{
  \item{controlDir, testDir, compareDir}{
    The names of the directories where output is produced.
  }
  \item{\dots}{
    Further arguments, not currently used.
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
  \code{\link{gdiff}}, and
  \code{\link{gdiffOutput}}.
}
\keyword{ dplot }
\concept{ visual testing }
