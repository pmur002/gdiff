\name{gdiffPackageOutput}
\alias{gdiffPackageOutput}
\title{
  Generate Output Files from Function Package
}
\description{
  Generate a set of graphical output files by running the
  examples from the help page for a function.
}
\usage{
gdiffPackageOutput(pkg, dir, ...)
}
\arguments{
  \item{pkg}{
    The name of a package.
  }
  \item{dir}{
    The name of a directory in which to create output files.
  }
  \item{\dots}{
    Further arguments; see \code{\link{gdiffOutput}}.
  }
}
\value{
  A character vector containing the names (and paths) of all
  output files that were generated.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{gdiffOutput}} and
  \code{\link{gdiffExamplesOutput}}.
}
\keyword{ dplot }
\concept{ visual testing }
