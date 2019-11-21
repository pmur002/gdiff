\name{gdiffExamplesOutput}
\alias{gdiffExamplesOutput}
\alias{gdiffExamplesOutput.function}
\alias{gdiffExamplesOutput.character}
\title{
  Generate Output Files from Function Examples
}
\description{
  Generate a set of graphical output files by running the
  examples from the help page for a function.
}
\usage{
gdiffExamplesOutput(fun, dir, ...)
\method{gdiffExamplesOutput}{function}(fun, dir, name=NULL, ...)
\method{gdiffExamplesOutput}{character}(fun, dir, name=fun, ...)
}
\arguments{
  \item{fun}{
    Either a function or the name of a function.
  }
  \item{dir}{
    The name of a directory in which to create output files.
  }
  \item{name}{
    A name to be used for output files.
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
  \code{\link{gdiffPackageOutput}}.
}
\examples{
gdiffExamplesOutput(plot, dir=file.path(tempdir(), "Control"))
}
\keyword{ dplot }
\concept{ visual testing }
