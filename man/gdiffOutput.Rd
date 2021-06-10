\name{gdiffOutput}
\alias{gdiffOutput}
\alias{gdiffOutput.function}
\title{
  Generate Graphical Output
}
\description{
  Generate a set of graphical output files.
}
\usage{
gdiffOutput(x, dir, ...)
\method{gdiffOutput}{function}(x, dir, name=deparse(substitute(x)), ...)
}
\arguments{
  \item{x}{
    A function.
  }
  \item{dir}{
    The name of a directory in which to create output files.
  }
  \item{name}{
    A name to be used for output files.
  }
  \item{\dots}{
    Further arguments, currently including:
    \describe{
      \item{clean}{
        A logical indicating whether the output directories should
        be emptied.  Can also be a list of logicals with names
        \code{control}, \code{test}, and \code{compare}.
      }
      \item{device}{
        A specification of the graphics device to use for output;
        see \code{\link{gdiffDevice}}.        
        Can also be a list of graphics devices.
      }
      \item{session}{
        A specification of the R session to use for output;
        see \code{\link{gdiffSession}}.
        Can also be a list of sessions with names \code{control}
        and \code{test}.
      }
      \item{ncpu}{
        How many cpus to employ when generating output.
      }
    }
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
  \code{\link{gdiffExamplesOutput}},
  \code{\link{gdiffPackageOutput}}, and
  \code{\link{gdiff}}.
}
\examples{
f <- function() plot(1)
\donttest{
gdiffOutput(f, dir=file.path(tempdir(), "Control"))
}
}
\keyword{ dplot }
\concept{ visual testing }
