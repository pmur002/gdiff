\name{gdiff}
\alias{gdiff}
\alias{gdiff.function}
\alias{gdiff.list}
\title{
  Generate and Compare Control and Test Output
}
\description{
  Generate a set of "control" graphical output files in one directory,
  a set of "test" graphical output files in another directory,
  and compare the two sets of output files (possibly generating
  "compare" graphical output of the differences in a third directory).
}
\usage{
gdiff(x, ...)
\method{gdiff}{function}(x, name=deparse(substitute(x)), ...)
\method{gdiff}{list}(x, name, ...)
}
\arguments{
  \item{x}{
    Either a function, or a named list
    of functions (with names \code{control} and \code{test}).
  }
  \item{name}{
    A name to be used for output files.
  }
  \item{\dots}{
    Further arguments, currently including:
    \describe{
      \item{controlDir, testDir, compareDir}{
        The names of the directories where output is produced.
      }
      \item{clean}{
        A logical indicating whether the output directories should
        be emptied.  Can also be a list of logicals with names
        \code{control}, \code{test}, and \code{compare}.
      }
      \item{compare}{
        A logical indicating whether to perform the comparison step.
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
  A \code{"gdiffComparison"} object, which is a list containing
  information about the output files generated and the differences
  detected.  
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{gdiffExamples}},
  \code{\link{gdiffPackage}},
  \code{\link{gdiffOutput}}, and 
  \code{\link{gdiffCompare}}.
}
\examples{
f <- function() plot(1)
gdiff(f,
      controlDir=file.path(tempdir(), "Control"),
      testDir=file.path(tempdir(), "Test"),
      compareDir=file.path(tempdir(), "Compare"))
}
\keyword{ dplot }
\concept{ visual testing }
