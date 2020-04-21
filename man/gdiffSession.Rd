\name{gdiffSession}
\alias{gdiffSession}
\alias{gdiffGenerateOutput}
\alias{currentSession}
\alias{localSession}
\alias{remoteSession}
\alias{remoteSession.character}
\alias{remoteSession.cluster}
\alias{dockerSession}
\title{
  Define an R Session for Generating Output
}
\description{
  Define the R session to be used for generating graphical output files.
  There are several predefined sessions, e.g., 
  \code{currentSession()}, and further sessions can be defined
  using \code{gdiffSession()}.
}
\usage{
currentSession(libPaths=NULL)
localSession(libPaths=NULL,
             Rpath=file.path(R.home("bin"), "Rscript"),
             ...)
remoteSession(remote, ...)
\method{remoteSession}{character}(remote, libPaths=NULL, Rpath="Rscript", ...)
\method{remoteSession}{cluster}(remote, libPaths=NULL, user=NULL, ...)
dockerSession(image, volumes=NULL, env=NULL, network="bridge",
              libPaths=NULL, Rpath="Rscript", ...)

gdiffSession(class, ...)
gdiffGenerateOutput(codeFun, dir, device, clean, ncpu)
}
\arguments{
  \item{libPaths}{
    One or more paths to installed R packages.
  }
  \item{Rpath}{
    A path to an \code{Rscript} binary.
  }
  \item{remote}{
    Either the name of a host or a cluster object (as produced by
    \code{parallel::makeCluster()}).
  }
  \item{user}{
    A user name.
  }
  \item{image}{
    The name of a Docker image.
  }
  \item{volumes}{
    A character vector of volumes to mount on the container (of the
    form \code{/path/on/host:/path/on/container/}).
  }
  \item{env}{
    A character vector of environment variable settings for the
    container
    (of the form \code{VAR=value}).
  }
  \item{network}{
    A character vector describing the network connection for the container.
  }
  \item{class}{
    A unique class for a new type of R session.
  }
  \item{\dots}{
    Further arguments for future methods.
  }
  \item{codeFun, dir, device, clean, ncpu}{
    Arguments passed to \code{generateOutput} methods
    that can be passed on to \code{gdiffGenerateOutput}.
  }
}
\details{
  When defining a new session, \code{gdiffSession()} is just used
  to establish a new class.  The important thing to do is to provide a
  \code{generateOutput} method for that class.  Typically, this
  method will call \code{gdiffGenerateOutput()}.
}
\value{
  A \code{"gdiffSession"} object, which may be used as the \code{session}
  argument to \code{\link{gdiff}}.  
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{gdiff}}.
}
\examples{
f <- function() plot(1)
gdiff(f, session=currentSession(),
      controlDir=file.path(tempdir(), "Control"),
      testDir=file.path(tempdir(), "Test"),
      compareDir=file.path(tempdir(), "Compare"))
}
\keyword{ dplot }
\concept{ visual testing }
