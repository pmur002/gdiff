\name{gdiffDevice}
\alias{gdiffDevice}
\alias{pngDevice}
\alias{pdfDevice}
\alias{postscriptDevice}
\alias{cairo_pdf_device}
\title{
  Define a Graphics Device for Generating Output
}
\description{
  Define the device to be used for generating graphical output files.
  There are several predefined graphical devices, e.g., 
  \code{pngDevice()}, and further devices can be defined
  using \code{gdiffDevice()}.
}
\usage{
pngDevice(...)
pdfDevice(...)
postscriptDevice(...)
cairo_pdf_device(suffix=".cairo.pdf", ...)

gdiffDevice(name, suffix=name, open,
            close=function(dir, name) dev.off())
}
\arguments{
  \item{name}{
    A name for the device (used by default for naming output files).
  }
  \item{suffix}{
    A suffix to be used for output files.
  }
  \item{open}{
    A function that opens a graphics device;  this should open the
    device in such a way that multiple pages of graphical output
    will produce multiple files.
  }
  \item{close}{
    A function that closes the graphics device.
  }
  \item{\dots}{
    Further arguments typically used within the function that
    opens the device.
  }
}
\value{
  A \code{"gdiffDevice"} object, which may be used as the \code{device}
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
\donttest{
gdiff(f, device=pdfDevice(),
      controlDir=file.path(tempdir(), "Control"),
      testDir=file.path(tempdir(), "Test"),
      compareDir=file.path(tempdir(), "Compare"))
}
}
\keyword{ dplot }
\concept{ visual testing }
