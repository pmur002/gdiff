\name{NEWS}
\title{NEWS file for the gdiff package}
\encoding{UTF-8}

\section{Changes in version 0.2-2}{
  \itemize{
    \item New function \code{diffFiles}.

    \item Attempt to handle broken output files (files that \pkg{magick}
    cannot read or convert or compare) more elegantly, including
    reporting them in the comparison result.

    \item If get an error reading PDF file with \pkg{magick}, try
    \command{pdftoppm} (if it exists).

    \item "Remote" sessions all now load \pkg{gdiff} package.
  }
}
\section{Changes in version 0.2-0}{
  \itemize{
    \item Fix listing of output files for \code{remoteSession} and
    \code{clusterSession}.
    
    \item Added \code{volumes} and \code{env} and \code{network} arguments to
    \code{dockerSession}.
  }
}
