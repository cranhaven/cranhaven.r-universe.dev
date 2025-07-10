\name{cenmle-class}
\docType{class}
\alias{cenmle-class}
\alias{cenmle-lognormal}
\alias{cenmle-gaussian}

\title{Class "cenmle"}
\description{
    A "cenmle" object is returned from \code{cenmle}.  It extends the
    `cenreg' class returned from \code{survreg}.
}
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{cenmle(obs, censored)}.
}
\section{Slots}{
  \describe{
    \item{\code{survreg}:}{Object of class \code{"survreg"}  }
  }
}
\section{Extends}{
Class \code{"list"}, from data part.
Class \code{"vector"}, by class \code{"list"}.
}
\section{Methods}{
  \describe{
    \item{mean}{\code{signature(x = "cenmle")}: ... }
    \item{median}{\code{signature(x = "cenmle")}: ... }
    \item{sd}{\code{signature(x = "cenmle")}: ... }
    %\item{predict}{\code{signature(object = "cenmle")}: ... }
    %\item{print}{\code{signature(x = "cenmle")}: ... }
    %\item{quantile}{\code{signature(x = "cenmle")}: ... }
    \item{summary}{\code{signature(object = "cenmle")}: ... }
  }
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
  \code{\link{survreg}}
}
\examples{
    x        = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
    xcen     = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    class(cenmle(x, xcen))
}
\keyword{classes}
