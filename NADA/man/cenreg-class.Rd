\name{cenreg-class}
\docType{class}
\alias{cenreg-class}
\alias{cenreg-lognormal}
\alias{cenreg-gaussian}

\title{Class "cenreg"}
\description{
    A "cenreg" object is returned from \code{cenreg}.  It extends the
    `cenreg' class returned from \code{survreg}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{cenreg(obs, censored, groups)}.
}
\section{Slots}{
  \describe{
    \item{\code{conf.int}:}{Numeric value of confidence level (0.95)}
    \item{\code{n}:}{Total number of samples}
    \item{\code{n.cen}:}{Total censored samples}
    \item{\code{survreg}:}{Object of class \code{"survreg"}  }
    \item{\code{y}:}{Total y samples}
    \item{\code{ycen}:}{Total censored y samples}
  }
}
\section{Extends}{
Class \code{"list"}, from data part.
Class \code{"vector"}, by class \code{"list"}.
}
\section{Methods}{
  \describe{
    \item{predict}{\code{signature(object = "cenreg")}: ... }
    \item{print}{\code{signature(x = "cenreg")}: ... }
    \item{summary}{\code{signature(object = "cenreg")}: ... }
  }
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
  \code{\link{survreg}}
}
%\examples{
%    # This is wrong for cenreg
%    x        = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
%    xcen     = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
%
%    class(cenreg(x, xcen))
%}
\keyword{classes}
