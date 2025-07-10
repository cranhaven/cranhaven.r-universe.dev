\name{ros-class}
\docType{class}
\alias{ros-class}
%\alias{lines,ros-method}
%\alias{mean,ros-method}
%\alias{median,ros-method}
%\alias{plot,ros,missing-method}
%\alias{predict,ros-method}
%\alias{print,ros-method}
%\alias{quantile,ros-method}
%\alias{sd,ros-method}
%\alias{summary,ros-method}

\title{Class "ros"}
\description{
    A "ros" object is returned from \code{ros}.  It extends the
    "lm" class returned from \code{lm}.
}
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{ros(obs, censored)}.
}
\section{Slots}{
  \describe{
    %No slots, it just extends a list.
    \item{\code{.Data}:}{Object of class \code{"list"}  }
  }
}
\section{Extends}{
Class \code{"list"}, from data part.
Class \code{"vector"}, by class \code{"list"}.
}
\section{Methods}{
  \describe{
    \item{lines}{\code{signature(x = "ros")}: ... }
    \item{mean}{\code{signature(x = "ros")}: ... }
    \item{median}{\code{signature(x = "ros")}: ... }
    \item{plot}{\code{signature(x = "ros", y = "missing")}: ... }
    \item{predict}{\code{signature(object = "ros")}: ... }
    \item{print}{\code{signature(x = "ros")}: ... }
    \item{quantile}{\code{signature(x = "ros")}: ... }
    \item{sd}{\code{signature(x = "ros")}: ... }
    \item{summary}{\code{signature(object = "ros")}: ... }
  }
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
  \code{\link{ros}}
}
\examples{
    obs      = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
    censored = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    class(ros(obs, censored))
}
\keyword{classes}
