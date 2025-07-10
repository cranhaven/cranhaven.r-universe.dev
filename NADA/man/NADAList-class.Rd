\name{NADAList-class}
\docType{class}
\alias{NADAList-class}

\title{Class "NADAList"}
\description{
    A "NADAList" simply extends the `list' class.
}
\section{Objects from the Class}{
  NADAList objects are created by calls like 
  \code{cenken(y, ycen, x, xcen)} and other functions.
}
\section{Slots}{
  \describe{
  %No slots, it just extends a list.
    \item{\code{.Data}:}{Object of class \code{"list"}  }
  }
}
\section{Extends}{
    Class \code{"list"}, from data part.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "NADAList")}: ... }
  }
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
  \code{\link{cenken}}
}
%\examples{
%}
\keyword{classes}
