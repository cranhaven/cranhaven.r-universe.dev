\name{cenken-class}
\docType{class}
\alias{cenken-class}

\title{Class "cenken"}
\description{
    A "cenken" object is returned from \code{cenken}.  It extends the
    `list' class. 
}
\section{Objects from the Class}{
  Objects can be created by calls of the form 
  \code{cenken(y, ycen, x, xcen)}.
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
    \item{lines}{\code{signature(x = "cenken")}: ... }
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
%    # This is wrong for cenken
%    x        = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
%    xcen     = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
%
%    class(cenken(x, xcen))
%}
\keyword{classes}
