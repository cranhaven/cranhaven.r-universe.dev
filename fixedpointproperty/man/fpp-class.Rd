\name{fpp-class}
\Rdversion{1.1}
\docType{class}
\alias{fpp-class}
\alias{fpp}
\alias{plot,fpp-method}

\title{Class \code{"fpp"}}
\description{
Objects of the fpp class are used by methods and functions of the \code{fp} package, to visualize and compute the fixed-point property in response time data.

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fpp", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{dens}:}{Object of class \code{"array"}. This is an array of objects of class \code{"density"}.}
    \item{\code{diff}:}{Object of class \code{"data.frame"}. This is a dataframe with the paired differences between densities.}
    \item{\code{dat}:}{Object of class \code{"data.frame"}. This is a dataframe with the input data. }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "fpp")}: ... }
	 }
}
\references{
Van Maanen, L., De Jong, R., Van Rijn, H (2014). How to assess the existence of competing strategies in cognitive tasks: A primer on the fixed-point property. PLOS One, 9, e106113

Van Maanen, L. Couto, J. & Lebetron, M. (2016). Three boundary conditions for computing the fixed-point property in binary mixture data. PLOS One, 11, e0167377.
}
\author{
Leendert van Maanen (l.vanmaanen@uu.nl)
}

\examples{
showClass("fpp")
}
\keyword{classes}
