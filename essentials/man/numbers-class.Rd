\name{numbers-class}
\alias{coerce,ANY,numbers-method}
\Rdversion{1.1}
\docType{class}
\alias{numbers-class}

\title{Class \code{"numbers"}}
\description{
An umbrella formal class encompassing all objects interpretable as numbers. This
includes \code{integer}, \code{double} and \code{complex}.
}
\section{Extends}{
This class extends both "numeric" and "complex", directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{
      A method is defined to coerce an arbitrary object to a numbers vector by
      calling \code{\link{as.numbers}}. The object is returned as is if it
      already extends class "numeric" or "complex".
    }
  }
}
\keyword{classes}
