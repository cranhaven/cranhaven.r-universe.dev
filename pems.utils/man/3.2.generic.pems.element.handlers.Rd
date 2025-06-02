\name{3.2.generic.pems.element.handlers}
\alias{3.2.generic.pems.element.handlers}
\alias{generic.pems.element.handlers}
\alias{pems.element.generics}
\alias{print.pems.element}
\alias{plot.pems.element}
\alias{plot}
\alias{units.pems.element}
\alias{units<-.pems.element}
\alias{summary.pems.element}
\alias{summary}
\alias{as.pems.pems.element}
\alias{round.pems.element}
\alias{round}
\alias{[.pems.element}
\alias{[<-.pems.element}


\title{ Generic handling of pems.element objects }

\description{
  pems elements objects can be manipulated using generic functions like print, plot and 
  summary in a similar fashion to objects of other R classes. 
}

\usage{

\method{[}{pems.element} (x, i, ..., force = TRUE, wrap = FALSE)

\method{[}{pems.element} (x, i, ..., force = TRUE, wrap = FALSE) <- value

\method{as.pems}{pems.element}(x, ...)

\method{print}{pems.element}(x, ..., n = NULL, rows = NULL, width = NULL)

\method{plot}{pems.element}(x, y = NULL, xlab = NULL, ylab = NULL, ...)

\method{units}{pems.element}(x)

\method{units}{pems.element}(x) <- value

\method{summary}{pems.element}(object, ...)

\method{round}{pems.element}(x, ...)

}

\arguments{

  \item{x,object}{ 
  (An Object of pems.element class). For direct use with \code{print}, \code{plot}, \code{summary}, 
  etc. NOTE: Object naming (i.e., \code{x} or \code{object}) 
  is determined in parent or base function in R, so naming 
  varies by method.
}

  \item{i}{ 
  Element indices, which operate in a similar fashion to \code{vector} indices.
}

  \item{...}{ 
  Addition options, typically passed to associated default method(s).
}

  \item{force, wrap}{ 
  (Logicals) Data element handling options: \code{force} forces data to fit it destination; 
  \code{wrap} expands data to fit its destination by wrapping the source \code{pems.element}.
}

  \item{value}{
   (Vector) For calls in \code{pems.element[1] <- value} or \code{units(pems.element) <- value}, 
   the value to be inserted.
}

  \item{n, rows, width}{
   (Numerics) For \code{print(pems)}, number of elements, rows or screen width to foreshorten 
   print output to. 
}

  \item{y, xlab, ylab}{ 
  (other plot arguments). As with the default \code{\link{plot}} method, \code{y} is an optional 
  second data verctor, typically numeric, and \code{xlab} and \code{ylab} are labels to use on 
  x and y axes.
}

}

\value{
  Generic functions provide appropriate (conventional) 
  handling of objects of \code{'pems.elements'} class: 

  \code{print(pems.element)} provides a (to console) 
  description of the supplied \code{pems.element} object. 

  \code{plot(pems.element)} generates a standard R plot 
  of the supplied \code{pems.element}. 

  \code{units(pems.element)} extracts the units from the 
  supplied \code{pems.element}.  
 
}

\author{ Karl Ropkins }

\note{
  A dedicated \code{round(pems.element)} is required as 
  a wrapper to \code{round.Date} and \code{round.POSIXt} 
  handling.
}


\examples{

#the velocity pems.element in pems.1
pems.1$velocity

}

\keyword{methods}
