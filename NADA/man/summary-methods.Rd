\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,ANY-method}
\alias{summary,ros-method}
\alias{summary,cenfit-method}
\alias{summary,cenreg-method}
\title{  Methods for function summary in Package `NADA' }
\description{
    Methods for summarizing objects in package \pkg{NADA} 
}
\usage{
\S4method{summary}{ros}(object, plot=FALSE, ...)

\S4method{summary}{cenfit}(object, ...)

\S4method{summary}{cenreg}(object, ...)

} % End usage
\arguments{

\item{object}{
    An output object from a NADA function such as \code{\link{ros}}.
}
\item{plot}{
    Logical indicating if summary graphs be generated?
}
\item{...}{
    Additional arguments passed to the generic method.
}
} % End arguments


\keyword{methods}
