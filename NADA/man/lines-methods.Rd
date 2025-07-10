\name{lines-methods}
\docType{methods}
\alias{lines-methods}
\alias{lines,ANY-method}
\alias{lines,ros-method}
\alias{lines,cenfit-method}
\alias{lines,cenken-method}
\title{Methods for function lines in Package NADA}
\description{
    Methods for adding \code{lines} to plots in package \pkg{NADA}
}
\usage{
\S4method{lines}{ros}(x, ...)

\S4method{lines}{cenfit}(x, ...)

\S4method{lines}{cenken}(x, ...)

} % End usage

\arguments{

\item{x}{
    An output object from a NADA function such as \code{\link{ros}}.
}
\item{...}{
        Additional arguments passed to the generic method.
}

} % End arguments

\seealso{
    \code{\link{lines}},
    \code{\link{plot}}
}

\keyword{methods}
