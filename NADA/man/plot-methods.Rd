\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,cenfit,ANY-method}
\alias{plot,ros,missing-method}
\alias{plot,ros-method}
\alias{plot,cenfit-method}
\alias{plot,cenmle-method}
\alias{plot,cenmle-lognormal,ANY-method}
\alias{plot,cenmle-gaussian,ANY-method}
\alias{plot,cenreg-method}
\alias{plot,cenreg,ANY-method}
\title{Methods for function plot in Package NADA}
\description{
  Methods for plotting objects in package \pkg{NADA} 
}
\usage{
\S4method{plot}{ros}(x, plot.censored=FALSE, lm.line=TRUE, grid=TRUE, ...)

\S4method{plot}{cenfit}(x, conf.int=FALSE, ...)

\S4method{plot}{cenmle}(x, ...)

\S4method{plot}{cenreg}(x, ...)

} % End usage

\arguments{

\item{x}{
    An output object from a NADA function such as \code{\link{ros}}.
}

\item{conf.int}{
        A logical indicating if confidence intervals should be computed.  For
        \code{cenfit} objects, the confidence interval is set during the call
        to \code{\link{cenfit}}.  Currently not supported for \code{ros} 
        objects.
}
\item{plot.censored}{\code{ros}: should censored values be plotted?}
\item{lm.line}{\code{ros}: should the linear regression line be plotted?}
\item{grid}{\code{ros}: should a grid be overlayed?}

\item{...}{
        Additional arguments passed to the generic method.
}

} % End arguments

\seealso{
    \code{\link{plot}}
}
\keyword{methods}
