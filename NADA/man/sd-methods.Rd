\name{sd-methods}
\docType{methods}
\alias{sd-methods}
\alias{sd,ANY-method}
\alias{sd,cenfit-method}
\alias{sd,ros-method}
\alias{sd,cenmle-method}
\alias{sd,cenmle-lognormal-method}
\alias{sd,cenmle-gaussian-method}
\title{Methods for function sd in Package NADA}
\description{
  Methods for computing standard deviations in package \pkg{NADA} 
}
\usage{
\S4method{sd}{ros}(x, na.rm=FALSE)

\S4method{sd}{cenfit}(x, na.rm=FALSE)

\S4method{sd}{cenmle}(x, na.rm=FALSE)

} % End usage

\arguments{

\item{x}{
    An output object from a NADA function such as \code{\link{ros}}.
}
\item{na.rm}{Should NAs be removed prior to computation?}

} % End arguments
\seealso{\code{\link{sd}}}

\keyword{methods}
