\name{median-methods}
\docType{methods}
\alias{median-methods}
\alias{median,ANY-method}
\alias{median,ros-method}
\alias{median,cenfit-method}
\alias{median,cenmle-method}
\alias{median,cenmle-lognormal-method}
\alias{median,cenmle-gaussian-method}
\title{Methods for function median in Package NADA}
\description{
  Methods for computing the median using model objects in package \pkg{NADA}
}
\usage{
\S4method{median}{ros}(x, na.rm=FALSE)

\S4method{median}{cenfit}(x, na.rm=FALSE)

\S4method{median}{cenmle}(x, na.rm=FALSE)

%\S4method{median}{cenmle-lognormal}(x, na.rm=FALSE, ...)

%\S4method{median}{cenmle-gaussian}(x, na.rm=FALSE, ...)

} % End usage

\arguments{

\item{x}{
    An output object from a NADA function such as \code{\link{ros}}.
}
\item{na.rm}{Should NAs be removed prior to computation?}

} % End arguments

\seealso{
    \code{\link{median}}
}

\keyword{methods}
