\name{mean-methods}
\docType{methods}
\alias{mean-methods}
\alias{mean,ANY-method}
\alias{mean,ros-method}
\alias{mean,cenfit-method}
\alias{mean,cenmle-method}
\alias{mean,cenmle-lognormal-method}
\alias{mean,cenmle-gaussian-method}
\title{Methods for function mean in Package NADA}
\description{
    Methods for computing the mean using model objects in package \pkg{NADA}
}
\usage{
\S4method{mean}{ros}(x, ...)

\S4method{mean}{cenfit}(x, ...)

\S4method{mean}{cenmle}(x, ...)

%\S4method{mean}{cenmle-lognormal}(x, ...)

%\S4method{mean}{cenmle-gaussian}(x, ...)

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
    \code{\link{mean}}
}

\keyword{methods}
