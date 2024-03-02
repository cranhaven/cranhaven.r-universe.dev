\name{TSmodel}
\alias{TSmodel}
\alias{TSmodel.TSmodel}
\alias{TSmodel.TSestModel}
\alias{is.TSmodel}

\title{Time Series Models}
\description{Construct or extract a "TSmodel" from objects.}
\usage{
    TSmodel(obj, ...)
    \method{TSmodel}{TSmodel}(obj, ...)
    \method{TSmodel}{TSestModel}(obj, ...)
    is.TSmodel(obj)
}
\arguments{
    \item{obj}{ 
      An object containing an object of class TSmodel or a list containing the 
      information necessary to build an object of class \code{TSmodel}.}
    \item{...}{arguments passed to other methods.}
}
\details{
This is a generic method which will extract a \code{TSmodel} from an 
object (e.g. a \code{TSestModel}. 
The default method will try to build an ARMA or state-space 
\code{TSmodel} from a list, which must contain the necessary information.

This class of objects is returned by estimation methods or can be built 
according to the description for specific 
sub-classes (e.g. \code{ARMA}, \code{SS}).

The \code{TSmodel} class of objects has methods for the generic functions
\code{print, testEqual, seriesNames, seriesNamesInput,
seriesNamesOutput, l, roots, stability,  
forecast, featherForecasts, horizonForecasts,
simulate, MonteCarloSimulations}

Also, the function \code{is.TSmodel} and the functions \code{toSS, toARMA, to.troll} are supported.
Other model classes inherit from the class \code{TSmodel}.

This class of objects contains a time series model. It is the class of
objects expected by many of the functions in this package.  

Sub-class (e.g. ARMA and SS for linear,
time-invariant ARMA and state space models.) are documented individually.
Many of the functions in this package are
designed for estimating and converting among various representations of
these types of models.
}

\seealso{
    \code{\link{ARMA}},
    \code{\link{SS}},
    \code{\link{TSestModel}},
    \code{\link{TSdata}}
}
\concept{DSE}
\keyword{ts}

