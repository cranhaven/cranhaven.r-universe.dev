\name{quantile-methods}
\docType{methods}
\alias{quantile-methods}
\alias{quantile,ANY-method}
\alias{quantile,cenfit-method}
\alias{quantile,ros-method}
\alias{quantile,cenmle-method}
\alias{quantile,cenmle-lognormal-method}
\alias{quantile,cenmle-gaussian-method}
\alias{NADAprobs}
\title{Methods for function quantile in Package NADA}
\description{
  Methods for the function \code{quantile} in package \pkg{NADA} 

  Compute the modeled values of quantiles or probabilities using a model object.
}
\usage{

\S4method{quantile}{ros}(x, probs=NADAprobs, ...)

\S4method{quantile}{cenfit}(x, probs=NADAprobs, conf.int=FALSE, ...)

\S4method{quantile}{cenmle}(x, probs=NADAprobs, conf.int=FALSE, ...)

} % usage
\arguments{

\item{x}{
    An output object from a NADA fuction such as \code{\link{ros}}.
}
\item{probs}{
    Numeric vector of probabilities for which to calculate model values.
    The default is the global variable 
    NADAprobs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95).
}
\item{conf.int}{
        A logical indicating if confidence intervals should be computed.
        For \code{cenfit} and \code{cenmle} objects, the confidence
        interval is set during the call to \code{\link{cenfit}}.
        Currently not supported for \code{ros} objects.
}
\item{...}{
        Additional arguments passed to the generic method.
}
} % arguments
\examples{
    data(Cadmium)

    mymodel = cenfit(Cadmium$Cd, Cadmium$CdCen, Cadmium$Region)

    quantile(mymodel, conf.int=TRUE)
}
\keyword{methods}
