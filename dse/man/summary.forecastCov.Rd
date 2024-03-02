\name{summary.forecastCov}
\alias{summary.forecastCov}
\alias{summary.forecastCovEstimatorsWRTdata.subsets}
\alias{summary.forecastCovEstimatorsWRTtrue}
\alias{summary.estimatedModels}

\alias{print.summary.forecastCov}
\alias{print.summary.forecastCovEstimatorsWRTdata.subsets}
\alias{print.summary.forecastCovEstimatorsWRTtrue}
\alias{print.summary.estimatedModels}

\title{Summary Specific Methods}
\description{See the generic function description.}
\usage{
    \method{summary}{forecastCov}(object, horizons=object$horizons,
        series=seq(nseriesOutput(object$data)), ...)
    \method{summary}{forecastCovEstimatorsWRTdata.subsets}(object, ...)
    \method{summary}{forecastCovEstimatorsWRTtrue}(object,
        digits=options()$digits, ...)
    \method{summary}{estimatedModels}(object, ...)
    
    \method{print}{summary.forecastCov}(x, digits=options()$digits, ...)
    \method{print}{summary.forecastCovEstimatorsWRTdata.subsets}(x,
       digits=options()$digits, ...)
    \method{print}{summary.forecastCovEstimatorsWRTtrue}(x,
       digits=options()$digits, ...)
    \method{print}{summary.estimatedModels}(x, digits=options()$digits, ...)
}
\arguments{
    \item{object}{an object for which a summary is to be printed.}
    \item{x}{an object for which a summary is to be printed.}
    \item{digits}{a non-null value is used to indicate the number of significant
       digits. If \code{digits} is \code{NULL} then the value of digits 
       specified by \code{options} is used.}
    \item{horizons}{optional integer vector indicating horizons at which the 
       summary should be calculated.}
    \item{series}{
       The series which should be plotted. The default NULL gives all series.}
    \item{...}{arguments passed to other methods.}
}

\seealso{
    \code{\link{summary}}
    \code{\link{print}}
}
\concept{DSE}
\keyword{ts}


