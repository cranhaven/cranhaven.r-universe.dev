\name{print.forecastCov}
\alias{print.estimatedModels}
\alias{print.forecastCov}
\alias{print.forecastCovEstimatorsWRTdata.subsets}
\alias{print.forecastCovEstimatorsWRTtrue}

\title{Print Specific Methods}
\description{See the generic function description.}
\usage{
    \method{print}{estimatedModels}(x, digits=options()$digits, ...)
    \method{print}{forecastCov}(x, digits=options()$digits, ...)
    \method{print}{forecastCovEstimatorsWRTdata.subsets}(x, digits=options()$digits, ...)
    \method{print}{forecastCovEstimatorsWRTtrue}(x, digits=options()$digits, ...)
}
\arguments{
    \item{x}{an object to be printed.}
    \item{digits}{a non-null value is used to indicate the number of significant
       digits. If \code{digits} is \code{NULL} then the value of digits 
       specified by \code{options} is used.}
    \item{...}{(further arguments, currently disregarded).}
}
\seealso{
    \code{\link{print}}
    \code{\link{summary}}
}

\concept{DSE}
\keyword{ts}

