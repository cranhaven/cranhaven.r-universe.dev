\name{print.estimatedModels}
\alias{print.EstEval}
\alias{print.MonteCarloSimulations}

\title{Print Specific Methods}
\description{See the generic function description.}
\usage{
    \method{print}{EstEval}(x, digits=options()$digits, ...)
    \method{print}{MonteCarloSimulations}(x, digits=options()$digits, ...)
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

