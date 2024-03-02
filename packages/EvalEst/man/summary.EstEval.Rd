\name{summary.EstEval}
\alias{summary.EstEval}
\alias{summary.TSestModelEstEval}
\alias{summary.TSmodelEstEval}
\alias{summary.MonteCarloSimulations}
\alias{summary.coefEstEval}
\alias{summary.rootsEstEval}

\alias{print.summary.TSestModelEstEval}
\alias{print.summary.TSmodelEstEval}
\alias{print.summary.EstEval}
\alias{print.summary.MonteCarloSimulations}
\alias{print.summary.coefEstEval}
\alias{print.summary.rootsEstEval}

\title{Summary Specific Methods}
\description{See the generic function description.}
\usage{
    \method{summary}{TSestModelEstEval}(object, ...)
    \method{summary}{TSmodelEstEval}(object, ...)
    \method{summary}{EstEval}(object, ...)
    \method{summary}{MonteCarloSimulations}(object, series=NULL, periods=1:3, ...)
    \method{summary}{coefEstEval}(object, verbose=TRUE, ...)
    \method{summary}{rootsEstEval}(object, verbose=TRUE, ...)
    
    \method{print}{summary.TSestModelEstEval}(x, digits=options()$digits, ...)
    \method{print}{summary.TSmodelEstEval}(x, digits=options()$digits, ...)
    \method{print}{summary.EstEval}(x, digits=options()$digits, ...)
    \method{print}{summary.MonteCarloSimulations}(x, digits=options()$digits, ...)
    \method{print}{summary.coefEstEval}(x, digits=options()$digits, ...)
    \method{print}{summary.rootsEstEval}(x, digits=options()$digits, ...)
}
\arguments{
    \item{object}{an object for which a summary is to be printed.}
    \item{x}{an object for which a summary is to be printed.}
    \item{digits}{a non-null value is used to indicate the number of significant
       digits. If \code{digits} is \code{NULL} then the value of digits 
       specified by \code{options} is used.}
    \item{series}{
       The series which should be plotted. The default NULL gives all series.}
    \item{periods}{optional integer vector indicating periods at which the 
       summary should be calculated.}
    \item{verbose}{logical indicating if a longer summary should be produced.}
    \item{...}{arguments passed to other methods.}
}

\seealso{
    \code{\link{summary}}
    \code{\link{print}}
}
\concept{DSE}
\keyword{ts}


