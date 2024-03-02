\name{summary.TSdata}
\alias{summary.TSdata}
\alias{summary.SS}
\alias{summary.ARMA}
\alias{summary.TSestModel}

\alias{print.summary.TSdata}
\alias{print.summary.SS}
\alias{print.summary.ARMA}
\alias{print.summary.TSestModel}

\title{Specific Methods for Summary}
\description{See the generic function description.}
\usage{
    \method{summary}{TSdata}(object, ...)
    \method{summary}{SS}(object, ...)
    \method{summary}{ARMA}(object, ...)
    \method{summary}{TSestModel}(object, ...)
    \method{print}{summary.TSdata}(x, digits=options()$digits, ...)
    \method{print}{summary.SS}(x, digits=options()$digits, ...)
    \method{print}{summary.ARMA}(x, digits=options()$digits, ...)
    \method{print}{summary.TSestModel}(x, digits=options()$digits, ...)
    }
\arguments{
    \item{object}{an object to be summarized.}
    \item{x}{a summary object to be printed.}
    \item{digits}{number of significant digits to use for printing.}
    \item{...}{arguments passed to other methods.}
}

\seealso{
    \code{\link{print}}, 
    \code{\link{summary}}
}

\concept{DSE}
\keyword{ts}

