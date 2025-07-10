\name{cenfit-class}
\docType{class}
\alias{cenfit-class}
\alias{[,cenfit,numeric,missing-method}
\alias{[,cenfit,numeric,missing,ANY-method}

\title{Class "cenfit"}
\description{  
    A cenfit object is returned from the NADA 
    \code{cenfit} function.}
\section{Slots}{
  \describe{
    \item{\code{survfit}:}{Object of class \code{survfit} returned from
    the \code{survfit} function.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "cenfit", i = "numeric", j = "missing")}: ... }
    \item{mean}{\code{signature(x = "cenfit")}: ... }
    \item{median}{\code{signature(x = "cenfit")}: ... }
    \item{plot}{\code{signature(x = "cenfit", y = "ANY")}: ... }
    \item{predict}{\code{signature(object = "cenfit")}: ... }
    \item{print}{\code{signature(x = "cenfit")}: ... }
    \item{quantile}{\code{signature(x = "cenfit")}: ... }
    \item{sd}{\code{signature(x = "cenfit")}: ... }
    \item{summary}{\code{signature(object = "cenfit")}: ... }
  }
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
    \code{\link{cenfit}} 
}
\examples{
    obs      = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
    censored = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    class(cenfit(Cen(obs, censored)))
}
\keyword{classes}
