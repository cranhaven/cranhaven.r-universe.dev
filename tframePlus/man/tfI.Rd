\name{tfI}
\alias{tfI}

\title{Integrate Multivariate Time Series}
\description{Integrate multivariate time series using cumsum}

\usage{
    tfI (x, I=rep(TRUE, NCOL(x)), t0=rep(0, NCOL(x)))
}

\arguments{
    \item{x}{a multivariate time series object.}
    \item{I}{a logical of length equal the number of series in \code{x} 
       indicating which series to integrate, default all.}
    \item{t0}{a vector of length equal the number of series in \code{x} 
       indicating initial (time zero) valuess, defaults to zero.}
}
\value{a multivariate time series object of the same dimension as \code{x}.}
\details{
  This function first prepends \code{t0} to \code{x} and then applies 
  \code{cumsum} to each series indicated by \code{I}. The first value is then
  dropped from the result.
}

\seealso{
    \code{\link[tframe]{tfL}}
    \code{\link[tframe]{tfwindow}}
    \code{\link[stats]{aggregate}}
    \code{\link[base]{cumsum}}
    \code{\link{as.quarterly}}
}
\keyword{utilities}
\keyword{ts}
