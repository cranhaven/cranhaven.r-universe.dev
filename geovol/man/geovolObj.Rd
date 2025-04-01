\name{geovolObj}
\alias{geovolObj}

\title{Auxiliary function}

\description{
Auxiliary function used in the estimation of the GEOVOL model. Not intended for the average user.
}

\usage{
  geovolObj(par, par.fixed, y, x, flag)
}

\arguments{
  \item{par}{scalar with the value of the parameter being estimated. If \code{x}=TRUE, \code{par} contains the value of the GEOVOL factor at time t. Otherwise, it contains the value of the GEOVOL loading for time series i.}
  \item{par.fixed}{numeric vector with the values of the parameters being fixed during estimation. If \code{x}=TRUE, \code{par.fixed} contains the values of the estimated GEOVOL loadings from previous iteration. Otherwise, it contains the values of the estimated GEOVOL factor from previous iteration.}
  \item{y}{numeric vector, time series or \code{\link{zoo}} containing the cross-section at time t if \code{x}=TRUE or the time series i if otherwise.)}
  \item{x}{logical. If \code{TRUE}, it estimates the value of the GEOVOL factor at time t. Otherwise, it estimates the GEOVOL loading for time series i.}
  \item{flag}{integer. If 0, returns a numeric vector with the values of the objective function; if 1 returns the the value of the objective function; if 2, it returns the GEOVOL  scaled by the loading of series i, if \code{x}=FALSE.}
}

\value{
The values of the objective function or GEOVOL.
}

\author{Susana Campos-Martins}

\seealso{
\code{\link{geovol}}, \code{\link{fitted.geovol}}, \code{\link{residuals.geovol}} 
}

\keyword{Volatility Factor Models}
\keyword{Multivariate Time Series}
\keyword{Econometrics}
\keyword{Financial Econometrics}
