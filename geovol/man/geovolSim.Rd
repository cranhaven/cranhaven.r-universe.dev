\name{geovolSim}
\alias{geovolSim}

\title{Simulate from a GEOVOL model}

\description{
Simulate from a GEOVOL model.
}

\usage{
geovolSim(n, m, innovations = NULL, x.sd = 0.5, as.zoo = TRUE, verbose = FALSE)
}

\arguments{
  \item{n}{integer indicating the number of observations.}
  \item{m}{integer indicating the number of time series.}
  \item{innovations}{\code{NULL} or numeric vector with the innovations. If \code{NULL}, then standard normal innovations are generated with \code{\link{rnorm}}.}
  \item{x.sd}{numeric scalar indicating the standard deviation of the normal random variable, v, used to simulate the squared GEOVOL factor, x, according to the formula x = exp(v).}
  \item{as.zoo}{logical. If \code{TRUE}, then the returned result is of class \code{\link{zoo}}.}
  \item{verbose}{\code{logical}, if \code{TRUE}, the simulated time series, e, the innovations and the squared GEOVOL factor, x, are also returned.}
}

\value{A matrix with the simulated values.}

\author{Susana Campos-Martins}

\seealso{
\code{\link{geovol}},
\code{\link{geovolTest}},
}

\examples{
set.seed(123)

## Simulate from a GEOVOL model with s.d. 0.5 (default):
eSim <- geovolSim(n = 1000, m = 30)

## Simulate from a GEOVOL model with s.d. 1:
eSim <- geovolSim(n = 1000, m = 30, x.sd = 1)
}

\keyword{Volatility Factor Models}
\keyword{Multivariate Time Series}
\keyword{Econometrics}
\keyword{Financial Econometrics}
