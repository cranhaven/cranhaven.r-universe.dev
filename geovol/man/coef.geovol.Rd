\name{coef.geovol}

\alias{coef.geovol}
\alias{fitted.geovol}
\alias{logLik.geovol}
\alias{nobs.geovol}
\alias{print.geovol}
\alias{residuals.geovol}

\title{Extraction functions for a 'geovol' object}

\description{Extraction functions for objects of class 'geovol'.}

\usage{
  \method{coef}{geovol}(object, as.zoo = TRUE, ...)
  \method{fitted}{geovol}(object, as.zoo = TRUE, ...)
  \method{logLik}{geovol}(object, ...)
  \method{nobs}{geovol}(object, ...)
  \method{print}{geovol}(x, n.extreme = 20, ...)
  \method{residuals}{geovol}(object, as.zoo = TRUE, ...)
}

\arguments{
  \item{object}{an object of class 'geovol'.}
  \item{x}{an object of class 'geovol'.}
  \item{as.zoo}{logical. If \code{TRUE}, then the returned result is of class \code{\link{zoo}}.}
  \item{n.extreme}{\code{integer} indicating the \code{n.extreme} most extreme GEOVOL events to be printed.}
  \item{\dots}{additional arguments.}
}

\value{
  \item{coef:}{numeric vector containing parameter estimates.}
  \item{fitted:}{fitted (squared) GEOVOL time series.}
  \item{logLik:}{log-likelihood (normal density).}
  \item{nobs:}{the number of observations used in the estimation.}
  \item{print:}{print of the estimation results.}
  \item{residuals:}{GEOVOL standardised residuals.}
}

\references{
Engle, R.F. and Campos-Martins, S. (2020) Measuring and hedging geopolitical risk. Available at \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3685213}. 
}

\author{Susana Campos-Martins}

\seealso{\code{\link{geovol}}, \code{\link{geovolSim}}, \code{\link{zoo}}}

\examples{
set.seed(123)

## Simulate from a GEOVOL model (default):
eSim <- geovolSim(n = 1000,  m = 30)

## Test for GEOVOL
eTest <- geovolTest(e = eSim^2)

## Estimate a GEOVOL model:
geovolEst <- geovol(e = eSim)

## Print estimation results:
print(geovolEst)

## Extract coefficients:
coef(geovolEst)

## Extract and store GEOVOL:
sigma2Est <- fitted(geovolEst)

## Extract optimised log-likelihood values:
logLik(geovolEst)

## Extract and store GEOVOL standardised residuals:
etaEst <- residuals(geovolEst)
}

\keyword{Volatility Factor Models}
\keyword{Multivariate Time Series}
\keyword{Econometrics}
\keyword{Financial Econometrics}