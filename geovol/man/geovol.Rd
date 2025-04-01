\name{geovol}
\alias{geovol}

\title{Estimate GEOVOL and GEOVOL loadings}
 
\description{ 
Maximum likehlihood estimation of the GEOVOL and GEOVOL loadings. Estimation is carried out as a maximization-maximization procedure, where the latent multiplicative volatility factor and the factor loadings are estimated iteratively until convergence. Either a balanced or an unbalanced panel can be used. For each point in time t, GEOVOL is estimated using all the available values in the cross-section. The GEOVOL loadings are estimated for each time series. 
}

\usage{
  geovol(e, initial.values = list()) 
}

\arguments{
\item{e}{matrix, multivariate time series or \code{\link{zoo}} object of volatility standardized residuals with mean zero and unit variance.}
\item{initial.values}{a list containing the initial parameter values passed on to the optimization routine (\link{optim} for the GEOVOL factor and loadings. If list(), the default, then the values are chosen automatically; \code{x} - \code{NULL} or numeric vector containing the \code{n} initial values for the GEOVOL factor, \code{x_{t}, t=1,\dots,n}. \code{s} - \code{NULL} or numeric vector containing the \code{m} initial values for the GEOVOL loadings, \code{s_{i}, i=1,\dots,m}.}
}

\value{An object of class 'geovol'.}

\references{
Engle, R.F. and Campos-Martins, S. (2020) Measuring and hedging geopolitical risk. Available at \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3685213}. 
}

\author{Susana Campos-Martins}

\seealso{
\code{\link{geovol}},
\code{\link{geovolSim}},
\code{\link{geovolTest}},
\code{\link{optim}},
}

\examples{
set.seed(123)

## Simulate from a GEOVOL model (default):
eSim <- geovolSim(n = 1000,  m = 30)

## Test for GEOVOL
geovolTest(e = eSim^2-1)

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
