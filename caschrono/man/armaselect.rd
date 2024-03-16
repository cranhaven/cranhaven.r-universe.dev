\name{armaselect}
\alias{armaselect}
\title{Minic method}
\description{\code{armaselect} implements  the MINIC  (Minimum Information Criterion) 
identification method and returns the nbmod best ARMA models, with respect to
the Schwarz's Bayesian Criterion (sbc).}
\usage{
armaselect(y, max.p = 15, max.q = 15, nbmod = 10)
}
\arguments{
 \item{y}{a time series}
 \item{max.p}{an integer, the maximum value for the autoregressive component, p}
 \item{max.q}{an integer, the maximum value for the moving average component, q}
 \item{nbmod}{an integer, the number of models that will be returned (\code{nbmod} may be 
 lower than \code{max.p} x \code{max.q}).}
 }

\value{
A matrix with \code{nbmod} rows and 3 columns (values of \code{p}, \code{q} and \code{sbc})
}

\author{Yves Aragon}

\examples{
set.seed(4123)
n2 <- 210
yc <- arima.sim(n = 200, list(ar = -0.8, ma = c(-0.3, 0.6)),
 sd = sqrt(1.5))
yc <- yc - 10
armaselect(yc, nbmod = 5) 
}
\keyword{ts}
