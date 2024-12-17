\name{acc}
\alias{acc}
\title{Autocorrelation Coefficient}
\usage{
acc(x, lag = 1)
}
\arguments{
\item{x}{a vector, usually residuals.}

\item{lag}{lag for which the autocorrelation should be calculated.}
}
\value{
Autocorrelation coefficient of lag k, numeric value.
}
\description{
Calculates the autocorrelation coefficient between a vector and its k-period lag.
This can be used as an estimator for rho in an AR(1) process.
}
\examples{
## Simulate AR(1) Process with 30 observations and positive autocorrelation
X <- ar1sim(n = 30, u0 = 2.0, rho = 0.7, var.e = 0.1)
acc(X$u.sim, lag = 1)

## Equivalent result using acf (stats)
acf(X$u.sim, lag.max = 1, plot = FALSE)$acf[2]

}
\references{
NIST/SEMATECH e-Handbook of Statistical Methods,
\url{http://www.itl.nist.gov/div898/handbook/eda/section3/eda35c.htm}.
}
\seealso{
\code{\link{lagk}}, \code{\link[stats]{acf}}.
}
\concept{AR1}
\concept{autocorrelation}
\concept{autoregressive model}
\concept{lagged variables}
