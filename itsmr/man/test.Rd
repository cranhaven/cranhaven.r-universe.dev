\name{test}
\Rdversion{1.1}
\alias{test}

\title{
Test residuals for stationarity and randomness
}

\description{
Test residuals for stationarity and randomness
}

\usage{
test(e)
}

\arguments{
\item{e}{Time series data (typically residuals from \code{Resid})}
}

\details{
Plots ACF, PACF, residuals, and QQ.
Displays results for Ljung-Box, McLeod-Li, turning point,
difference-sign, and rank tests.
The plots can be used to check for stationarity and the
other tests check for white noise.
}

\value{
None
}

\seealso{
\code{\link{Resid}}
}

\examples{
M = c("log","season",12,"trend",1)
e = Resid(wine,M)
test(e) ## Is e stationary?
a = arma(e,1,1)
ee = Resid(wine,M,a)
test(ee) ## Is ee white noise?
}
