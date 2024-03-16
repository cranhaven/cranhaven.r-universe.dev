\name{plotacfthemp}
\alias{plotacfthemp}
\title{Plots the ACF and PACF of a theoretical ARMA model and the empirical ACF and PACF of an observed series}
\description{\code{plotacfthemp} plots the ACF and PACF of a theoretical ARMA model and the empirical ACF and PACF of an observed series.}
\usage{
plotacfthemp(y, ar = numeric(0), ma = numeric(0), lag.max = 20, titre = "")
}
\arguments{
 \item{y}{time series, a \code{ts} object}
 \item{ar}{numeric vector of AR coefficients}
 \item{ma}{numeric vector of MA coefficients}
 \item{lag.max}{integer, Maximum lag required.}
 \item{titre}{a string of characters for the title}
 }
\details{This function uses the \code{ARMAacf} and \code{acf} functions to 
compute theoritical and empirical ACF and PACF}
\value{
No values
}

\author{Yves Aragon and Thibault Laurent}

\examples{
set.seed(951)
ya <- arima.sim(n=200, list(ma = c(-0.3, 0.6)), 
sd = sqrt(1.5))
plotacfthemp(ya, ma=c(-0.3,0.6), titre="MA(2)") 
}
\keyword{ts}
