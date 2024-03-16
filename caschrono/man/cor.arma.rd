\name{cor.arma}
\alias{cor.arma}
\title{Correlation matrix of the parameters for an Arima model}
\description{Computes the correlation matrix for the estimated parameters of an Arima model.}
\usage{
cor.arma(mod)
}
\arguments{
 \item{mod}{an Arima object}
 }
\value{
A p x p matrix  (p, the number of parameters of the ARIMA model)
}

\author{Yves Aragon}

\examples{
set.seed(4123)
n2 <- 210
yc <- arima.sim(n = 200, list(ar = -0.8, ma= c(-0.3, 0.6)),
 sd = sqrt(1.5))                               
yc <- yc - 10
if(require("forecast")){
fit <- Arima(yc, order = c(1, 0, 2))
cor.arma(fit)
}
}
\keyword{ts}
