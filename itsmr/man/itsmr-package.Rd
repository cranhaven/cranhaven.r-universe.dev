\name{itsmr-package}
\Rdversion{1.1}
\alias{itsmr-package}
\alias{itsmr}
\docType{package}

\title{
Time Series Analysis Using the Innovations Algorithm
}

\description{
Provides functions for modeling and forecasting time series data.
Forecasting is based on the innovations algorithm.
A description of the innovations algorithm can be found in the textbook
\emph{Introduction to Time Series and Forecasting}
by Peter J. Brockwell and Richard A. Davis.
}

\details{
\tabular{ll}{
Package: \tab itsmr\cr
Type: \tab Package\cr
Version: \tab 1.10\cr
Date: \tab 2022-07-27\cr
License: \tab FreeBSD\cr
LazyLoad: \tab yes\cr
URL: \tab \url{https://georgeweigt.github.io/itsmr-refman.pdf}\cr
}
}

\author{
George Weigt\cr
Maintainer: George Weigt <g808391@icloud.com>
}

\references{
Brockwell, Peter J., and Richard A. Davis.
\emph{Introduction to Time Series and Forecasting.}
2nd ed. Springer, 2002.
}

\keyword{ package }

\examples{
plotc(wine)

## Define a suitable data model
M = c("log","season",12,"trend",1)

## Obtain residuals and check for stationarity
e = Resid(wine,M)
test(e)

## Define a suitable ARMA model
a = arma(e,p=1,q=1)

## Obtain residuals and check for white noise
ee = Resid(wine,M,a)
test(ee)

## Forecast future values
forecast(wine,M,a)
}
