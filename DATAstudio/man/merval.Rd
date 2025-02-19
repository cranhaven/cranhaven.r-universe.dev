\encoding{UTF-8}
\name{merval}
\alias{merval}
\title{MERVAL Stock Market Data}
\description{
  Raw interval data series corresponding to weekly minimum and maximum
  values of the MERVAL index (Argentina stock market) ranging from
  January 1 2016 to September 30 2020, along with prices at open
  and prices at close. 
}
\usage{merval}
\format{
  A dataframe with 353 observations and 5 columns: \code{dates},
  \code{low}, \code{high}, \code{open}, and \code{close}.}
\references{
  de Carvalho, M. and Martos, G. (2022). Modeling interval trendlines:
  Symbolic singular spectrum analysis for interval time series.
  \emph{Journal of Forecasting}, \bold{41}, 167-180.
}
\source{
  Yahoo Finance. 
}	
\examples{
data(merval)
attach(merval)
head(merval, 3)
oldpar <- par(pty = 's')
plot(low, high, pch = 20)
abline(a = 0, b = 1, lty = 2, col = "gray")
par(oldpar)
}
\keyword{Financial}
