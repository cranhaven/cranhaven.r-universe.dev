\name{PM10}
\alias{PM10}
\docType{data}
\title{
Air pollution data
}
\description{
The data are a subsample of 500 observations from a data set that originates in a study where air pollution at a road is related to traffic volume and meteorological variables, collected by the Norwegian Public Roads Administration. The response variable consists of hourly values of the logarithm of the concentration of PM10 (particles), measured at Alnabru in Oslo, Norway, between October 2001 and August 2003. (Source: Statlib)
}
\usage{data(PM10)}
\format{
  A data frame with 500 observations on the following 8 variables.
  \describe{
    \item{\code{PM10}}{Hourly values of the logarithm of the concentration of PM10 (particles)}
    \item{\code{cars}}{The logarithm of the number of cars per hour}
    \item{\code{temp}}{Temperature 2 meters above ground (degree C)}
    \item{\code{wind.speed}}{Wind speed (meters/second)}
    \item{\code{temp.diff}}{The temperature difference between 25 and 2 meters above ground (degree C)}
    \item{\code{wind.dir}}{Wind direction (degrees between 0 and 360)}
    \item{\code{hour}}{Hour of day}
    \item{\code{day}}{Day number from October 1. 2001}
  }
}
\source{
\url{http://lib.stat.cmu.edu/}, dataset PM10, submitted by Magne Aldrin on July 28, 2004
}
\references{
Aldrin, M. (2006) Improved predictions penalizing both slope and curvature in additive models, \emph{Computational Statistics & Data Analysis}, \bold{50}, pp 267--284
}
\examples{
data(PM10)
}
\keyword{datasets}
