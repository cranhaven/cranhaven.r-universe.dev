\name{PM10trunc}
\alias{PM10trunc}
\docType{data}
\title{
Air pollution data (Truncated)
}
\description{
Dataset \code{\link{PM10}}, truncated from the left at variable value PM10 = 2 (8 percent truncation).
}
\usage{data(PM10trunc)}
\format{
  A data frame with 460 observations on the following 8 variables.
  \describe{
    \item{\code{PM10}}{Hourly values of the logarithm of the concentration of PM10 (particles). Left-truncated at point 2.}
    \item{\code{cars}}{The logarithm of the number of cars per hour}
    \item{\code{temp}}{Temperature 2 meters above ground (degree C)}
    \item{\code{wind.speed}}{Wind speed (meters/second)}
    \item{\code{temp.diff}}{The temperature difference between 25 and 2 meters above ground (degree C)}
    \item{\code{wind.dir}}{Wind direction (degrees between 0 and 360)}
    \item{\code{hour}}{Hour of day}
    \item{\code{day}}{Day number from October 1. 2001}
  }
}
\examples{
data(PM10trunc)
}
\keyword{datasets}
