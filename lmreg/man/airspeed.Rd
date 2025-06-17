\name{airspeed}
\alias{airspeed}
\docType{data}
\title{
Air speed experiment data
}
\description{
Air speed data, which is part of a larger data set from a designed experiment (Wilkie, 1962).
}
\usage{data(airspeed)}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{Posmaxspeed}}{The position of highest speed of air
blown down the space between a roughened rod and a smoothed pipe
surrounding it. The position is defined as the distance (in inches) from the center of the rod, in excess of 1.4 inches}
    \item{\code{Reynolds}}{Reynolds number of air flow (dimensionless)}
    \item{\code{Ribht}}{Height of ribs on the roughened rod (in inches)}
  }
}
\source{
Wilkie, D. (1962) A method of analysis of mixed level factorial experiments. Applied Statistics, pp.184-195.
}
\examples{
data(airspeed)
head(airspeed)
}
\keyword{datasets}
