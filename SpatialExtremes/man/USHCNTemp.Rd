\name{USHCNTemp}
\docType{data}
\alias{USHCNTemp}
\title{Summer/Winter annual maxima/minima temperature in continental US.}
\usage{data(USHCNTemp)}
\description{
  Summer maxima/Winter minima temperatures over the years 1911--2010
  observed at 424 weather stations located in continental USA.
  }
\format{This data set contains three R objects: 'maxima.summer',
  'minima.winter' and 'metadata'. 'maxima.summer' is a 100 by 424 matrix
  giving the temperature in degrees, each column correspond to one
  location. 'minima.winter is a 99 by 424 matrix giving the temperature
  in degrees, each column correspond to one location. 'metadata' is a
  424 by 5 data frame giving station identifier, the longitude,
  latitude, elevation and the state for each station.}
\examples{
data(USHCNTemp)
##require(maps) ## <<-- to plot US borders
maps::map("usa")
plot(metadata[,2:3], pch = 15)
}
\author{Mathieu Ribatet}
\keyword{datasets}
