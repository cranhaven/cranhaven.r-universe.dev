\name{wind}
\docType{data}
\alias{wind}
\title{Annual maxima wind gusts in the Netherlands.}
\usage{data(wind)}
\description{
  Annual maxima wind gusts (km/h) in the Netherlands recorded at 35
  weather stations over the period 1971--2012.}
\format{This data set contains two R objects: 'coord' and 'wind'. 'wind'
  is a 42 by 35 matrix giving the wind gust speeds in km/h---with
  missing values, each column correspond to one location. 'coord' is a
  35 by 3 matrix giving the longitude (degree), latitude (degree) and
  elevation (m).}
\examples{
##require(maps)
data(wind)
par(mar = rep(0, 4))
maps::map(xlim = c(0, 9), ylim = c(47.5, 57.5))
points(coord[,1:2], pch = 15)
}
\author{Mathieu Ribatet}
\keyword{datasets}
