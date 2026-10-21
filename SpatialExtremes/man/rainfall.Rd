\name{rainfall}
\docType{data}
\alias{rainfall}
\title{Summer annual maxima precipitation in Switzerland}
\usage{data(rainfall)}
\description{
  Maximum daily rainfall amounts over the years 1962--2008 occuring
  during June--August at 79 sites in Switzerland.
  }
\format{This data set contains two R objects: 'rain' and 'coord'. 'rain'
  is a 47 by 79 matrix giving the amount of rain in millimeters, each
  column correspond to one locations. 'coord' is a 79 by 3 matrix giving
  the longitude, latitude and the elevation for each station, all of
  them being in meters.}
\examples{
data(rainfall)
op <- par(mfrow = c(1,2),pty = "s", mar = c(0,0,0,0))
swiss(city = TRUE)
idx.site <- c(1, 10, 20)
points(coord[-idx.site,])
points(coord[idx.site,], pch = 15, col = 2:4)

par(mar = c(2,4,0,0))
plot(1962:2008, rain[,1], type = "b", xlab = "Year", ylab =
"Precipitation (cm)", ylim = c(0, 120), col = 2)
lines(1962:2008, rain[,10], col = 3, type = "b")
lines(1962:2008, rain[,20], col = 4, type = "b")
par(op)
}
\author{Mathieu Ribatet}
\keyword{datasets}
