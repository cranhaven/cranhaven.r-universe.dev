\name{SpatialCircle}
\alias{SpatialCircle}
\title{
Creates a circle of radius r around a specified point.
}

\description{
Returns a SpatialLines object, that defines a circle of radius r around a specified point.
}

\usage{
SpatialCircle(x,y,r,n=100,proj4str)
}

\arguments{
\item{x, y}{
x and y coordinates of the circle
}

 \item{r}{
 radius (of the same scale as plotting window)
 }

  \item{n}{
precision indicator of the circle.
}

  \item{proj4str}{
  projection string.
}

}
\seealso{\link{get.avg.bathy}} 

\author{
Robert K. Bauer
}

\keyword{bathymetry}
\keyword{circle}
\keyword{data extraction}


\examples{

## Example 1: load & plot bathymetry of the Baltic Sea, defined by longitudes and latidtues
lon <- c(9, 31)
lat <- c(53.5, 66)
# bathy <- get.bathy(lon=lon, lat=lat, main="Baltic Sea", cbpos='r')
plotmap(lon=lon, lat=lat)
spc <- SpatialCircle(x= 20,y = 57.5,r=1)
plot(spc,add=TRUE)
# get.avg.bathy(c(20,57.5), radius = 1, bathy = bathy)
# get.avg(c(20,57.5), radius = 1, unit="km",raster = bathy)

}
