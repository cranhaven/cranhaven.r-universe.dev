\name{get.avg.bathy}
\alias{get.avg}
\alias{get.avg.bathy}


\title{
returns the average value of circles with specified coordinates and a defined radius
}

\description{
returns the average value of circles with specified coordinates and a defined radius
}

\usage{
get.avg.bathy(x, radius, unit="km", raster, bathy, v_area="medw4")
get.avg(x, radius, unit="km", raster)

}


\arguments{
\item{x}{
a vector or matrix providing the coordinates of the circle
}

 \item{radius}{
 the radius (if \code{unit} != "km", 
 the radius is assumed to be of the same scale as the plotting window)
 }

  \item{unit}{
  unit of the radius (by default "km").
  if \code{unit} != "km", the radius is assumed to be of the same scale as the plotting window.
}

  \item{raster}{
  raster object, from which the average should be calculated.
}

\item{bathy}{
  raster object with bathymetry data, from which the average should be calculated.
}

\item{v_area}{
character string identifying the region for which the bathymetry data should downloaded from the NOAA server.
}

}
\seealso{\link{SpatialCircle}} 

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
