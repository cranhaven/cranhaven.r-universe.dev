\name{findBoundaryLines}
\alias{findBoundaryLines}
\title{ Finding the regional boundaries
}
\description{ Method for identifying points on the boundaries between regions 
             (in most cases biases between countries)}
\usage{
findBoundaryLines(polygons, projOrig, projNew, regCode = "regCode")
}
\arguments{
\item{polygons}{A \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} with the polygons
               defining the boundaries of each separate region.}
\item{projOrig}{The original projection of the boundaries}
\item{projNew}{If a different projection is wanted for the output}
\item{regCode}{the column name of regions in the data polygons}
}


\details{
This function finds the points defining the boundary between two polygons and 
passes a \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} with these points back.
The result in mainly used by \code{\link{findRegionalBias}} for estimation
of regional biases. The function is based on the boundary between the 
polygons being defined by the same points. 
}

\value{
A \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} with points defining the
              boundaries between regions. 
}

\references{

Skoien, J. O., O. P. Baume, E. J. Pebesma, and G. B. M. Heuvelink. 2010. 
Identifying and removing heterogeneities between monitoring networks. Environmetrics 21(1), 66-84.

}
\author{ Jon Olav Skoien}
\examples{
data(meuse)
observations = data.frame(x = meuse$x,y = meuse$y,value = log(meuse$zinc))
coordinates(observations) = ~x+y
pBoundaries = spsample(observations, 10, "regular", bb = bbox(observations) +  
                         matrix(c(-400,-400,400,400),ncol=2),offset=c(0,0))
gridded(pBoundaries) = TRUE
cs = pBoundaries@grid@cellsize[1]/2

Srl = list()
nb = dim(coordinates(pBoundaries))[1]
for (i in 1:nb) {
  pt1 = coordinates(pBoundaries)[i,]
  x1 = pt1[1]-cs
  x2 = pt1[1]+cs
  y1 = pt1[2]-cs
  y2 = pt1[2]+cs

  boun = data.frame(x=c(x1,x2,x2,x1,x1),y=c(y1,y1,y2,y2,y1))
  coordinates(boun) = ~x+y
  boun = Polygon(boun)
  Srl[[i]] = Polygons(list(boun),ID = as.character(i))
}
pBoundaries = SpatialPolygonsDataFrame(SpatialPolygons(Srl),
                                       data = data.frame(ID=c(1:nb)))
observations$ID = over(observations, geometry(pBoundaries))
blines = findBoundaryLines(pBoundaries, regCode = "ID")
}
\keyword{spatial}
