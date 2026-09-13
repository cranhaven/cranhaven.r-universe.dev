\name{MAmap}
\alias{MAmap}
\docType{data}
\title{
Map of Massachusetts
}
\description{
A map of the outline of MA in SpatialPolygonsDataFrame format, converted from an ESRI shapefile using the \code{readShapePoly} function in the \pkg{maptools} package.}
\usage{data(MAmap)}
\format{
  The format is class SpatialPolygonsDataFrame (package "sp").
}
\details{
Lambert conformal conic projection for the State of Massachusetts, using standard parallels 41.71666667 and 42.68333333.  The latitude of origin is 41.0, the central meridian is -71.5, and the projection units are meters (False Easting: 200000 m; False Northing: 750000 m).   
}
\source{
Dr. Veronica Vieira, University of California, Irvine
}
\examples{
data(MAmap)
plot(MAmap)
}
\keyword{datasets}
