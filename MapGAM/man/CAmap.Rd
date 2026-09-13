\name{CAmap}
\alias{CAmap}
\docType{data}
\title{
Map of California
}
\description{
A map of the outline of CA in SpatialPolygonsDataFrame format, converted from an ESRI shapefile using the \code{readShapePoly} function in the \pkg{maptools} package.}
\usage{data(CAmap)}
\format{
  The format is class SpatialPolygonsDataFrame (package "sp").
}
\details{
State Plane Projected coordinate system, North America Datum 1983 (NAD_1983_StatePlane_California_I_FIPS_0401) for California, using standard parallels 40.00000000 and 41.66666667.  The latitude of origin is 39.3, the central meridian is -122.0 and the projection units are meters (False Easting: 200000 m; False Northing: 500000 m).   
}
\source{
Dr. Veronica Vieira, University of California, Irvine
}
\examples{
data(CAmap)
plot(CAmap)
}
\keyword{datasets}
