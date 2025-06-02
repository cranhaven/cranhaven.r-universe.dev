\name{ariari}
\alias{ariari}
\docType{data}
\title{
Ariari Map.
}
\description{
Map Basin Map. Spatial reference system: UTM 18S
}
\usage{data(ariari)}
\format{
  The format is:
Formal class 'SpatialPolygonsDataFrame' [package "sp"] 
}
\examples{
data(ariari)
pts <- spsample(ariari, n=25000, type="regular")
plot(pts)
}
\keyword{datasets}
