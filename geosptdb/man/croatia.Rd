\name{croatia}
\alias{croatia}
\docType{data}
\title{
Map Croatia.
}
\description{
Map Croatia. Spatial reference system: UTM 33N
}
\usage{data(croatia)}
\format{
  The format is:
Formal class 'SpatialPolygonsDataFrame' [package "sp"] 
}
\references{  
Hengl, T. (2009). \emph{A Practical Guide to Geostatistical Mapping}, 2nd edn, University of Amsterdam, Amsterdam.
}
\examples{
data(croatia)
pts <- spsample(croatia, n=25000, type="regular")
plot(pts)
}
\keyword{datasets}
