\name{DataAvailabilityIndex}
\alias{DataAvailabilityIndex}
\title{
A function for showing sampling point density map in a geographic area
}
\description{
An index map of density of sampling points in a geographic area
}
\usage{
DataAvailabilityIndex(Boundary, Scale, CP, Data)
}
\arguments{
  \item{Boundary}{
a spatial polygon or data frame of coordinates of vertices of a bounding geographic area where data search is intended
}
 \item{Scale}{
unit area to show spatial density of available sampled points
}
 \item{CP}{
coordinate projection of the Boundary spatial polygon
}
 \item{Data}{
input spatial spreadsheet database containing all possible point samples
}
}
\details{
The input spreadsheet database should contain spatial coordinates of available samples. Example input spatial spreadsheet database is the global soil database.The Scale should be provided in area units e.g., 0.5, 1, 20, 30 (square km). Large areas cover more data than small areas. Hence, they take time to process. Coordinate projection (CP) for Boundary polygon should be of formal class crs (coordinate reference system). It's preferrable to provide CP for Boundary area similar to CP for input data
}
\value{
A spatial raster map of density of sample locations per unit (specified) area
}
\author{
Christian Thine Omuto
}
\note{
Scales less than 0.1 square km may be too small for search. Large scales (say 10000 square km) may be too large and take time to process
}
\examples{
\donttest{
library(sp)
library(raster)
library(terra)
library(sf)
x <- c(20.02,25.69,25.69,20.02)
y <- c(-28.40,-32.76,-32.76,-34.84)
yx=data.frame(cbind(x, y))
CRs="+proj=longlat +datum=WGS84 +no_defs"
Data=SASglobeData("ph","ZAF")
coordinates(Data)=~Longitude+Latitude
crs(Data)=CRs
Index=DataAvailabilityIndex(yx,60,CRs,Data)
plot(Index)
}
}
