\name{world}
\alias{world}
\docType{data}
\title{
Boundary of the World fitting all Glottolog languages
}
\description{
A polygon representing a worldmap tailored to be not too detailed, but still fitting all glottolog languages inside the polygons.
}
\usage{data("world")}
\format{
  The format is a \code{sfc_POLYGON} of length 475 with an EPSG:4326 projection.
}
\details{
Some trickery was needed to produce a lightweight polygon to represent the worldmap in reasonably accuracy without becoming too large and unwiedly. The polygons are such that all coordinates for languages as listed in the glottolog (version 5) are inside these polygons.

The map has a basic EPSG:4326 projection, so longitude-latitude coordinates can immediately be added to it. However, this does not look very nice, because the polygon from Eurasia wraps around. Consider more suitable projections, see examples. To allow for a nice pacific-centered projection Greenland has been clipped.
}
\source{
Polygons are based on the data from \url{https://www.naturalearthdata.com} with adjustments. Glottolog coordinates to select and adjust the polygons are from \url{https://glottolog.org}.
}
\examples{
data(world)
plot(world)

# use different projections
azimuth_equaldist <- "+proj=aeqd +lat_0=90 +lon_0=45"
mollweide_atlantic <- "+proj=moll +lon_0=11.5"
mollweide_pacific <- "+proj=moll +lon_0=151"

plot(sf::st_transform(world, crs = azimuth_equaldist))
}
\keyword{datasets}
