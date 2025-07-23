\name{region2SpatialPolygon}
\alias{region2SpatialPolygon}
\alias{extent2SpatialPolygon}
\title{
creates polygon from raster extent
}
\description{
creates polygon from raster extent
}
\usage{
extent2SpatialPolygon(ext)
region2SpatialPolygon(region,v_area=region)

}
\arguments{
  \item{ext}{
  raster extent or vector of region limits (xmax, xmax, ymin, ymax)
}
 \item{region, v_area}{
 region identifier as defined in the region_definitions file.
}
}

\author{
Robert K. Bauer
}

\keyword{internal}

\examples{
r <- regions("lion")
ext <- extent(c(r$xlim,r$ylim))
extent2SpatialPolygon(ext)

region2SpatialPolygon("lion")
}