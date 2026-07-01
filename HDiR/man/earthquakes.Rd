\encoding{utf8}
\name{earthquakes}
\alias{earthquakes}
\docType{data}
\title{
Earthquakes on Earth between October 2004 and April 2020
}
\description{
Geographical coordinates (latitude and longitude) of earthquakes of magnitude greater than or equal to 2.5 degrees.
}
\usage{data("earthquakes")}
\format{
  A data frame with 272 observations on the following 2 variables:
  \describe{
    \item{\code{Latitude}}{A numeric vector containing the latitude coordinates.}
    \item{\code{Longitude}}{A numeric vector containing the longitude coordinates.}
  }
}

\details{
To map this dataset on the unit sphere, function \code{euclid} in package \code{Directional} can be used.

}

\source{
European-Mediterranean Seismological Centre, \url{https://www.emsc-csem.org}.}

\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
library(ggplot2)
}
if (requireNamespace("maps", quietly = TRUE)) {
library(maps)
}
if (requireNamespace("mapproj", quietly = TRUE)) {
library(mapproj)
}
data(earthquakes)
world <- map_data("world")
g.earthquakes <- ggplot() +
  geom_map(data = world, map = world,
           mapping = aes(map_id = region),
           color = "grey90", fill = "grey80") +
  geom_point(data = earthquakes,
            mapping = aes(x = Longitude, y = Latitude),
            color = "red",alpha=.2,size=.75,stroke=0) +
    scale_y_continuous(breaks = NULL, limits = c(-90, 90)) +
  scale_x_continuous(breaks = NULL, limits = c(-180, 180)) +
  coord_map("mercator")
g.earthquakes
}
\keyword{datasets}
