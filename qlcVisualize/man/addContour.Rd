\name{addContour}
\alias{addContour}
\title{
Add contourlines to a map
}
\description{
Convenience function to add contourlines to a map, specifically geared towards suggesting boundaries to the result of \link{weightedMap}. Internally based on a krige-interpolation.
}
\usage{
addContour(heights, points, window, crs,
           levels = c(0.4, 0.45, 0.5), grid = 50000, ...)
}
\arguments{
  \item{heights}{
Numeric vector with the same length as \code{points}. Typically a 0/1 vector describing presence or absence of a features.
}
  \item{points}{
Locations of the datapoints as \code{sfc_POINTS} or an sf object with such a geometry.
}
  \item{window}{
Window for the interpolation as \code{sfc_POLYGON} or an sf object with such a geometry.
}
  \item{crs}{
A crs in WKT format.
}
  \item{levels}{
Levels on which to draw the contourlines. Multiple lines get thicker towards higher values to suggest a center. These levels have to be related relative to the \code{heights}.
}
  \item{grid}{
Number of points inside the window for the krige-interpolation. Higher numbers lead to nicers contourlines, but take longer to evaluate.
}
  \item{...}{
Additional specifications passed internally to \code{\link{contour}}.
}
}
\details{
Internally, a grid is made inside the window and the height is interpolated using ordinary kriging from \code{[gstat]{krige}} with a model suggested by \code{\link[automap]{autofitVariogram}}.
}
\value{
Contourlines are added to the current plot.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\note{
This is a preliminary convenience function that will be used to overhaul \code{\link{levelMap}}
}
\seealso{
\code{\link{weightedMap}} for more involved example
}
\examples{
data(hessen)

# continuous variable between 0 and 1
data <- hessen$data[,1:3]
heights <- round(data[,1]/rowSums(data), digits = 1)
cols <- heat.colors(11)
names(cols) <- names(table(heights))

# boundary as sf
w <- sf::st_as_sf(hessen$boundary)
sf::st_crs(w) <- 4326
w <- sf::st_transform(w, 2397)

# points as sf
p <- sf::st_as_sf(hessen$villages, coords = c("longitude", "latitude"))
sf::st_crs(p) <- 4326
p <- sf::st_transform(p, 2397)

# plot map
plot(p, col = cols[as.character(heights)], border = NA, pch = 19)
plot(w, add = TRUE, border = "grey")

# add boundary
addContour(heights, points = p, window = w, crs = 2397, grid = 1000,
            levels = c(0.25, 0.35, 0.45), col = "blue")
}
