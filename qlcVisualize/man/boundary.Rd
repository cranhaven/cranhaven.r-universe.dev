\name{boundary}
\alias{boundary}
\title{
Checking boundary parameters for plotting of \code{levelMap}
}
\description{
The function \code{\link{levelMap}} can be tweaked by various parameters determining the boundary of the interpolation. The function \code{boundary} helps finding suitable parameters.
}
\usage{
boundary(points, density = 0.02, grid = 10, box.offset = 0.1
  , tightness = "auto", manual = NULL, plot = TRUE)
}
\arguments{
  \item{points}{
Points, typically a two-column matrix with x and y coordinates.
}
  \item{density}{
Density of points below which there should be no interpolation.
}
  \item{grid}{
Density of the grid.
}
  \item{box.offset}{
Distance of the box around the points.
}
  \item{tightness}{
Parameter influencing how tightly the boundary should be wrapped around the points. Passed internally to \link[MASS]{kde2d}. When \code{"auto"} this defaults to \link[MASS]{bandwidth.nrd}. Lower values will result in tighter boundaries.
  }
  \item{manual}{
Manually added boundary points in the form of a two-column matrix with coordinates.
  }
  \item{plot}{
Logical: by default the impact of the chosen parameters is shown. If \code{FALSE} then coordinates are returned that are the outside of the boundary.
}
}
\details{
Instead of trying to use a polygon as a boundary for the interpolation internally in \code{\link{levelMap}} it turned out to be easier to use a collection of points that mark the outside.
}
\value{
By default, returns a plot with the original points in black, the points below density in red, and the box around the points in blue. Contour lines of the density are shown to choose different density parameters.

When \code{plot = FALSE}, the blue and red points from the graphic are returned as a two-column matrix of x and y coordinates.
}

\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
Used internally in \code{\link{levelMap}}. The parameters of this function can be passed through, typically \code{density} and \code{box.offset}.
}
\examples{
data(hessen)

# show impact of the chosen parameters
boundary(hessen$villages, density = 0.1, grid = 20
  , manual = cbind(x = c(8.3, 9.2), y = c(49.9, 50.0)))

# return coordinates
boundary(hessen$villages, plot = FALSE)

# abstract example, showing tightness in action
oldpar<-par("mfrow")
par(mfrow = c(1,3))

p <- cbind(c(1:10, 1:10), c(1:10, 10:1))
boundary(p, density = 0.005, grid = 20, tightness = "auto")
boundary(p, density = 0.005, grid = 20, tightness = 5)
boundary(p, density = 0.005, grid = 20, tightness = 3)

par(mfrow = oldpar)

}
