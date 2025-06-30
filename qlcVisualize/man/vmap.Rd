\name{vmap}
\alias{vmap}
\alias{voronoimap}
\alias{voronoi}
\title{
Plotting a Voronoi-map ("v-map")
}
\description{
These functions are deprecated: use \link{weightedMap} instead.

A Voronoi-map (voronoi-tessellation, also known as dirichlet tessellation) is used in quantitative dialectology. This function is a convenience wrapper to easily produce dialect maps with voronoi tessellations. Also described here are a helper functions to produce the tessellation.
}
\usage{
vmap(tessellation, col = NULL, add = FALSE,
  outer.border = "black", border = "grey", lwd = 1, ...)

voronoi(points, window)
}
\arguments{
  \item{tessellation}{
Tessellation of class \code{\link[spatstat.geom]{tess}} from the library \code{spatstat.geom}. Can easily be produces by using the convenience function \code{voronoi} provided here.
}
  \item{col}{
Vector of colors for the filling of the tessellation. Is recycled when there are more tiles than colours. The order of the tiles is the same as the order of the points as specified in the function \code{\link{voronoi}}.
}
  \item{add}{
Add graphics to an existing plot
}
  \item{outer.border}{
Colour of the outer border. Specifying \code{NA} removes the border.
}
  \item{border}{
Colour of the inner borders. Specifying \code{NA} removes all borders.
}
  \item{lwd}{
Line width of borders.
  }
  \item{...}{
Further arguments passed to \link{polygon}.
}
  \item{points}{
Two-column matrix with all coordinates of the points to make a Voronoi tessellation.
  }
  \item{window}{
Outer boundary for the Voronoi tessellation. Should be in the form of an \code{\link[spatstat.geom]{owin}} object. There are two helper functions provided here to get such object. Note that the function \code{\link{voronoi}} will give warnings if there are points outside of this window.
  }
}
\details{
This code is almost completely based on functions from the \code{spatstat.geom} package. For convenience, first some geographical boundaries can easily be accessed and converted for use in \code{spatstat.geom}. Then a Voronoi tessellation can be made (based on the function \code{\link[spatstat.geom]{dirichlet}}, which in turn is based on \code{deldir} from the package \code{deldir}). Finally, this tessellation can be plotted filled with different colours.

Any legends have to be added manually by using \code{\link[graphics]{legend}}, see examples below.

The function \code{voronoi} returns a warning when points are attested that lie outside of the specified border. For these points there is no polygon specified. Indices for the rejected points outside the border can be accessed by \code{attr(x, "rejects")}.
}
\value{
\code{voronoi} returns a tessellation of the class \code{tess} from the package \code{spatstat.geom}. When points outside of the border are attested, the indices of these points are added to an attribute "rejects".
\code{vmap} plots a map.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
\dontrun{
# make a Voronoi tessellation for some villages in hessen
data(hessen)
plot(hessen$boundary)
points(hessen$villages, cex = 0.3)

tessellation <- voronoi(hessen$villages, hessen$boundary)
plot(tessellation)

# make a resizable plot with random colour specification
vmap(tessellation, col = rainbow(5), border = NA)
legend("bottomright", legend = c("a","b","c","d","e"), fill = rainbow(5))

# use actual colors from data, using first feature from supplied data
# multiple levels cannot easily be shown
# consider \link{lmap} for more detail
d1 <- hessen$data[,1:3]
d1 <- d1[,1]/rowSums(d1)
vmap(tessellation, col = rgb(1, 1-d1, 1-d1))
text(hessen$villages,labels=hessen$data[,1],cex=.5)
legend("bottomright", legend = c("es mir", "mir es / other"),
  fill = c("red", "white"))

# Use distances to determine colour, as proposed by Heeringa (2004)
# Note that different methods to establish distances can lead to rather
# different results! Also try method = "euclidean"
d <- dist(hessen$data, method = "canberra")
cols <- heeringa(d)
vmap(tessellation, col = cols, border = NA)
}
}
