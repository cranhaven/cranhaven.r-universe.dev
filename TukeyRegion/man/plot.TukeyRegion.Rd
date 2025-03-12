\name{plot.TukeyRegion}
\alias{plot.TukeyRegion}
\title{Plot the Tukey Region}
\usage{
\method{plot}{TukeyRegion}(x, newPlot = TRUE, drawPoints = TRUE, 
  drawRidges = TRUE, colorBackground = "white", 
  colorPoints = "red", colorFacets = "blue", 
  colorRidges = "green", lwd = 1, lty = 1, alpha = 1, ...)
}
\description{
Plots the two-dimensional and the three-dimensional Tukey region.
}
\arguments{
\item{x}{
object of class \code{TukeyRegion} to be plotted.
}
\item{newPlot}{
whether to create a new plot(2D)/scene(3D).
}
\item{drawPoints}{
whether to show the data points.
}
\item{drawRidges}{
whether to show the ridges; works for non-triangulated facets only.
}
\item{colorBackground}{
background color of the plot(2D)/scene(3D).
}
\item{colorPoints}{
color of the points in case they are shown.
}
\item{colorFacets}{
color of the facets.
}
\item{colorRidges}{
color of the facets' ridges in case they are shown.
}
\item{lwd}{
line width of the facets in 2D.
}
\item{lty}{
line type of the facets in 2D.
}
\item{alpha}{
transperency of the facets (and ridges if shown).
}
\item{...}{
included for compatibility and should not be used.
}
}
\details{
If dimension is equal to two, the traditional \code{\link{plot}} is produced. If dimension is equal to three, the 3D-scene is produced using the package \code{\link{rgl}}.
}
\author{
Pavlo Mozharovskyi <pavlo.mozharovskyi@ensai.fr>
}
\seealso{
\code{\link{TukeyRegion}}, \code{\link{TukeyMedian}}
}
\examples{
# See examples in TukeyRegion or TukeyMedian
}
