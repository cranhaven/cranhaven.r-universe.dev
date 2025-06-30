\name{weightedMap}
\alias{weightedMap}
\alias{wmap}
\title{
Construct weighted map using Voronoi tessellation and cartogram weighting
}
\description{
A weighted map ("wmap") is a combination of a Voronoi tessellation with cartogram weighting. A Voronoi map is a tessellation of a surface based on a set of geographic points. It is used to display areal patterns without overlap. Additionally, the size of the tiles can be weighted by cartogram-deformation to allow for varying the visual impression of the data. Specifically, this allows for equal-area-sized tiles to equally represent all data-points in the visual display.
}
\usage{
weightedMap(x, y = NULL, window = NULL, crs = NULL, weights = "equal",
  grouping = NULL, holes = NULL, concavity = 2, expansion = 1000,
  method = "dcn", maxit = 10, verbose = 0)
}
\arguments{
  \item{x}{
Coordinates of the data-points, either an \code{sf} object or a two-column matrix/dataframe with x ('longitude') and y ('latitude') coordinates. Alternatively, only specify the x-coordinates here and use the \code{y} parameter for the y-coordinates.
}
  \item{y}{
Latitude (y-coordinates), when the parameter \code{x} is used for longitude only.
}
  \item{window}{
Geographical window within which the Voronoi-tessellation will be displayed. Typically an \code{sf} (multi)polygon, but an attempt is made to interpret other formats (e.g. \code{owin} from Spatstat, \code{SpatVector} from Terra and \code{Spatial} from sp). Consider libraries like \code{geodata} and \code{rnaturalearth} to obtain suitable windows. Polygons that do not contain any coordinates are removed (with a warning). Coordinates that do not lie within the window are removed (with a warning).

When no window is provided (by default), then a concave hull is induced from the coordinates (using \code{\link[concaveman]{concaveman}}). Various other parameters explained below can be used to influence this hull.
}
  \item{crs}{
Coordinate reference system that is necessary for the projection of the map. When not provided, an attempt is made to extract a crs from the provided coordinates or from the provided window. Without any crs there will be a warning and \code{EPSG:3857} will be assumed. Note that the ubiquitous \code{EPSG:4326} is strictly speaking not a projection and results in various errors; use a projected version like \code{EPSG:3857} instead, or use any of the numerous better alternatives (see examples below for some ideas).
}
  \item{weights}{
Vector with weights for the deformation of the Voronoi-tiles. Should have the same length as the number of coordinates provided. The weights are passed to \code{\link[cartogramR]{cartogramR}} to perform the deformation. Defaults to \code{"equal"} for equal-area tiles. When \code{NULL} no weighting is performed, but a non-weighted Voronoi-map is still produced.
}
  \item{grouping}{
Influence the form of the concave window around the coordinates. Only used when there is no \code{window} provided. A vector with the same length as the number of coordinates, listing for each coordinate to which group it belongs. An attempt is made to make separate windows for each group. Note that a high value for the parameter \code{expansion} might result in overlap.
}
  \item{holes}{
A list of \code{x,y} coordinates where holes should be inferred. When \code{window = NULL} there will be holes inserted inside the window around the coordinates specified within a distance as specified in \code{expansion} from the nearest points around the coordinates. Not used when there is an explicit window provided.
}
  \item{concavity}{
Influence the form of the concave window around the coordinates. Only used when there is no \code{window} provided. Parameter passed internally to \code{\link[concaveman]{concaveman}} determining the concavity of the hull. High values result in more convex hulls. Low values (especially between 1 and 0) lead to highly concave ("wiggly") windows.
}
  \item{expansion}{
Influence the form of the concave window around the coordinates. Only used when there is no \code{window} provided. Expands the window (value in meters), and results in more "rounded" windows.
}
  \item{method}{
Method used for cartogram deformation, passed to \code{\link[cartogramR]{cartogramR}}. By default, the older-quicker-less accurate method \code{dcn} is used. More modern and accurate methods \code{gsm} and \code{gn} can also be used, but they might lead to strange results with more complex windows. They also sometimes lead to strange results, or downright stall. Lower \code{maxit} might prevent these problems, but lead to less accurate weighting. Also consider the option \code{verbose} to get an indication where things go wring.
}
  \item{maxit}{
Maximum number of iterations to find a suitable deformation. Parameter passed internally to \code{\link[cartogramR]{cartogramR}}. Higher values lead to better approximations of the size of the polygons to the weights. However, with complex maps it might take very long to converge (or even never finish).
}
  \item{verbose}{
With \code{verbose = 1} turn on verbose output for \code{\link[cartogramR]{cartogramR}} to check where the deformation might run wild
}
}
\details{
Internally, the Voronoi-tessellation is made without respecting the window, and only afterwards the window is superimposed on the tessellation. In some circumstances with internal holes in the window provided, an attempt is made to return tiles that do not jump across such holes. However, sometimes artefacts are still visible in the output.

Warnings are produced when coordinates lie outside the window provided. The results should still work, but without these points outside. Any colouring or other uses of the results have to be adapted accordingly by using the information in \code{$outsideWindow}. Polygons without any points inside are likewise removed with a warning.

To deal with overlapping coordinates some jitter is automatically applied to the coordinates provided.
}
\value{
List of various lengths, depending on specified parameters. Use the names to select any of these results:
\item{crs: }{
The crs in WKT format.
}
\item{points: }{
The coordinates as provided, but as a projected \code{sfc_POINT} object.
}
\item{grouping: }{
Character vector with the provided grouping, or the grouping as induced from any provided window.
}
\item{window: }{
The window around the coordinates as a projected \code{sfc_POLYGON} or \code{sfc_MULTIPOLYGON} object. Some polygons from a provided window might have been removed because they are empty.
}
\item{emptyPolygons: }{
Numeric vector with the indices of the polygons that are removed because they do not contain any of the coordinates provided.
}
\item{outsideWindow: }{
Numeric vector with the indices of the coordinates that are removed because they are outside of the window provided.
}
\item{voronoi: }{
Voronoi-tessellation of the window as a projected \code{sfc_MULTIPOLYGON} object.
}
\item{weights: }{
Numeric vector with the weights used for the deformation. Weights for coordinates outside of the window are removed.
}
\item{weightedPoints: }{
Coordinates after the weighting-deformation, specified as a projected \code{sfc_POINT} object.
}
\item{weightedWindow: }{
Window after the weighting-deformation, specified as a projected \code{sfc_POLYGON} or \code{sfc_MULTIPOLYGON} object.
}
\item{weightedMap: }{
Voronoi-tessellation after the weighting-deformation, specified as a projected \code{sfc_MULTIPOLYGON} object.
}
}
\note{
With more complex windows the deformation by \code{\link[cartogramR]{cartogramR}} might throw errors (like "IllegalArgumentException") or fall into an infinite loop. Try to reduce \code{maxit} and check \code{verbose = 1} to get an idea what might be going wrong.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
# generate a window from coordinates
# note the Germany-centered Gauss-Kruger projection "EPSG:2397"
# consider increasing 'maxit' to remove the warning about convergence
data(hessen)
v <- weightedMap(hessen$villages, expansion = 4000, crs = 2397, maxit = 2)
plot(v$weightedVoronoi)

# show the original locations before the transformation in orange
plot(v$points, add = TRUE, col = "green", cex = .5)
# show the new locations after transformation in red
plot(v$weightedPoints, add = TRUE, col = "blue", cex = .5)

# add the real border of Hessen for comparison
h <- sf::st_as_sf(hessen$boundary)
sf::st_crs(h) <- 4326
h <- sf::st_transform(h, 2397)
plot(h, add = TRUE, border = "red")

# use the Voronoi tiles e.g. for Heeringa-colouring (see function "heeringa()")
d <- dist(hessen$data, method = "canberra")
plot(v$weightedVoronoi, col = heeringa(d), border = NA)
plot(v$weightedWindow, add = TRUE, lwd = 2)

# grouping-vector can be used to make separations in the base-map
groups <- rep("a", times = 157)
groups[157] <- "b"
groups[c(58,59)] <- "c"
groups[c(101, 102, 107)] <- "d"
# holes-list can be used to add holes inside the region
holes <- list(c(9, 50.5), c(9.6, 51.3), c(8.9, 51))
v <- weightedMap(hessen$villages, grouping = groups, holes = holes,
                  crs = 2397, expansion = 3000)
plot(v$weightedVoronoi, col = "grey")

\dontrun{
# extensive example using data from WALS (https://wals.info). Both the worldmap
# and the WALS data are downloaded directly. The worldmap is projected and
# deformed so that each datapoint has equal area on the map.

# load worldmap
data(world)

# try different projections
azimuth_equaldist  <- "+proj=aeqd +lat_0=90 +lon_0=45"
mollweide_atlantic <- "+proj=moll +lon_0=11.5"
mollweide_pacific  <- "+proj=moll +lon_0=151"

plot(sf::st_transform(world, crs = azimuth_equaldist))

# load WALS data, example feature 13: "tone"
library(lingtypology)
feature <- "13A"
wals <- wals.feature(feature)
head(wals)

# get glottolog coordinates and correct errors in WALS
wals$glottocode[wals$glottocode == "poqo1257"] <- "poqo1253"
wals$glottocode[wals$glottocode == "mamc1234"] <- "mamm1241"
wals$glottocode[wals$glottocode == "tuka1247"] <- "tuka1248"
wals$glottocode[wals$glottocode == "bali1280"] <- "unea1237"
wals <- merge(wals[,c("glottocode", "wals.code", feature)],
              glottolog[,c("glottocode", "longitude", "latitude")])

# calculate an equally-weighted voronoi transformation
v <- weightedMap(wals$longitude, wals$latitude, window = world,
                  crs = mollweide_atlantic, method = "dcn", maxit = 10)

# prepare colors
cols <- c("lightsalmon", "grey", "lightpink")
names(cols) <- names(table(wals[,feature]))
cols <- cols[c(2,3,1)]

# the map
plot(v$weightedVoronoi, col = cols[wals[,feature]], border = "darkgrey", lwd = 0.2)
plot(v$weightedWindow, border = "black", add = TRUE, lwd = 0.5)
legend("bottomleft", legend = names(cols), fill = cols, cex = .7)

# add contourlines
height <- c(0, 0.5, 1)
names(height) <- c("No tones", "Simple tone system", "Complex tone system")
addContour(height = height[wals[,feature]],
           points = v$weightedPoints,
           window = v$weightedWindow,
           crs = v$crs,
           col = "darkred", levels = c(0.2, 0.4, 0.6))

# Alternative: using points instead of polygons
cols[2:3] <- c("orange", "red")
plot(v$weightedPoints, col = cols[wals[,feature]], cex = 1, pch = 19)
plot(v$weightedWindow, add = T, border = "darkgrey", lwd = 0.5)
}
}
\keyword{hplot}

