#' Point-Polygon Distances
#'
#' Calculate the maximum or minimum possible distance from a point to the edge
#' of a given polygon.
#'
#' @name point.poly.dist
#'
#' @param poly A simplefeatures object of class polygon or multipolygon.
#' @param point A simplefeatures object of class point.
#' @param max Logical; return maximum or minimum distance? default `TRUE`
#' @param by_element Logical; return total maximum or minimum, or for each input
#' point? default `FALSE`
#'
#' @import sf
#'
#' @return Maximum or minimum distance between a point and a polygon.
#' @export
#'
#' @examples
#' library(sf)
#' polys <- st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
#' crs = st_crs('OGC:CRS84'))
#' points <- st_sfc(st_multipoint(rbind(c(.25, .5), c(.75, .5), c(.5, .5))),
#'                  crs = st_crs('OGC:CRS84'))
#' point.poly.dist(points, polys)

point.poly.dist <- function(point, poly, max = TRUE, by_element = FALSE) {

  ## convert to sfc if necessary
  if (inherits(poly, 'Spatial')) poly <- st_as_sfc(poly)
  if (inherits(point, 'Spatial')) point <- st_as_sfc(point)

  ## project point using polygon CRS
  point <- st_transform(point, st_crs(poly))

  ## extract border vertices and point coordinates
  border <- st_coordinates(poly)[, 1:2]
  point <- matrix(st_coordinates(point)[, 1:2], ncol = 2)

  ## calculate distance from point to every border vertex
  if (st_is_longlat(poly)) {

    if (!requireNamespace("geosphere")) {
      stop(paste0("Package geosphere required to calculate distances for ",
                  "unprojected data\n",
                  "Install with: install.packages(\"geosphere\")\n"))
    }

    dists <- do.call(rbind,
                     lapply(split(point, seq(nrow(point))),
                            function(x) geosphere::distGeo(border, x)))
  } else {
    dists <- mapply(dist_fx, border[, 1], border[, 2], MoreArgs = list(point))
  }

  ## return maximum or minimum distance from point to polygon edge
  if (by_element) {

    return(units::set_units(unname(apply(dists, 1,
                                         ifelse(max, base::max, min))), 'm'))

  } else {

    return(units::set_units(ifelse(max, base::max, min)(dists), 'm'))

  }

}

## euclidean distance
dist_fx <- function(border_x, border_y, point) {
  return(sqrt((border_x - point[, 1])^2 + (border_y - point[, 2])^2))
}
