# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# mcp.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the changeRangeR R package.
#
# changeRangeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# changeRangeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with changeRangeR. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
#' @title Minimum Convex Hull Polygon
#' @param xy Matrix or Data frame of occurrence coordinates
#' @param crs Character of coordinate reference system for minimum convex hull
#' @description Generates a minimum convex polygon (MCP; i.e., convex hull) that
#' is delineated from occurrence locality coordinates.
#' This function is a wrapper for `chull()` that makes a SpatialPolygons object.
#' @return a SpatialPolygons object of the minimum convex hull around occurrences.
#' @examples
#' # generate occurrences
#' ras1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(ras1)<- runif(n = (108*108))
#' occs <- dismo::randomPoints(ras1, 4)
#' # create mcp
#' mcp(occs)
#' @export

# make a minimum convex polygon as SpatialPolygons object
mcp <- function(xy, crs = NULL) {
  #require(sp)
  xy <- as.data.frame(sp::coordinates(xy))
  coords.t <- grDevices::chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(xy.bord))), 1)))
  if(!is.null(crs)) {
    poly@proj4string <- sp::CRS(crs)
  } else {
    message("WARNING: this minimum convex polygon has no coordinate reference system.")
  }
  return(poly)
}
