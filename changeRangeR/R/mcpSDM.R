# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# mcpSDM.R
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
#' @name mcpSDM
#' @title SDM-based Minimum Convex Hull Polygon
#' @param p Raster* object of a continuous species distribution model prediction to base hull calculation on
#' @param xy Matrix or Data frame of occurrence coordinates
#' @param ch.orig SpatialPolygons object of original minimum convex hull based on occurrence locality coordinates
#' @param thr Numeric threshold used to convert the continuous SDM prediction to a binary range map;
#' this is then used to delineate the hull
#' @description Implements the technique to estimate IUCN's extent of occurrence (EOO) geographic range estimate of species
#' threat level by delineating a minimum convex polygon (i.e., convex hull) around a thresholded SDM prediction, first
#' described by Syfert et al. (2014) <doi:10.1016/j.biocon.2014.06.012>. For each increment of 0.01 between a user-specified threshold and the maximum SDM
#' prediction value, the prediction is thresholded to this value to make a binary raster. This raster is then converted
#' to points, which are used to delineate a trial MCP. Each trial MCP is spatially intersected with the original
#' MCP (based on the occurrence coordinates) and the original occurrence points. The Jaccard similarity index is calculated
#' to determine geographic similarity between the trial and observed MCP. The trial MCP is also spatially intersected with the
#' original occurrence points to determine how many were omitted. The "best" MCP is the one that has the highest JSI and also
#' omits the least original occurrence points.
#' @note Thresholds for SDM predictions greater than the minimum suitability across all occurrence localities will result in
#' some occurrences being excluded from the EOO, which does not match the definition from IUCN.
#' @return a list of 5 objects.
#' @references Syfert, M. M., Joppa, L., Smith, M. J., Coomes, D. A., Bachman, S. P., & Brummitt, N. A. (2014). Using species distribution models to inform IUCN Red List assessments. Biological Conservation, 177, 174–184. https://doi.org/10.1016/j.biocon.2014.06.012
#' @examples
#' \donttest{
#' # create continuous raster
#' p <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(p)<- runif(n = (108*108))
#' raster::crs(p) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' # create occurrences
#' xy <- dismo::randomPoints(p, 4)
#' # create original convex hull
#' ch.orig <- mcp(xy, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' # set threshold
#' thr <- 0.5
#' # mcpSDM
#' mcpSDM(p, xy, ch.orig, thr)
#' }
#' @export
#'

mcpSDM <- function(p, xy, ch.orig, thr) {
  #require(raster)

  vals.p <- raster::getValues(p)
  x <- seq(thr, max(vals.p, na.rm=TRUE), 0.01)
  jsi.vec <- numeric(length(x))
  ov.pts.vec <- numeric(length(x))
  ch.vec <- list()

  for(i in 1:length(x)) {
    th <- x[i]
    p.i <- p >= th
    p.i[p.i == 0] <- NA
    p.i.xy <- raster::rasterToPoints(p.i)
    if(nrow(p.i.xy) > 1) {
      ch.i <- changeRangeR::mcp(p.i.xy[,1:2], crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ch.i.sf <- sf::st_as_sf(ch.i)
      xy.sf <- sf::st_as_sf(sp::SpatialPoints(xy, proj4string = raster::crs(ch.i)))
      ov.xy <- sf::st_intersection(ch.i.sf, xy.sf)
      if(nrow(ov.xy) > 0) {
        ov.xy.sp <- sf::as_Spatial(ov.xy)
        ov.pts.vec[i] <- nrow(ov.xy.sp@coords)
        }
      ch.vec[[i]] <- ch.i
      if(sf::st_crs(ch.i.sf) != sf::st_crs(ch.orig)) {
        sf::st_crs(ch.i.sf) <- sf::st_crs(ch.orig)
      }
      ov.sf <- sf::st_intersection(ch.i.sf, sf::st_as_sf(ch.orig))
      if(nrow(ov.sf) > 0) {
        A <- raster::area(ch.i)
        B <- raster::area(ch.orig)
        C <- raster::area(sf::as_Spatial(ov.sf))
        jsi.vec[i] <- C / (A + B - C)
      }
    }
  }

  jsi.vec.allPts <- jsi.vec[which(ov.pts.vec == max(ov.pts.vec))]
  i.bestfit <- which(jsi.vec.allPts == max(jsi.vec.allPts))
  ch.bestfit <- ch.vec[[i.bestfit]]

  return(list(jsi = jsi.vec, thr = x, ov.pts = ov.pts.vec, best.fit = ch.bestfit, best.fit.ind = i.bestfit))
}
