# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# aooArea.R
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
#' @title Calculate AOO
#' @description Calculate area of occupancy measured in 2km resolution using a binary SDM
#' @param r Raster layer of a binary SDM. Must be either unprojected in the WGS84 datum, or projected in an equal area projection (see IUCN guidelines) measured in meters.
#' Unprojected rasters will result in an estimate while those projected in an equal area projection will be more accurate.
#' Raster values must be either NA indicating absence, or 1, indicating presence.
#' @param locs (optional) data.frame of occurrence records: Longitude and latitude. If provided, AOO of cells containing occurrence points
#' is returned. If NULL, AOO of SDM is returned. The projection should match that of r.
#' @return a list of three objects. The first object is a character showing the AOO of cells with occurrence records.
#' The second is a raster object of the resampled AOO. The third object (optional) represents the AOO raster showing pixels in which the localities occur, resampled to 2kmx2km
#'  (only if locs argument is supplied).
#' @examples
#' # create raster
#' r1 <- raster::raster(nrows=5, ncols=5, xmn=-5, xmx=5, ymn=-5, ymx=5)
#' raster::values(r1)<- runif(n = (5*5))
#' r1[r1 < 0.5] <- NA
#' r1[!is.na(r1)] <- 1
#' # calculate aooArea
#' AOOarea(r = r1)
#' @export

AOOarea <- function (r, locs = NULL){
  #require(raster)

  if (is.null(locs)) {
    if (raster::isLonLat(r)) {
      dummy <- r
      r[!is.na(r)] <- 1
      raster::res(dummy) <- 0.01666667
      r.2km <- raster::resample(x = r, y = dummy, method = "ngb")
      km2 <- raster::cellStats(!is.na(r.2km), stat = sum) * 4
      #rasArea <- paste0("AOO:", fc.cells, " km^2")
      #rasArea <- cbind("AOO:", paste(fc.cells), " km^2")
      rasArea <- data.frame(Metric = "AOO", km2)
    }
    else {
      r.dummy <- r
      r[!is.na(r)] <- 1
      agg <- 2000/raster::res(r)[1]
      r.2km <- raster::aggregate(x = r, fact = agg, fun = "max")
      km2 <- raster::cellStats(r.2km, na.rm = T, stat = sum) *
        4
      #rasArea <- paste0("AOO: ", fc.cells, " km^2")
      rasArea <- data.frame(Metric = "AOO", km2)
    }
    aooPix = NULL
  }
  else {
    r[!is.na(r)] <- 1
    r.2km <- raster::projectRaster(from = r, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                                   method = "ngb")
    dummy <- r.2km
    raster::res(dummy) <- 0.01666667
    r.2km <- raster::resample(x = r.2km, y = dummy, method = "ngb")
    locCells <- raster::extract(r.2km, locs)
#    rasArea <- paste0("AOO of cells with occurrence records:",
 #                     paste(length(locCells) * 4), "km^2")
    km2 <- length(locCells) * 4
    rasArea <- data.frame(Metric = "AOO with records", km2)
    aooPix <- raster::rasterize(locs, r.2km)
  }

  return(list(area = rasArea, aooRaster = r.2km, aooPixels = aooPix))
}
