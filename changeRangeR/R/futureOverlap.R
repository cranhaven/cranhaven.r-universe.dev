# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# futureOverlap.R
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
#' @title Calculate the ratio of future overlap of SDMs with shapefile categories
#' @description Calculate future overlap of SDMs with shapefile categories
#' @param r list of rasters of binary SDMs
#' @param r.names list of character values of the names representing each raster in r
#' @param futures List of SpatialPolygons* objects with same CRS as r
#' @param futures.names list of character values of the names representing each SpatialPolygons* object in futures.
#' @param field The shapefile field attribute containing the features to compare (i.e., the column name).
#' @param category a list of the names of shapefile features to compare. If all features are to be used, input "All".
#' @return a matrix showing the overlap between raster names and features.
#' @examples
#' #create rasters
#' r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r1)<- runif(n = (108*108))
#' r2 <- r1 >=0.5
#' r2[r2<1] <- NA
#' r3 <- r1 >=0.75
#' r3[r3<1] <- NA
#' # Create r
#' r <- list(r2, r3)
#' # create r.names
#' r.names <- c('scenario 1', 'scenario 2')
#' # create futures
#' coords <- dismo::randomPoints(r1, 3)
#' future <- sp::Polygon(coords)
#' future <- sp::SpatialPolygons(list(sp::Polygons(list(future), ID = "a")))
#' futures <- list(future, future)
#' futures.names <- list("fut1", "fut2")
#' # set field and category
#' field = "a"
#' category = "All"
#' # run function
#' futureOverlap(r, futures, field, category, r.names, futures.names)
#' @export



futureOverlap <- function(r, futures, field, category, r.names, futures.names){
  # setClass("ratioOverlap", slots = list(maskedRange = "RasterLayer", ratio = "character"))
  #require(sf)
  #require(raster)
  #require(dplyr)
  #require(sp)

  #rat.io <- lapply(futures, function(shp){
  if (category == "All"){
    shp <- lapply(futures, sf::st_as_sf)
    #r <- lapply(r, function(x) raster::projectRaster(x, crs = crs(shp[[1]]), method = "ngb"))
    # shp <- lapply(shp, function(x) sf::st_transform(x, crs = crs(r[[1]])))
    maskedRange <- mapply(raster::mask, r, shp)
    # maskedRange <- lapply(r, function(x) lapply(futures, function(y) raster::mask(x ,y)))
  } else {
    shp <- lapply(futures, sf::st_as_sf)
    # if (crs == NULL){
    #   fc <- filter(shp, grepl(category, field))
    #   out<- mask(r, fc)
    # } else {
    #r <- lapply(raster::projectRaster(r, crs = crs(shp)))
    #shp <- lapply(shp, function(x) sf::st_transform(x, crs = crs(r[[1]])))
    fc <- lapply(category, function(x) dplyr::filter(shp, shp[[field]]==x))
    fc <- do.call("rbind", fc)
    #fc <- filter(shp, shp[[field]]==category)
    maskedRange <- raster::mask(r, fc)
    #  }
    #  return(out)
  }
  #})

  # rat.io.s <- mapply(maskedRange, function(x){
  #   raster::ncell(maskedRange[!is.na(maskedRange)]) / raster::ncell(r[!is.na(r)]) * 100
  # })

  rat.io.s <- mapply(function(x, y){raster::ncell(x[!is.na(x)]) / raster::ncell(y[!is.na(y)]) * 100}, x=maskedRange, y=r)

  #ratio <- ncell(maskedRange[!is.na(maskedRange)]) / ncell(r[!is.na(r)]) * 100
  #ratio <- paste0("Percentage of range within shape is ", ratio, "%")
  ratioValues <- mapply(function(x,y,z){paste0("Overlap between ", x, " and ", y, "is", z)}, x=r.names, y=futures.names, z=round(x = rat.io.s, digits = 3))
  ratioValues <- data.frame(strsplit(ratioValues, "is "))
  out <- strsplit(t(ratioValues), "is")
  out<- do.call("rbind", out)
  colnames(out) <- c("Layer comparisons", "overlap")
  out <- as.data.frame(out)
  out$overlap <- as.numeric(out$overlap)
  return(out)
  # out <- new("ratioOverlap", maskedRange = maskedRange, ratio = ratio)
}
