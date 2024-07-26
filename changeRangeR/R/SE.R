# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# SE.R
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
#' @name Species Endemism
#' @title Calculate species endemism
#' @description Calculate species endemism as the number of species in a place divided by the total number of places in which those species are found.
#' @param rStack a rasterStack of binary species presences
#' @return Raster object showing species endemism.
#' @author pgalante@@amnh.org
#' @examples
#' # create binary raster
#' r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r1)<- runif(n = (108*108))
#' r1[r1 < 0.5] <- NA
#' r1[r1 > 0.5] <- 1
#' r2 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r2)<- runif(n = (108*108))
#' r2[r2 < 0.5] <- NA
#' r2[r2 > 0.5] <- 1
#' r3 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r3)<- runif(n = (108*108))
#' r3[r3 < 0.5] <- NA
#' r3[r3 > 0.5] <- 1
#' rStack <- raster::stack(r1, r2, r3)
#' # calculate SE
#' SpeciesEndemism(rStack)
#' @export


SpeciesEndemism <- function (rStack){
  #require(raster)
  rStack[rStack == 0] <- NA
  p.df <- as.data.frame(raster::rasterToPoints(rStack))
  sspTotal <- raster::colSums(p.df, na.rm = T)
  ssp.df <- t(p.df[-c(1, 2)]) * (sspTotal[-c(1, 2)])
  ssp.PixSum <- raster::rowSums(t(ssp.df), na.rm = T)
  spSum <- raster::rowSums(p.df[-c(1, 2)], na.rm = T)
  SEvals <- spSum/ssp.PixSum
  stackTotal <- sum(rStack, na.rm = T)
  stackTotal[stackTotal == 0] <- NA
  stackTotal[!is.na(stackTotal)] <- SEvals
  return(stackTotal)
}
