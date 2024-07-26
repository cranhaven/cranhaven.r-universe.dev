# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# ratioOverlap.R
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
#' @title Calculate the proportion of a range area that is either 1: contained by geographic categories, or 2: correlated with
#' a continuous environmental layer.
#' @description Calculate the proportion of the species' range (e.g., a thresholded SDM) that is contained by landcover categories
#' taken from a shapefile. Example shapefile categories include protected areas, threatened areas. ratioOverlap returns a list
#' of the masked raster layer and the percent of the total range that lies within the shapefile polygons specified. If shp is a raster
#' representing an environmental layer, the proportion of presence within quantiles of the environmental layer and the correlation
#' between the layer and the range are returned.
#' @param r either categorical raster or shapefile object representing a binary range.
#' @param shp either 1) a shapefile of land cover features or 2) a continuous raster. Must be in same projection as r parameter. If shp is a raster, then the number of cells within each quantile are calculated.
#' @param rasMask (optional) a raster layer to calculate the Pearson correlation with the object r. Only if r or shp is a raster layer.
#' @param field The shapefile field attribute containing the features to compare (i.e., the column name).
#' @param category a list of the names of shapefile features to compare. If all features are to be used, input "All".
#' @param subfield boolean. If TRUE, the overlap ratio of all unique categories of shp will be calculated.
#' @param quant Either the character string "quartile" for the ratio of each quartile, or a concatenation of values to use instead.
#' @return a list of three objects. The first object is a raster object showing the masked range. The second is a character showing the
#' percentage of range within the category of interest. The third shows the correlation with rasMask if it is supplied.
#' @examples
#' # create binary raster
#' r <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r)<- runif(n = (108*108))
#' r[r < 0.5] <- NA
#' r[r > 0.5] <- 1
#' # create shp
#' shp <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(shp)<- runif(n = (108*108))
#' # ratioOverlap
#' ratioOverlap(r = r, shp = shp)
#' @export

# # # # load r
# r = raster(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/Forest_suitable_projected_coarse.tif"))
# # create random polygon based on r
# mcp <- function (xy) {
#  xy <- as.data.frame(coordinates(xy))
#  coords.t <- chull(xy[, 1], xy[, 2])
#  xy.bord <- xy[coords.t, ]
#  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
#  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
# }
# rbuf <- gBuffer(mcp(dismo::randomPoints(r, 3)), width = 50000)
# rbuf@proj4string <- crs(r)
# #r <- rbuf
# # load shp and reproject
# shp <- readRDS(file.path(system.file(package="changeRangeR", "extdata/DemoData/shapefiles"), "WDPA_COL_olinguito_simp.rds"))
# shp <- spTransform(shp, CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs"))
# # convert shp to raster
# shp <- raster::rasterize(shp, r)
# # Define args
# field <- "DESIG_ENG"
# #category <- "All"
# category <- c("National Natural Park", "Regional Natural Parks", "Integrated Management Regional Districts")
# subfield <- TRUE
# #test
# t <- ratioOverlap(r, shp, field = field, category = category, subfield = FALSE, quant = c(0.5, 0.75))
# quant = c(0.25, 0.87)

ratioOverlap <- function(r, shp = NULL, rasMask = NULL, field = NULL, category = NULL, subfield = FALSE, quant = "quartile"){

  #setClass("ratioOverlap", slots = list(maskedRange = "RasterLayer", ratio = "character"))
  #require(sf)
  #require(raster)
  #require(dplyr)

  ## if r is a shapefile
  if(!("RasterLayer" %in% class(r)) & !("RasterLayer" %in% class(shp))){
    if(subfield == FALSE){
      if(category == "All"){
        maskedRange <- sf::st_intersection(r, shp)
      }else{
        fc <- lapply(category, function(x) dplyr::filter(shp, shp[[field]]==x))
        fc <- do.call("rbind", fc)
        maskedRange <- sf::st_intersection(r, fc)
      }
      ratio <- (sum(sf::st_area(maskedRange)) / sf::st_area(r)) * 100
      ratio <- paste0("Percentage of range within shape is ", round(x = ratio, digits = 3), "%")
      correlation <- NULL
    }
    if(subfield == TRUE){
      if(category[1] == "All"){
        # mask r by shp
        #maskedRange <- sf::st_intersection(r, shp)
        # For each unique category within the field of interest create a list of masks
        uniqueCategories <- unique(shp[[field]])
        uniqueShapes <- lapply(uniqueCategories, function(x) dplyr::filter(shp, shp[[field]] == x))
        maskedRange <- lapply(uniqueShapes, function(x) sf::st_intersection(x, r))
        category <- uniqueCategories
      }else{
        fc <- lapply(category, function(x) dplyr::filter(shp, shp[[field]]==x))
        #fc <- do.call("rbind", fc)
        maskedRange <- lapply(fc, function(x) sf::st_intersection(r, x))
        #maskedRange <- sf::st_intersection(r, fc)
       if(!("list" %in% class(maskedRange))){
          maskedRange <- list(maskedRange)
        }
        ratio <- lapply(maskedRange, function(x) raster::ncell(x[!is.na(x)]) / raster::ncell(r[!is.na(r)]) * 100)
        ratio <- paste0("Percentage of range within ", paste0(category, collapse =", ") ," is ", round(x = unlist(ratio), digits = 3), "%")
        correlation <- NULL
      }
      if(!exists("ratio")){
      if(!("list" %in% class(maskedRange))){
        maskedRange <- list(maskedRange)
      }
      ratio <- lapply(maskedRange, function(x) sum(sf::st_area(x)) / (sf::st_area(r)) * 100)
      ratio <- paste0("Percentage of range within ", category, " is ", round(x = ratio, digits = 3), "%")
      correlation <- NULL
      }
    }
  }else{
    if(!("RasterLayer" %in% class(r)) & ("RasterLayer" %in% class(shp))){
      if(quant[[1]] == "quartile"){
      r <- raster::rasterize(r, shp, method = "ngb")
      maskedRange <- raster::mask(r, shp)
      maskedQuants <- raster::quantile(raster::mask(shp, r))
      q25 <- raster::ncell(shp[shp < maskedQuants[2]])
      q50 <- raster::ncell(shp[shp > maskedQuants[2] & shp < maskedQuants[3]])
      q75 <- raster::ncell(shp[shp > maskedQuants[3] & shp < maskedQuants[4]])
      q100 <- raster::ncell(shp[shp > maskedQuants[4] & shp < maskedQuants[5]])
      ratq25 <- q25 / raster::ncell(r[!is.na(shp)])
      ratq50 <- q50 / raster::ncell(r[!is.na(shp)])
      ratq75 <- q75 / raster::ncell(r[!is.na(shp)])
      ratq100 <- q100 / raster::ncell(r[!is.na(shp)])
      ratio <- rbind(paste0("The proportion of the range below 25%: ", round(x = ratq25, digits = 3)), paste0("The proportion of the range between 25% and 50%: ", round(x = ratq50, digits = 3)), paste0("The proportion of the range between 50% and 75%: ", round(x = ratq75, digits = 3)),
                     paste0("The proportion of the range between 75% and 100%: ", round(x = ratq100, digits = 3)))
      } else {
        quant = quant
        r <- raster::rasterize(r, shp, method = "ngb")
        maskedRange <- raster::mask(r, shp)
        maskedQuants <- raster::quantile(raster::mask(shp, r), probs = quant)
        quantList <- lapply(maskedQuants, function(x) shp<x)

        sumRas <- quantList[[1]]
        sumRas[!is.na(sumRas)] <- 0
        quantRaster <- NULL

        for(i in 1:length(quantList)){
          quantRaster[[i]] <- quantList[[i]] - sumRas
          quantRaster[[i]][quantRaster[[i]]< 1] <- NA
          sumRas <- sum(sumRas, quantList[[i]], na.rm = T)
          sumRas[sumRas > 0] <- 1
        }
        #quantRaster <- stack(quantRaster)
        rat.ios <- lapply(quantRaster, function(x) (raster::ncell(x[!is.na(x)]) / raster::ncell(r[!is.na(shp)])))
        ratio <- as.data.frame(cbind(names(quantList), as.numeric(rat.ios)))
        colnames(ratio) <- c("value", "ratio")
        #ratio$ratio <- round(ratio$ratio, 3)
      }


      if(!is.null(rasMask)){
        rasMask.resam <- raster::resample(rasMask, r, method = "bilinear")
        rRasmask <- raster::stack(rasMask.resam, r)
        layerCorrs <- raster::layerStats(rRasmask, stat = "pearson")
        correlation <- layerCorrs$`pearson correlation coefficient`[[1]]
      }
    }
  }


  ## if r is a raster
  if(("RasterLayer" %in% class(r))){
    if(("SpatialPolygonsDataFrame" %in% class(shp)) | ("sf" %in% class(shp))){
      if(subfield == FALSE){
        if (category[1] == "All"){
          shp <- sf::st_as_sf(shp)
          r <- raster::projectRaster(r, crs = raster::crs(shp), method = 'ngb')
          maskedRange <- raster::mask(r, shp)
          maskedRange <- list(maskedRange)
        }
        else {
          shp <- sf::st_as_sf(shp)
          # if (crs == NULL){
          #   fc <- filter(shp, grepl(category, field))
          #   out<- mask(r, fc)
          # } else {
          r <- raster::projectRaster(r, crs = raster::crs(shp))
          fc <- lapply(category, function(x) dplyr::filter(shp, shp[[field]]==x))
          fc <- do.call("rbind", fc)
          #fc <- filter(shp, shp[[field]]==category)
          maskedRange <- raster::mask(r, fc)
          #  }
          #  return(out)
        }
      }
      if(subfield == TRUE){
        if (category[1] == "All"){
          shp <- sf::st_as_sf(shp)
          uniqueCategories <- unique(shp[[field]])
          uniqueShapes <- lapply(uniqueCategories, function(x) dplyr::filter(shp, shp[[field]] == x))
          maskedRange <- lapply(uniqueShapes, function(x) raster::mask(r, x))
          category <- uniqueCategories
          #shp <- sf::st_as_sf(shp)
          #r <- raster::projectRaster(r, crs = crs(shp)@projargs, method = 'ngb')
          #maskedRange <- raster::mask(r, shp)
        }
        else {
          shp <- sf::st_as_sf(shp)
          # if (crs == NULL){
          #   fc <- filter(shp, grepl(category, field))
          #   out<- mask(r, fc)
          # } else {
          r <- raster::projectRaster(r, crs = raster::crs(shp))
          fc <- lapply(category, function(x) dplyr::filter(shp, shp[[field]]==x))
          maskedRange <- lapply(fc, function(x) raster::mask(r, x))

          if(!("list" %in% class(maskedRange))){
            maskedRange <- list(maskedRange)
          }
          correlation <- NULL
          ratio <- cbind(as.numeric(lapply(maskedRange, function(x) raster::ncell(x[!is.na(x)]) / raster::ncell(r[!is.na(r)]) * 100)))
          ratio <- round(ratio, 3)
          rat.tab <- cbind(category, ratio)
          ratio <- list(paste0("Percent of range within ", rat.tab[,1], " is ", rat.tab[,2], " %"))
          #fc <- filter(shp, shp[[field]]==category)
          #maskedRange <- raster::mask(r, fc)
          #  }
          #  return(out)
        }
      }
      if(!exists("ratio")){
      if(!("list" %in% class(maskedRange))){
        maskedRange <- list(maskedRange)
      }
      correlation <- NULL
      ratio <- lapply(maskedRange, function(x) raster::ncell(x[!is.na(x)]) / raster::ncell(r[!is.na(r)]) * 100)
      ratio <- paste0("Percentage of range within ", paste0(category, collapse = ", "), " is ", round(x = unlist(ratio), digits = 3), "%")
      #ratio <- raster::ncell(maskedRange[!is.na(maskedRange)]) / raster::ncell(r[!is.na(r)]) * 100
      #ratio <- paste0("Percentage of range within shape is ", ratio, "%")
      }
    }
    if(("RasterLayer" %in% class(shp))){
      if(quant[[1]] == "quartile"){
      shp <- raster::crop(shp, r)
      r <- raster::crop(r, shp)
      maskedRange <- raster::mask(r, shp)
      maskedQuants <- raster::quantile(raster::mask(shp, r))
      q25 <- raster::ncell(shp[shp < maskedQuants[2]])
      q50 <- raster::ncell(shp[shp > maskedQuants[2] & shp < maskedQuants[3]])
      q75 <- raster::ncell(shp[shp > maskedQuants[3] & shp < maskedQuants[4]])
      q100 <- raster::ncell(shp[shp > maskedQuants[4] & shp < maskedQuants[5]])
      ratq25 <- q25 / raster::ncell(r[!is.na(shp)])
      ratq50 <- q50 / raster::ncell(r[!is.na(shp)])
      ratq75 <- q75 / raster::ncell(r[!is.na(shp)])
      ratq100 <- q100 / raster::ncell(r[!is.na(shp)])
      ratio <- rbind(paste0("The proportion of the range below 25%: ", round(x = ratq25, digits = 3)), paste0("The proportion of the range between 25% and 50%: ", round(x = ratq50, digits = 3)), paste0("The proportion of the range between 50% and 75%: ", round(x = ratq75, digits = 3)),
                     paste0("The proportion of the range between 75% and 100%: ", round(x = ratq100, digits = 3)))
      } else {
          quant = quant
          #r <- raster::rasterize(r, shp, method = "ngb")
          maskedRange <- raster::mask(r, shp)
          maskedQuants <- raster::quantile(raster::mask(shp, r), probs = quant)
          quantList <- lapply(maskedQuants, function(x) shp<x)

          sumRas <- quantList[[1]]
          sumRas[!is.na(sumRas)] <- 0
          quantRaster <- NULL

          for(i in 1:length(quantList)){
            quantRaster[[i]] <- quantList[[i]] - sumRas
            quantRaster[[i]][quantRaster[[i]]< 1] <- NA
            sumRas <- sum(sumRas, quantList[[i]], na.rm = T)
            sumRas[sumRas > 0] <- 1
          }
          #quantRaster <- stack(quantRaster)
          rat.ios <- lapply(quantRaster, function(x) (raster::ncell(x[!is.na(x)]) / raster::ncell(r[!is.na(shp)])))
          ratio <- as.data.frame(cbind(names(quantList), as.numeric(rat.ios)))
          colnames(ratio) <- c("value", "ratio")
          #ratio$ratio <- round(ratio$ratio, 3)
        }
    }

    correlation = NULL
    if(!is.null(rasMask)){
      rasMask.resam <- raster::resample(rasMask, r, method = "bilinear")
      rRasmask <- raster::stack(rasMask.resam, r)
      layerCorrs <- raster::layerStats(rRasmask, stat = "pearson" )
      correlation <- layerCorrs$`pearson correlation coefficient`[[2]]
    }
  }

  #out <- new("ratioOverlap", maskedRange = maskedRange, ratio = ratio)
  return(list(maskedRange = maskedRange, ratio = ratio, correlation = correlation))

}

