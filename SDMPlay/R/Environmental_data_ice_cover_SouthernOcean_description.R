#' Environmental descriptor example (ice cover, Southern Ocean)
#'
#' @description Average ice cover layer at the scale of the Southern Ocean at 0.1° resolution
#'
#' @usage
#'data("ice_cover_mean_SO")
#'
#' @format
#' RasterLayer. Grid: nrow= 350, ncol= 3600, ncells= 1260000 pixels. Spatial resolution: 0.1. Spatial extent: -180, 180, -80, -45 (longmin, longmax, latmin, latmax); \cr Crs : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0. Origin=0
#'
#'@source
#'[ADD website](https://data.aad.gov.au/metadata/records/fulldisplay/environmental_layers)
#'
#'@examples
#'library(raster)
#'data("depth_SO")
#'data("ice_cover_mean_SO")
#'data("seafloor_temp_2005_2012_mean_SO")
#'predictors_stack_SO <- raster::stack(depth_SO,ice_cover_mean_SO,seafloor_temp_2005_2012_mean_SO)
#'names(predictors_stack_SO)<-c("depth","ice_cover_mean","seafloor_temp_mean")
#'predictors_stack_SO

"ice_cover_mean_SO"



