#' Apply Polygon Mask to Raster Layers
#'
#' This function crops and extends raster layers to a study area extent (bbox) defined by longitude
#' and latitude then applies a mask based on a provided spatial polygon to remove areas outside the polygon.
#'
#' @param layers A stack of raster layers (`SpatRaster` object) to be processed.
#' @param study_area A spatial polygon (`sf` object) used to mask the raster layers.
#'
#' @return A `SpatRaster` object representing the masked raster layers.
#'
#' @export
layer_mask <- function(layers, study_area) {
  # Validate inputs
  if (!inherits(layers, "SpatRaster")) {
    stop("Argument 'layers' must be a 'SpatRaster' object.")
  }
  if (!(inherits(study_area, "sf") | inherits(study_area, "sfc") | inherits(study_area, "sfc_MULTIPOLYGON") | inherits(study_area, "sfc_POLYGON"))) {
    stop("Argument 'study_area' must be an 'sf' object.")
  }

  # Convert study_area to SpatVector
  study_area_vect <- terra::vect(study_area)

  # Crop to study_area extent
  cropped_layer <- terra::crop(layers, terra::ext(study_area_vect), snap = "out")

  # Extend to study_area extent
  extended_layer <- terra::extend(cropped_layer, terra::ext(study_area_vect), snap = "out")

  # Apply mask to remove areas outside the polygon
  processed_layers <- terra::mask(extended_layer, study_area_vect, touches = TRUE)

  return(processed_layers)
}
