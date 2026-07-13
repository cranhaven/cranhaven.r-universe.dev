#' `fastCrop` is deprecated.
#'
#' @param x Raster to crop
#' @param y Raster to crop with
#' @param ... other
#'
#' @export
#' @seealso `velox::VeloxRaster_crop`
#'
#' @rdname deprecated
fastCrop <- function(x, y, ...) {
  .Deprecated("terra::crop")
}
