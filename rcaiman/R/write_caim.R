#' Write canopy image
#'
#' @description
#' Wrapper around [terra::writeRaster()] that writes a canopy image as GeoTIFF
#' with 8- or 16-bit unsigned integers, setting CRS and extent.
#'
#' @details
#' Adds the `.tif` extension to `path` if missing. The CRS is set to EPSG:7589
#' and the extent to `[0, ncol] × [0, nrow]` in pixel units. Data are written as
#' `INT1U` when `bit_depth = 8` and `INT2U` when `bit_depth = 16`.
#'
#' @param caim [terra::SpatRaster-class].
#' @param path character vector of length one. Destination file path (extension
#'   `.tif` will be enforced).
#' @param bit_depth numeric vector of length one. Either `8` or `16`.
#'
#' @return No return value. Called for side effects.
#'
#' @examples
#' \dontrun{
#' caim <- read_caim() %>% normalize_minmax(0, 255)
#' write_caim(caim * (2^8 - 1),  file.path(tempdir(), "test_8bit"),  8)
#' write_caim(caim * (2^16 - 1), file.path(tempdir(), "test_16bit"), 16)
#' # Note: values are scaled by (2^bit_depth - 1) to avoid the maximum bin,
#' # which read_caim() might turn NA.
#' }
write_caim <- function(caim, path, bit_depth) {
  .check_vector(path, "character", 1)

  if (!any(bit_depth == 16, bit_depth == 8)) {
    stop("bit_depth should be 8 or 16.")
  }

  terra::crs(caim) <- "epsg:7589" # https://spatialreference.org/ref/sr-org/7589/
  terra::ext(caim) <- terra::ext(0, ncol(caim), 0, nrow(caim))

  file_name <- basename(path)
  file_name <- .extension(file_name, "tif")

  if (bit_depth == 8) {
    suppressWarnings(
      terra::writeRaster(caim, file.path(dirname(path), file_name),
                         filetype = "GTiff", datatype = "INT1U",
                         overwrite = TRUE)
    )
  } else {
    suppressWarnings(
      terra::writeRaster(caim, file.path(dirname(path), file_name),
                         filetype = "GTiff", datatype = "INT2U",
                         overwrite = TRUE)
    )
  }
}
