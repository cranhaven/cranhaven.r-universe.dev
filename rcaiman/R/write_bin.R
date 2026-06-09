#' Write and read binarized images
#'
#' Wrapper functions around [terra::rast()] to read and write binary masks.
#'
#' @section Functions:
#' \describe{
#'   \item{`write_bin`}{Write a one-layer logical [terra::SpatRaster-class]
#'     to disk as a GeoTIFF (`GTiff`, `INT1U`). No return value.}
#'   \item{`read_bin`}{Read a file with values `255` and/or `0`, such as the one
#'      produced by `write_bin` (see *Details*), and return a logical
#'     [terra::SpatRaster-class] (`TRUE` for `255`, `FALSE` for `0`).}
#' }
#'
#' @param path character vector of length one. File path to read or write. See
#'   examples.
#' @param bin logical [terra::SpatRaster-class] with a single layer.
#'
#' @details
#' `write_bin()` multiplies the input logical raster by 255 and writes the
#' result as a GeoTIFF (`GTiff`) with datatype `INT1U`. Both `write_bin()` and
#' `read_bin()` set the raster extent to `terra::ext(0, ncol(r), 0, nrow(r))`
#' and the CRS to EPSG:7589.
#'
#' @return See *Functions*
#'
#' @name read_bin
#' @rdname read_bin
#' @aliases write_bin
#'
#' @export
#'
#' @seealso [read_caim()], [write_caim()].
#'
#' @examples
#' \dontrun{
#' z <- zenith_image(1000, lens())
#' m <- !is.na(z)
#' my_file <- file.path(tempdir(), "mask.tif")
#' write_bin(m, my_file)
#' m_from_disk <- read_bin(my_file)
#' plot(m - m_from_disk)
#' }
read_bin <- function(path) {
  .check_vector(path, "character", 1)
  .assert_file_exists(path)

  r <- rast(path)
  terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
  # https://spatialreference.org/ref/sr-org/7589/
  terra::crs(r) <- "epsg:7589"
  r <- is.na(r)
  if (stats::sd(r[]) == 0) r <- rast(path)
  as.logical(r)
}

#' @rdname read_bin
#' @export
write_bin <- function(bin, path) {
  .assert_logical_mask(bin)
  .check_vector(path, "character", 1)

  file_name <- basename(path)
  file_name <-  .extension(file_name, "tif")

  terra::crs(bin) <- "epsg:7589" # https://spatialreference.org/ref/sr-org/7589/
  terra::ext(bin) <- terra::ext(0, ncol(bin), 0, nrow(bin))

  suppressWarnings(
    terra::writeRaster(bin * 255, file.path(dirname(path), file_name),
                       filetype = "GTiff", datatype = "INT1U", overwrite = TRUE)
  )
}

