#' Crop a canopy image
#'
#' Extracts a rectangular region of interest (ROI) from a canopy image. This
#' function complements [read_caim()] and [read_caim_raw()].
#'
#' @param r [terra::SpatRaster-class].
#'
#' @inheritParams read_caim
#'
#' @return [terra::SpatRaster-class] object containing the same layers and values
#'   as `r` but restricted to the selected ROI, preserving all other properties.
#'
#' @note `rcaiman` uses terra without geographic semantics: rasters are kept with
#'   unit resolution (cell size = 1) and a standardized extent
#'   `ext(0, ncol, 0, nrow)` with CRS EPSG:7589.
#'
#' @export
#'
#' @examples
#' caim <- read_caim()
#' ncell(caim)
#' caim <- crop_caim(caim, c(231,334), 15, 10)
#' ncell(caim)
crop_caim <- function(r, upper_left = NULL, width = NULL,
                      height = NULL) {

  .assert_spatraster(r, "r")
  .check_vector(upper_left, "numeric", 2, allow_null = TRUE, sign = "positive")
  .check_vector(width, "numeric", 1, allow_null = TRUE, sign = "positive")
  .check_vector(height, "numeric", 1, allow_null = TRUE, sign = "positive")

  terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
  # https://spatialreference.org/ref/sr-org/7589/
  terra::crs(r) <- "epsg:7589"

  # START code from read_caim
  if (all(!is.null(upper_left), !is.null(height), !is.null(width))) {
    xmn <- terra::xFromCol(r, upper_left[1])
    xmx <- terra::xFromCol(r, upper_left[1] + width)
    ymx <- terra::yFromRow(r, upper_left[2])
    ymn <- terra::yFromRow(r, upper_left[2] + height)

    if (any(is.na(xmn), is.na(xmx), is.na(ymn), is.na(ymx))) {
      stop(
        "The selection is outside the picture border, review `upper_left`, `height`, and `width`."
      )
    }
    e <- terra::ext(xmn, xmx, ymn, ymx)
    r <- terra::crop(r, e)
    terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
  }
  # END code from read_caim()
  r
}
