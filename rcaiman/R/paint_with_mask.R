#' Paint with mask
#'
#' @description
#' Paint image pixels inside or outside a logical mask with a solid color.
#'
#' @param r [terra::SpatRaster-class]. The image. Values should be normalized,
#'   see [normalize_minmax()]. Only images with one or three layers are
#'   supported.
#' @param color character vector of length one or numeric vector of length
#'   three. Fill color. If character, it is converted to RGB automatically. If
#'   numeric, values must be in range \eqn{[0, 1]}.
#' @param where character vector of length one Region to paint relative to `m`.
#'   Either `"outside"` (default) or `"inside"`.
#'
#' @inheritParams compute_canopy_openness
#'
#' @return numeric [terra::SpatRaster-class] with three layers and the geometry
#'   of `r`. Equal to `r`, but with pixels in the selected region painted with
#'   `color`. Single-layer inputs are replicated to allow color painting.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' r <- read_caim()
#' z <- zenith_image(ncol(r), lens())
#' a <- azimuth_image(z)
#' m <- select_sky_region(z, 20, 70) & select_sky_region(a, 90, 180)
#'
#' masked_caim <-  paint_with_mask(normalize_minmax(r), m)
#' plotRGB(masked_caim * 255)
#'
#' masked_bin <- paint_with_mask(binarize_with_thr(r$Blue, 125), m)
#' plotRGB(masked_bin * 255)
#'
#' r <- normalize_minmax(r)
#' paint_with_mask(r, m, color = c(0.2, 0.2, 0.2))  # vector
#' paint_with_mask(r, m, color = "blue")     # name
#' paint_with_mask(r, m, color = "#00FF00")  # hexadecimal
#' }
paint_with_mask <- function(r, m, color = "red", where = "outside") {
  .assert_spatraster(r)
  .assert_logical_mask(m)
  .assert_same_geom(r, m)
  .assert_choice(where, c("outside", "inside"))

  # Convert character color to RGB vector
  if (is.character(color) && length(color) == 1) {
    color <- grDevices::col2rgb(color) / 255
    color <- as.numeric(color)
  }

  if (!(length(color) == 3 && is.vector(color) && is.numeric(color))) {
    stop("`color` must be character vector of length one or numeric vector of length three.")
  }

  if (terra::nlyr(r) == 1) {
    red <- green <- blue <- r
  } else {
    stopifnot(terra::nlyr(r) == 3)
    red   <- terra::subset(r, 1)
    green <- terra::subset(r, 2)
    blue  <- terra::subset(r, 3)
  }

  target <- if (where == "outside") !m else m
  red[target]   <- color[1]
  green[target] <- color[2]
  blue[target]  <- color[3]

  c(red, green, blue)
}



