#' Build azimuth image
#'
#' Creates a single-layer raster in which pixel values represent azimuth
#' angles, assuming an upwards-looking hemispherical photograph with the
#' optical axis vertically aligned.
#'
#' @inheritParams sky_grid_segmentation
#'
#' @param orientation numeric vector of length one. Azimuth angle (in degrees)
#'   corresponding to the direction at which the top of the image was pointing
#'   when the picture was taken. This design follows the common field protocol
#'   of recording the angle at which the top of the camera points.
#'
#' @return [terra::SpatRaster-class] with the same dimensions as the input
#'   zenith image. Each pixel contains the azimuth angle in degrees, with zero
#'   representing North and angles increasing counter-clockwise. The
#'   object carries attributes `orientation` and `lens_coef`.
#'
#' @note
#' If `orientation = 0`, North (0 deg) is located at the top of the image, as in
#' conventional maps, but East (90 deg) and West (270 deg) appear flipped
#' relative to maps. To understand this, take two flash-card-sized pieces of
#' paper. Place one on a table in front of you and draw a compass rose on it.
#' Hold the other above your head, with the side facing down toward you, and
#' draw another compass rose following the directions from the one on the table.
#' This mimics the situation of taking an upwards-looking photo with a
#' smartphone while viewing the screen, and it will result in a mirrored
#' arrangement. Compare both drawings to see the inversion.
#'
#'
#' @export
#'
#' @examples
#' z <- zenith_image(600, lens("Nikon_FCE9"))
#' a <- azimuth_image(z)
#' plot(a)
#' \dontrun{
#' a <- azimuth_image(z, 45)
#' plot(a)
#' }
azimuth_image <- function (z, orientation = 0) {
  .assert_single_layer(z, "z")
  .check_vector(orientation, "numeric", 1, sign = "any")

  m <- is.na(z)

  xy <- terra::xyFromCell(z, seq(length = ncell(z)))
  sph <- pracma::cart2sph(
    matrix(c(xy[, 1] - ncol(z) / 2,
             xy[, 2] - ncol(z) / 2,
             terra::values(z)), ncol = 3)
  )

  values(z) <- sph[, 1] * 180 / pi
  values(z) <- terra::values(abs(terra::trans(z) - 180))
  # above line is to orient North up and West left

  if (orientation != 0) {
    if (!requireNamespace("imager", quietly = TRUE)) {
      stop(paste("Package \"imager\" needed for this function to work.",
                 "Please install it."),
           call. = FALSE)
    }
    picture_cw_rotation <- -orientation
    v <- imager::as.cimg(as.array(z)) %>% suppressWarnings()
    v <- imager::rotate_xy(v, -picture_cw_rotation,
                           ncol(z) / 2, nrow(z) / 2, interpolation = 0)
    terra::values(z) <- as.matrix(v)
  }

  z[m] <- NA
  names(z) <- "Azimuth image"
  attr(z, "orientation") <- orientation
  z
}
