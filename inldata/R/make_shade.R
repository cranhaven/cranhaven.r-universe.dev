#' Compute Hill Shade
#'
#' @description Compute hill shade from a digital elevation model.
#'   A hill shade layer is often used as a backdrop on top of which another, semi-transparent, layer is drawn.
#'
#' @param x 'SpatRaster' object.
#'   Spatial raster of land-surface elevations, such as the [`dem`] dataset.
#' @param scale 'numeric' number.
#'   Scaling factor used to convert the elevation values.
#'   The default value is 2.
#' @param neighbors 'numeric' count.
#'   Number of neighboring cells to use to compute slope or aspect with.
#'   Either 8 (queen case) or 4 (rook case).
#' @param angle 'numeric' number.
#'   The sun's angle of elevation above the horizon, ranges from 0 to 90 degrees.
#'   A value of 0 degrees indicates that the sun is on the horizon.
#'   A value of 90 degrees indicates that the sun is directly overhead.
#'   Default is a 40 degree angle.
#' @param direction 'numeric' number.
#'   Direction (azimuth) angle of the light source (sun), in degrees.
#'   An direction of 0 degrees indicates north, east is 90 degrees, south is 180 degrees,
#'   and west is 270 degrees (default).
#' @param normalize 'logical' flag.
#'   Whether to set values below zero to zero and results normalized between 0 and 1.
#'
#' @return Spatial raster of hill shade values.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' elevation <- terra::unwrap(dem)
#' hillshade <- make_shade(elevation)
#' terra::plot(hillshade,
#'   col = inlcolor::get_colors(
#'     n = 256,
#'     scheme = "grayC"
#'   ),
#'   mar = c(2, 2, 1, 4),
#'   legend = FALSE
#' )
#' terra::plot(elevation,
#'   col = inlcolor::get_colors(
#'     n = 256,
#'     scheme = "dem2",
#'     alpha = 0.7,
#'     bias = 0.9
#'   ),
#'   add = TRUE
#' )

make_shade <- function(x,
                       scale = 2,
                       neighbors = 8,
                       angle = 40,
                       direction = 270,
                       normalize = TRUE) {

  # check arguments
  checkmate::assert_class(x, classes = "SpatRaster")
  checkmate::assert_number(scale, lower = 0)
  checkmate::assert_choice(neighbors, choices = c(4, 8))
  checkmate::assert_numeric(angle, lower = 0, upper = 90, finite = TRUE)
  checkmate::assert_numeric(direction, lower = 0, upper = 360, finite = TRUE)
  checkmate::assert_flag(normalize)

  # compute shade
  x[] <- x[] * scale
  slope <- terra::terrain(x, v = "slope", neighbors = neighbors, unit = "radians")
  aspect <- terra::terrain(x, v = "aspect", neighbors = neighbors, unit = "radians")
  shade <- terra::shade(slope, aspect, angle = angle, direction = direction)

  # normalize shade
  if (normalize) {
    is <- is.finite(shade[]) & shade[] < 0
    shade[is] <- 0
    ran <- range(shade[], na.rm = TRUE)
    vec <- seq(ran[1], ran[2], length.out = 255)
    shade[] <- findInterval(shade[], vec) / 255
  }

  shade
}
