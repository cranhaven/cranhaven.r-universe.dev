#' Select sky region
#'
#' @description
#' Select pixels from a single-layer image based on value limits.
#'
#' @details
#' Works with any numeric [terra::SpatRaster-class] of one layer, but is
#' especially well-suited for images from [zenith_image()] or
#' [azimuth_image()]. For azimuth ranges that wrap around 0 deg, combine two
#' masks with logical OR.
#'
#' @param r single-layer [terra::SpatRaster-class], typically from
#'   [zenith_image()] or [azimuth_image()].
#' @param from,to numeric vectors of length one. Angles in deg, inclusive limits.
#'
#' @return Logical [terra::SpatRaster-class] (`TRUE` for the selected region) of
#'   the same dimensions as `r`.
#'
#' @seealso [paint_with_mask()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' z <- zenith_image(1000, lens())
#' a <- azimuth_image(z)
#' m1 <- select_sky_region(z, 20, 70)
#' plot(m1)
#' m2 <- select_sky_region(a, 330, 360)
#' plot(m2)
#' plot(m1 & m2)
#' plot(m1 | m2)
#'
#' # 15 deg on each side of 0
#' m1 <- select_sky_region(a, 0, 15)
#' m2 <- select_sky_region(a, 345, 360)
#' plot(m1 | m2)
#'
#' # You can use this
#' plot(!is.na(z))
#' # instead of this
#' plot(select_sky_region(z, 0, 90))
#' }
select_sky_region <- function(r, from, to) {
  .assert_single_layer(r)
  .check_vector(from, "numeric", 1, sign = "any")
  .check_vector(to, "numeric", 1, sign = "any")
  if (from > to) stop("`from` must be greater than `to`.")

  if (to >= max(r[], na.rm = TRUE)) {
    c1 <- !is.na(r)
  } else {
    c1 <- !binarize_with_thr(r, to)
  }

  if (from <= min(r[], na.rm = TRUE)) {
    c2 <- !is.na(r)
  } else {
    c2 <- binarize_with_thr(r, from)
  }

  c1 & c2
}

