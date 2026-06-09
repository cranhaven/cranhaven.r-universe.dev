#' Generate conventional-lens-like image
#'
#' @description
#' Create an RGB image that resembles a photo taken with a conventional lens,
#' using a small patch from the example hemispherical image.
#'
#' @details
#' This is a fixed crop and reorientation of `read_caim()`. It does not perform
#' any re-projection. Intended for documentating functions.
#'
#' The following code was used to define the region:
#' ``````````````````
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#'
#' m <- rast(z)
#' m[] <- calc_spherical_distance(
#'   z[] * pi / 180,
#'   a[] * pi / 180,
#'   1,# hinge-angle
#'   90 * pi / 180
#' )
#' m <- !binarize_with_thr(m, 30 * pi / 180)
#' m[is.na(z)] <- 0
#' m
#'
#' x11()
#' plot(m * caim$Blue)
#' za <- click(c(z, a))
#' za
#' row_col <- row_col_from_zenith_azimuth(z, a, za[,1], za[,2])
#' plot(caim$Blue)
#' points(row_col$col, nrow(caim) - row_col$row, col = 2, pch = 10)
#' mn_y <- min(nrow(caim) -row_col$row)
#' mx_y <- max(nrow(caim) -row_col$row)
#' mn_x <- min(row_col$col)
#' mx_x <- max(row_col$col)
#' r <- terra::crop(caim$Blue, terra::ext(mn_x, mx_x, mn_y, mx_y))
#' plot(r)
#' ``````````````````
#'
#' @return Three-layer [terra::SpatRaster-class] with bands in RGB order.
#'
#' @export
#'
#' @seealso [read_caim()]
#'
#' @examples
#' conventional_lens_image()
conventional_lens_image <- function() {
  caim <- read_caim()
  r <- terra::crop(caim, terra::ext(29, 195, 191, 400))
  r <- terra::t(rev(r))
  r
}

