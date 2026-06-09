#' Calculate canopy openness
#'
#' Calculate canopy openness from a binarized hemispherical image with angular
#' coordinates.
#'
#' Canopy openness is computed following the equation from
#' \insertCite{Gonsamo2011;textual}{rcaiman}:
#'
#' \deqn{
#' CO = \sum_{i = 1}^{N} GF(\phi_i, \theta_i) \cdot \frac{\cos(\theta_{1,i}) -
#'  \cos(\theta_{2,i})}{n_i}
#' }
#'
#' where \eqn{GF(\phi_i, \theta_i)} is the gap fraction in cell \eqn{i},
#' \eqn{\theta_{1,i}} and \eqn{\theta_{2,i}} are the lower and upper zenith
#' angles of the cell, \eqn{n_i} is the number of cells in the corresponding
#' zenith ring, and \eqn{N} is the total number of cells.
#'
#' When a mask is provided via the `m` argument, the equation is adjusted to
#' compensate for the reduced area of the sky vault:
#'
#' \deqn{
#' CO = \frac{\sum_{i=1}^{N} GF(\phi_i, \theta_i) \cdot w_i}
#' {\sum_{i=1}^{N} w_i}
#' \quad \text{with} \quad
#' w_i = \frac{\cos(\theta_{1,i}) - \cos(\theta_{2,i})}{n_i}
#' }
#'
#' The denominator ensures that the resulting openness value remains
#' scale-independent. Without this normalization, masking would lead to
#' underestimation, as the numerator alone assumes full hemispherical coverage.
#'
#' @param bin logical [terra::SpatRaster-class] with one layer. A binarized
#'   hemispherical image. See [binarize_with_thr()] for details.
#' @param m logical [terra::SpatRaster-class] with one layer. A binary mask with
#'   `TRUE` for selected pixels.
#'
#' @inheritParams sky_grid_segmentation
#'
#' @return Numeric vector of length one, constrained to the range \eqn{[0, 1]}.
#'
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' m <- select_sky_region(z, 0, 70)
#' bin <- binarize_with_thr(caim$Blue, thr_isodata(caim$Blue[m]))
#' plot(bin)
#' compute_canopy_openness(bin, z, a, m, 10)
compute_canopy_openness <- function(bin, z, a, m = NULL, angle_width = 10) {
  .assert_logical_mask(bin)
  .check_r_z_a_m(NULL, z, a, m)
  # angle_width is check by sky_grid_segmentation

  g <- sky_grid_segmentation(z, a, angle_width)
  g[!m] <- 0
  ds <- extract_feature(bin, g, return = "vector")
  ids <- .decode_label(as.numeric(names(ds)))
  mx_angles <- ids$ring_ID * angle_width * pi / 180
  mn_angles <- mx_angles - angle_width * pi / 180
  .n <- 360 / angle_width
  if (is.null(m)) {
    sum(ds * ((cos(mn_angles) - cos(mx_angles)) / .n))
  } else {
    sum(ds * ((cos(mn_angles) - cos(mx_angles)) / .n)) /
      sum((cos(mn_angles) - cos(mx_angles)) / .n)
  }
}
