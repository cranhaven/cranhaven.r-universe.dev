#' Correct vignetting effect
#'
#' Apply a vignetting correction to an image using a polynomial model.
#'
#' Vignetting is the gradual reduction of image brightness toward the periphery.
#' This function corrects it by applying a device-specific correction as a
#' function of the zenith angle at each pixel.
#'
#' @inheritParams fisheye_to_equidistant
#'
#' @param lens_coef_v numeric vector. Coefficients of the vignetting function
#'   \eqn{f_v(\theta) = 1 + a\theta + b\theta^2 + \dots + m\theta^n}, where
#'   \eqn{\theta} is the zenith angle (in radians) and \eqn{a,b,\dots,m} are the
#'   polynomial coefficients. Degrees up to 6 are supported. See
#'   [extract_radiometry()] for guidance on estimating these coefficients.
#'
#' @return [terra::SpatRaster-class] with the same content as `r` but with
#'   pixel values adjusted to correct for vignetting, preserving all other
#'   properties (layers, names, extent, and CRS).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file("external/APC_0836.jpg", package = "rcaiman")
#' caim <- read_caim(path)
#' z <- zenith_image(2132, lens("Olloclip"))
#' a <- azimuth_image(z)
#' zenith_colrow <- c(1063, 771)
#'
#' caim <- expand_noncircular(caim, z, zenith_colrow)
#' m <- !is.na(caim$Red) & !is.na(z)
#' caim[!m] <- 0
#'
#' bin <- binarize_with_thr(caim$Blue, thr_isodata(caim$Blue[m]))
#' display_caim(caim$Blue, bin)
#'
#' caim <- invert_gamma_correction(caim, 2.2)
#' caim <- correct_vignetting(caim, z, c(-0.0546, -0.561, 0.22)) %>%
#'         normalize_minmax()

# The lens_coef_v values are from doi:10.1016/j.agrformet.2024.110020
#' }
correct_vignetting <- function(r, z, lens_coef_v) {
  .check_r_z_a_m(r, z, r_type = "any")
  .check_vector(lens_coef_v, "numeric", sign = "any")

  # only to avoid note from check, code is OK without this line.
  a <- b <- d <- e <- f <- NA

  .fv <- function(theta, lens_coef_v) {
    x <- lens_coef_v[1:6]
    x[is.na(x)] <- 0
    for (i in 1:6) assign(letters[i], x[i])
    1 + a * theta + b * theta^2 + c * theta^3 +
      d * theta^4 + e * theta^5 + f * theta^6
  }
  r <- r / .fv(z * pi / 180, lens_coef_v)
  r[is.na(z)] <- 0
  r
}
