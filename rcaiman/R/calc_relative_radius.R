#' Calculate relative radius
#'
#' Convert zenith angles (degrees) to normalized radial distance using the lens
#' projection model.
#'
#' This helper maps zenith angle(s) to a relative radius in \[0, 1\] given the
#' lens projection coefficients.
#'
#' @param angle numeric vector. Zenith angles in degrees.
#'
#' @inheritParams zenith_image
#'
#' @returns Numeric vector of the same length as `angle`, constrained to \[0, 1\].
#'
#' @export
#'
#' @examples
#' calc_relative_radius(45, lens())
calc_relative_radius <- function(angle, lens_coef) {
  .check_vector(angle, "numeric", sign = "any")
  .check_vector(lens_coef, "numeric", sign = "any")

  angle <- .degree2radian(angle)
  powers <- seq_along(lens_coef)
  terms <- vapply(powers, function(p) lens_coef[p] * angle^p, numeric(length(angle)))

  relative_radius <- if (length(lens_coef) == 1) {
    terms
  } else {
    if (length(angle) > 1) {
      rowSums(terms)
    } else {
      sum(terms)
    }
  }
  as.vector(relative_radius)
}
