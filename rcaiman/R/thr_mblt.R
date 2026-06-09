#' Compute model-based thresholds
#'
#' @description
#' Compute threshold values from background digital numbers (DN) using
#' Equation 1 in \insertCite{Diaz2018;textual}{rcaiman}, a linear function
#' whose slope can be weighted.
#'
#' @details
#' The model was derived from canopy targets (perforated, rigid, dark
#' surfaces) backlit under homogeneous illumination, photographed with a
#' Nikon Coolpix 5700 in JPEG mode. Images were gamma-back-corrected with
#' a default gamma of 2.2 (see [invert_gamma_correction()]). Results showed that the optimal
#' threshold is linearly related to the background DN (see Figures 1 and 7
#' in \insertCite{Diaz2018;textual}{rcaiman}). This shifted the goal from
#' estimating an optimal threshold \insertCite{Song2014;textual}{rcaiman} to
#' estimating the background DN as if the canopy were absent, as proposed by
#' \insertCite{Lang2010;textual}{rcaiman}.
#'
#' To apply the weighting parameter (w) from Equation 1, supply `slope` as
#' \eqn{slope \times w}.
#'
#' Equation 1 was developed with 8-bit images. New coefficients should be
#' calibrated in the 0–255 domain, which is what [thr_mblt()] expects, even
#' though the `dn` argument must be normalized. This design choice harmonizes
#' behavior across the package.
#'
#' @note
#' Users are encouraged to acquire raw files (see [read_caim_raw()]).
#'
#' @param dn numeric vector or [terra::SpatRaster-class]. Background digital
#'   number. Values must be normalized; if taken from JPEG, apply gamma back
#'   correction.
#' @param intercept,slope numeric vectors of length one. Linear coefficients.
#'
#' @return An object of the same class and dimensions as `dn`.
#'
#' @seealso [normalize_minmax()], [invert_gamma_correction()]
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#' thr_mblt(invert_gamma_correction(125), -7.8, 0.95 * 0.5)
thr_mblt <- function (dn, intercept, slope) {
  handling_dn <- c(
    tryCatch(.check_vector(dn, "numeric", sign = "any"),
             error = function(e) FALSE),
    tryCatch(.assert_single_layer(dn),
             error = function(e) FALSE)
  )
  if (!any(handling_dn)) {
    stop("`dn` must be either numeric vector or SpatRaster class.")
  }
  .check_vector(intercept, "numeric", 1, sign = "any")
  .check_vector(slope, "numeric", 1, sign = "positive")

  dn <- dn * 255

  if (.get_max(dn) > 255) warning("\"dn\" values should be normalized")
  dn[dn > 255] <- 255

  thr <- intercept  + slope * dn
  thr[thr < 0] <- 0
  thr[is.na(thr)] <- 0

  thr / 255
}
