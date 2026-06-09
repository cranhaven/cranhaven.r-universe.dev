#' Gamma back correction of JPEG images
#'
#' @description Approximates the inversion of the gamma encoding applied to JPEG
#' images.
#'
#' @details Digital cameras typically encode images using the sRGB color space,
#' which applies a non-linear transformation—commonly referred to as gamma
#' correction—to the sensor's linear luminance response. This function applies a
#' power transformation to approximate the inverse of that encoding, restoring a
#' response closer to linear.
#'
#' @param dn numeric vector or [terra::SpatRaster-class]. Digital
#'   numbers from a JPEG file (range 0–255, as per standard 8-bit encoding).
#' @param gamma numeric vector of length one. Exponent applied in the inverse
#'   gamma correction (typically 2.2 for sRGB).
#'
#' @returns Same properties as `dn`, with values adjusted by
#'   inverse gamma correction and rescaled to the range \eqn{[0, 1]}.
#'
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' path <- system.file("external/APC_0836.jpg", package = "rcaiman")
#' caim <- read_caim(path)
#' z <- zenith_image(2132,  lens("Olloclip"))
#' a <- azimuth_image(z)
#' zenith_colrow <- c(1063, 771)
#'
#' caim <- expand_noncircular(caim, z, zenith_colrow)
#' m <- !is.na(caim$Red) & !is.na(z)
#' caim[!m] <- 0
#'
#' bin <- binarize_with_thr(caim$Blue, thr_isodata(caim$Blue[m]))
#'
#' display_caim(caim$Blue, bin)
#'
#' caim <- invert_gamma_correction(caim, 2.2)
#' }
invert_gamma_correction <- function(dn, gamma = 2.2) {
  .check_vector(gamma, "numeric", 1, sign = "positive")
  handling_dn <- c(
    tryCatch(.check_vector(dn, "numeric", sign = "any"),
             error = function(e) FALSE),
    tryCatch(.assert_spatraster(dn),
             error = function(e) FALSE)
  )
  if (!any(handling_dn)) {
    stop("`dn` must be either numeric vector or SpatRaster class.")
  }
  (dn / 255)^gamma
}
