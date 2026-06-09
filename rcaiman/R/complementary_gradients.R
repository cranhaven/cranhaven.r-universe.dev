#' Calculate complementary gradients
#'
#' @description
#' Compute three color-opponent gradients to enhance the visual separation
#' between sky and canopy in hemispherical photographs, particularly under
#' diffuse light or complex cloud patterns.
#'
#' @details
#' The method exploits chromatic differences between the red, green, and blue
#' bands, following a simplified opponent-color logic. Each gradient is
#' normalized by total brightness and modulated by a logistic contrast
#' function to reduce the influence of underexposed regions:
#'
#' - `"green_magenta"` = \eqn{(R - G + B) / (R + G + B)} · logistic(brightness)
#' - `"yellow_blue"`   = \eqn{(-R - G + B) / (R + G + B)} · logistic(brightness)
#' - `"red_cyan"`      = \eqn{(-R + G + B) / (R + G + B)} · logistic(brightness)
#'
#' The `logistic(brightness)` term is computed as:
#' \deqn{
#' \text{logistic}(x) = \frac{1}{1 + \exp\left(-\frac{x - q_{0.1}}{\mathrm{IQR}}\right)}
#' }
#' where \eqn{q_{0.1}} is the 10th percentile of brightness values
#' (\eqn{x = R + G + B}), and \eqn{IQR} is their interquartile range.
#'
#' This weighting suppresses gradients in poorly exposed regions to reduce
#' spurious values caused by low signal-to-noise ratios.
#'
#' @note
#' This function is part of a paper under preparation.
#'
#' @param caim numeric [terra::SpatRaster-class] with three layers named
#'   `"Red"`, `"Green"`, and `"Blue"`. Digital numbers should be linearly
#'   related to radiance. See [read_caim_raw()] for details.
#'
#' @return Numeric [terra::SpatRaster-class] with three layers and the same
#'   geometry as `caim`. The layers (`"green_magenta"`, `"yellow_blue"`,
#'   `"red_cyan"`) are chromatic gradients modulated by brightness.
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' com <- complementary_gradients(caim)
#' plot(com)
#' }
complementary_gradients <- function(caim) {
  .assert_rgb3(caim)

  R <- caim$Red
  G <- caim$Green
  B <- caim$Blue
  brightness <- R + G + B

  thr <- stats::quantile(brightness[brightness != 0], 0.1, na.rm = TRUE)
  low_exposure <- terra::rast(R)
  low_exposure[] <- stats::plogis(
    brightness[],
    thr,
    stats::IQR(brightness[], na.rm = TRUE)
  )

  green_magenta <- (R - G + B) / brightness * low_exposure
  yellow_blue <- (-R - G + B) / brightness * low_exposure
  red_cyan <- (-R + G + B) / brightness * low_exposure

  names(green_magenta) <- "green_magenta"
  names(yellow_blue) <- "yellow_blue"
  names(red_cyan) <- "red_cyan"
  c(green_magenta, yellow_blue, red_cyan)
}

