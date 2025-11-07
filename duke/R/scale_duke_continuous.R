#' Continuous scale constructor.
#'
#' Applies a Duke branded and accessible continuous color scale to ggplot
#' geometric objects. It is applicable for both fill and color arguments.
#' Defines gradient scale from dark to light to improve visibility and contrast
#' for readers.
#'
#' @param ... Arguments passed on to [continuous_scale()](https://ggplot2.tidyverse.org/reference/continuous_scale.html?q=continuous_scale).
#' @param low Low end of color gradient.
#' @param high High end of color gradient.
#' @param space Color space in which to calculate gradient.
#' @param na.value Color used for NA values.
#' @param guide Type of legend. "colorbar" for continuous scale, "legend" for discrete scale.
#' @param aesthetics String or vector of strings detailing what aesthetic features this continuous scale can apply to.
#'
#' @return a visualization with continuous duke color scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' # default
#' ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = body_mass_g)) +
#'   geom_point()
#'
#' # vs. with Duke scale
#' ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = body_mass_g)) +
#'   geom_point() +
#'   scale_duke_continuous()
scale_duke_continuous <- function(..., low = "#00539B", high = "#E2E6ED", space = "Lab",
                                  na.value = "#666666", guide = "colourbar", aesthetics = c("colour", "color", "fill")) {
  ggplot2::continuous_scale(
    aesthetics, "duke_c",
    scales::seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide, ...
  )
}
