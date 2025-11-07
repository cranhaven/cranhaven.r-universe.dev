#' Discrete scale constructor for color argument.
#'
#' Applies a Duke branded and accessible discrete color palette to ggplot
#' geometric objects using color argument. It prioritizes high contrast colors
#' aligned with Web Content Accessibility Guidelines (WCAG).
#'
#' Partial code for this function can be attributed to
#' [ggthemes](https://github.com/jrnold/ggthemes/blob/main/R/colorblind.R).
#'
#' @param ... Arguments passed on to `discrete_scale`.
#' @param na.value Color used for NA values
#'
#' @return a visualization with discrete duke color scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' # default
#' ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
#'   geom_point()
#'
#' # vs. with Duke scale
#' ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
#'   geom_point() +
#'   scale_duke_color_discrete()
#'
#' # vs. with shape and Duke scale
#' ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, shape = species, color = species)) +
#'   geom_point() +
#'   scale_duke_color_discrete()
#'
#' # vs. with Duke scale, UK spelling
#' ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, colour = species)) +
#'   geom_point() +
#'   scale_duke_colour_discrete()
#'
#' # Make some species NAs to demonstrate na.value usage
#' penguins_with_NAs <- penguins |>
#'   mutate(species = if_else(species == "Gentoo", NA, species))
#'
#' # with default na.value
#' ggplot(penguins_with_NAs, aes(x = body_mass_g, color = species)) +
#'   geom_density() +
#'   scale_duke_color_discrete()
#'
#' # with custom na.value
#' ggplot(penguins_with_NAs, aes(x = body_mass_g, color = species)) +
#'   geom_density() +
#'   scale_duke_color_discrete(na.value = "pink")
scale_duke_color_discrete <- function(..., na.value = "#B5B5B5") {

  ggplot2::discrete_scale(
    aesthetics = "color",
    "duke_d_color",
    duke_pal(),
    na.value = na.value,
    ...
  )

}

#' @rdname scale_duke_color_discrete
#' @export
scale_duke_colour_discrete <- scale_duke_color_discrete
