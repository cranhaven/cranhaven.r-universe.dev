#' Discrete scale constructor for fill argument.
#'
#' Applies a Duke branded and accessible discrete color palette to ggplot
#' geometric objects using fill argument. It prioritizes high contrast colors
#' aligned with Web Content Accessibility Guidelines (WCAG).
#'
#' @param ... Arguments passed on to `discrete_scale`.
#' @param na.value Color used for NA values
#'
#' @return a visualization with discrete duke color scale in fill
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' # default
#' ggplot(penguins, aes(x = species, fill = species)) +
#'   geom_bar()
#'
#' # vs. with Duke scale
#' ggplot(penguins, aes(x = species, fill = species)) +
#'   geom_bar() +
#'   scale_duke_fill_discrete()
#'
#' # Make some species NAs to demonstrate na.value usage
#' penguins_with_NAs <- penguins |>
#'   mutate(species = if_else(species == "Gentoo", NA, species))
#'
#' # with default na.value
#' ggplot(penguins_with_NAs, aes(x = species, fill = species)) +
#'   geom_bar() +
#'   scale_duke_fill_discrete()
#'
#' # with custom na.value
#' ggplot(penguins_with_NAs, aes(x = species, fill = species)) +
#'   geom_bar() +
#'   scale_duke_fill_discrete(na.value = "pink")
scale_duke_fill_discrete <- function(..., na.value = "#B5B5B5") {

  ggplot2::discrete_scale(
    aesthetics = "fill",
    "duke_d_fill",
    duke_pal(),
    na.value = na.value,
    ...
  )

}
