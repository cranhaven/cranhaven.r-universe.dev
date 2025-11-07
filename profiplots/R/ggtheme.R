#' Profinit ggplot theme
#'
#' The current version of the theme is based on `ggplot2::theme_minimal()` not
#' tweaking any of its settings. Later on, the theme will be customised to
#' better fill our needs and preferences.
#'
#' @export
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @seealso [scale_fill_profinit] and [scale_color_profinit] for Profinit color scales.
#' @returns Ggplot2's theme object adjusted according to Profinit's preferences.
#' @examples
#' # default behavior
#' library(ggplot2)
#' iris_plt <- ggplot(iris, aes(x=Sepal.Length, fill=Species)) +
#'   geom_density(alpha=.5) +
#'   scale_fill_profinit()
#' iris_plt
#'
#' # With profinit theme
#' iris_plt + theme_profinit()
#'
theme_profinit <- function() {
  reference_theme <- ggplot2::theme_minimal()
  theme_adjustment <- ggplot2::theme(
    plot.caption = ggplot2::element_text(colour = "darkgray")
    # TO BE DEFINED
  )
  ggplot2::`%+replace%`(reference_theme, theme_adjustment)
}
