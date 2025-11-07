#' Implements theme components.
#'
#' Defines the overall aesthetic and thematic features of the plot. This
#' function specifies simple background, grid line, text, and legend arguments
#' to create minimalist design. Its use is intended for ggplot objects.
#'
#' @param base_size the base size
#' @param base_family the base family
#' @param base_line_size the baseline size
#' @param base_rect_size the base rect
#'
#' @return a plot with Duke colors
#' @export
#' @importFrom ggplot2 '%+replace%'
#' @importFrom ggplot2 'margin'
#' @importFrom ggplot2 'element_text'
#' @examples
#'
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' # default
#' p <- ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
#'   geom_point() +
#'   labs(
#'     title = "Bill length and depth of penguins",
#'     subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
#'     x = "Bill depth (mm)",
#'     y = "Bill length (mm)",
#'     color = "Species",
#'     caption = "Source: palmerpenguins package."
#'   )
#' p
#'
#' # vs. with Duke theme
#' p +
#'   theme_duke()
#'
#' # vs. with Duke theme and scale
#' p +
#'   scale_duke_color_discrete() +
#'   theme_duke()
#'
#' # with Duke theme, scale, and further customization to theme
#' p +
#'   scale_duke_color_discrete() +
#'   theme_duke() +
#'   theme(
#'     plot.title = element_text(color = "red", size = 20),
#'     plot.background = element_rect(fill = "pink", color = "yellow"),
#'     panel.grid = element_blank()
#'   )
theme_duke <- function(
    base_size = 11,
    base_family = "",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22
    ) {

  # Define fonts
  base_family <- base_family

  # Define half_line
  half_line <- base_size / 2

  # Starts with v and then modify some parts
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(

      # title text
      plot.title = ggplot2::element_text(
        hjust = 0, vjust = 1,
        margin = ggplot2::margin(b = half_line),
        family = base_family, face = "bold",
        colour = "#00539B"
      ),

      # subtitle text
      plot.subtitle = ggplot2::element_text(
        hjust = 0, vjust = 1,
        margin = ggplot2::margin(b = half_line),
        family = base_family, colour = "#00539B"
      ),

      # caption text
      plot.caption = ggplot2::element_text(
        hjust = 1, vjust = 1,
        margin = ggplot2::margin(t = half_line),
        family = base_family, colour = "#00539B"
      ),

      # axis title and text
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = half_line / 2),
        vjust = -0.5,
        family = base_family, colour = "#00539B"
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = half_line / 2),
        vjust = 1,
        angle = 90,
        family = base_family, colour = "#00539B"
      ),
      axis.text = ggplot2::element_text(
        colour = "#012169"
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 0.8 * half_line / 2),
        vjust = 1,
        family = base_family
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.8 * half_line / 2),
        hjust = 1,
        family = base_family
      ),

      # legend title and text
      legend.text = ggplot2::element_text(
        family = base_family,
        color = "#00539B"
      ),
      legend.title = ggplot2::element_text(
        family = base_family,
        color = "#00539B"
      ),

      # background panel for facet plot titles
      strip.background = ggplot2::element_rect(fill = "#E2E6ED", color = "#E2E6ED"),

      # text for facet plots
      strip.text = ggplot2::element_text(
        margin = ggplot2::margin(t = half_line / 2, b = half_line / 2),
        family = base_family,
        colour = "#012169"
      ),
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = half_line / 2, b = half_line / 2),
        family = base_family,
        colour = "#012169"
      ),
      strip.text.y = ggplot2::element_text(
        margin = ggplot2::margin(t = half_line / 2, b = half_line / 2),
        family = base_family,
        colour = "#012169"
      ),

      complete = TRUE
    )
}
