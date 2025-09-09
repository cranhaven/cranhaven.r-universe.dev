#' Render OES Theme
#'
#' Defines the OES plotting theme
#'
#' @author Miles Williams
#'
#' @param base_size An integer specifying a font size; defaults to \code{12}.
#'
#' @param base_family A character string identifying a font family; defaults to
#' the OES-preferred \code{"Lato"}.
#'
#' @param device A character string identifying a plotting device; defaults to
#' \code{"pdf"}.
#'
#' @return A list specifying a ggplot theme. See \code{Details} for more.
#'
#' @details A list of length 93 of classes \code{theme} and \code{gg} that
#' defines elements of the display other than the data. \code{theme_oes()} wraps
#' \code{ggplot2::theme_bw()} and replaces several default values.
#'
#' @import extrafont
#'
#' @export

theme_oes <- function(base_size = 12,
                      base_family = "Lato",
                      device = "pdf") {

  extrafont::loadfonts(device = device, quiet = TRUE)

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.background = element_rect(fill = NA),
      axis.text.x = element_text(color = "black", family = base_family),
      axis.text.y = element_text(color = "black", family = base_family),
      axis.ticks.x = element_blank(),
      axis.line = element_line(colour = "black",
                               size = 0.5),
      legend.title = element_text(size = 14,
                                  face = "italic"),
      legend.text = element_text(size = 14),
      legend.background = element_blank()
    )
}
