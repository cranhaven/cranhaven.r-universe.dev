#' @noRd
theme_greatR <- function(base_size = 12, base_family = "sans") {
  (
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        size = ceiling(base_size * 0.7),
        color = "black"
      ),
      axis.title = ggplot2::element_text(
        size = ceiling(base_size * 0.8)
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        fill = NA,
        color = "black",
        linewidth = 1,
        linetype = "solid"
      ),
      strip.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(
        size = ceiling(base_size * 0.75)
      ),
      legend.position = "right",
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(
        size = ceiling(base_size * 1.1), face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        size = ceiling(base_size * 1.05)
      )
    )
  )
}

#' @noRd
greatR_palettes <- list(
  cont = c("#3e1190", "#4268b7", "#4195bd", "#51bdb9", "#8fdc9f", "#f9ef93"),
  disc = c("#1b9e77", "#f38400"),
  hist = c("REG" = "#4268b7", "NON-REG" = "#d98484")
)
