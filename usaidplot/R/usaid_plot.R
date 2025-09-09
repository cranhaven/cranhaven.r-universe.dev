#' Create plots with the United States Agency for International Development's color palette
#'
#' @param data_type A value to denote either "discrete" or "continuous" data are being graphed. "discrete" is the default.
#' @param ppt A TRUE or FALSE option that changes the plot and facet background to match USAID's PowerPoint template colors.
#' @return Returns a ggplot2 theme
#'
#' @import ggplot2
#' @import extrafont
#' @import extrafontdb
#' @keywords usaid_plot
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#' geom_point(aes(fill = factor(gear)), shape = 21, stroke = 1, col = "white", size = 6) +
#' usaid_plot()
#'


usaid_plot <- function(data_type = "discrete", ppt = FALSE) {
  is_windows <- tolower(Sys.info()[['sysname']]) == "windows"

  font_family <- ifelse(is_windows==TRUE, "Gill Sans MT", "Gill Sans")

  theme_settings <- ggplot2::theme(
    legend.position = "NA",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#CFCDC9"),
    panel.grid.major.x = ggplot2::element_line(color = "#CFCDC9"),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, family = font_family, color = "black"),
    plot.subtitle = element_text(size = 20, family = font_family, color = "black"),
    plot.caption = element_text(size = 16, family = font_family, color = "black"),
    text = element_text(size = 22, family = font_family, color = "black")
  )

  if (ppt) {
    theme_settings$plot.background <- ggplot2::element_rect(fill = '#E7E7E5', colour = '#E7E7E5')
    theme_settings$strip.background <- ggplot2::element_rect(fill = "#E7E7E5")
    theme_settings$legend.key = element_rect(fill = "#E7E7E5")
  }

  scales <- list(theme = theme_settings)

  if (data_type == "discrete") {
    scales <- c(
      scales,
      ggplot2::scale_fill_manual(values = rep(c("#002F6C", "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED", "#8C8985"), 500)),
      ggplot2::scale_color_manual(values = rep(c("#002F6C", "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED", "#8C8985"), 500))
    )
  } else if (data_type == "continuous") {
    scales <- c(
      scales,
      ggplot2::scale_fill_gradient(low = "#002F6C", high = "#BA0C2F"),
      ggplot2::scale_color_gradient(low = "#002F6C", high = "#BA0C2F")
    )
  }

  return(scales)
}
