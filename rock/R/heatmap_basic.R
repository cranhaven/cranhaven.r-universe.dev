#' Generic convenience function to create a heatmap
#'
#' @param data A data frame
#' @param x,y,fill The variables (columns) in `data` to use for the x axis,
#' y axis, and fill of the heatmap, respectively.
#' @param xLab,yLab,fillLab The labels to use for the x axis, y axis, and fill,
#' respectively
#' @param plotTitle The plot title.
#' @param fillScale The fill scale.
#' @param theme The theme.
#'
#' @return The heatmap, as a ggplot2 object.
#' @export
#'
#' @examples rock::heatmap_basic(mtcars, 'am', 'cyl', 'mpg');
heatmap_basic <- function(data,
                          x,
                          y,
                          fill,
                          xLab = x,
                          yLab = y,
                          fillLab = fill,
                          plotTitle = "Heatmap",
                          fillScale = ggplot2::scale_fill_viridis_c(),
                          theme = ggplot2::theme_minimal()) {

  if (!is.data.frame(data)) {
    stop("As argument `data`, you have to pass a data.frame!");
  }

  if (!(all(c(x, y, fill) %in% names(data)))) {
    stop("Not all of `x`, `y`, and `fill` exist in `data`!");
  }

  heatMap <-
    ggplot2::ggplot(data = data,
                    mapping = ggplot2::aes_string(
                    x = x,
                    y = y,
                    fill = fill)
    ) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_discrete(position = "top") +
    theme +
    fillScale +
    ggplot2::labs(x = xLab,
                  y = yLab,
                  fill = fillLab,
                  title = plotTitle) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1),
      plot.subtitle = ggplot2::element_text(hjust = 1),
      axis.text.x = ggplot2::element_text(angle = 30,
                                          hjust = 0)
    );

  return(heatMap);

}
