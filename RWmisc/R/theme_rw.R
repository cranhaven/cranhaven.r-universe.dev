#' Blank Theme
#'
#' A ggplot theme with no grid elements or gray background.
#'
#' @return A ggplot [theme][ggplot2::theme] object.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = hp, y = mpg)) +
#' ggplot2::geom_point() +
#' theme_rw()
theme_rw <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(plot.background = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank())
}
