#' @title An easy way to show  colors in ggplot2
#' @description the same function can see
#'  \code{\link[scales]{show_col}}, but it is a
#'  ggplot2 object. You can use it like the show_col()
#'  function in scales package, but it can save by ggsave() function.
#' @param  ncol the number f col
#' @param byrow logical
#' @param colors string about colors
#' @param label logical
#' @param number logical, the default is
#' @param size the size of label, the default is 1.
#' @param border  The color of  border

#' @export  show_color
#' @importFrom  ggplot2 ggplot geom_text geom_rect  geom_label
#'   theme_void
#' @importFrom grDevices rgb2hsv  col2rgb
#' @author Chai
#++++++++++++++++++++++++++
show_color <- function(colors, ncol, byrow = TRUE,
                       label = FALSE, number = FALSE, size = 1,
                       border = "black") {
  len <- length(colors)
  nrow <- ifelse(len %% ncol == 0, len / ncol, len %/% ncol + 1)
  n <- nrow * ncol
  s <- c(colors, rep("white", abs(n - len)))
  x <- numeric(n)
  y <- numeric(n)
  if (byrow == TRUE) {
    x <- rep(seq(1, by = 1, length.out = ncol), nrow)
    y <- rep(seq(-1, by = -1, length.out = nrow), each = ncol)
  } else {
    x <- rep(seq(1, by = 1, length.out = ncol), each = nrow)
    y <- rep(seq(-1, by = -1, length.out = nrow), ncol)
  }
  x2 <- x
  y2 <- y + 1
  d <- data.frame(
    x = x - 1, y = y,
    x2, y2,
    col = s
  )
  ggplot() +
    geom_rect(
      data = d, aes(
        xmin = x, xmax = x2,
        ymin = y, ymax = y2,
        fill = I(s)
      ),
      colour = border
    ) +
    theme_void() -> p

  if (label == TRUE) {
    col_v <- rgb2hsv(col2rgb(colors))[3, ]
    v <- ifelse(col_v > 0.6, "black", "white")
    v <- append(v, rep("white", abs(n - len)))
    l <- s
    col2 <- v
    d1 <- data.frame(
      x = x - 0.5, y = y + 0.5,
      l, col2
    )
    if (number == TRUE) {
      d1$l <- append(1:len, rep(0, abs(n - len)))
    }

    p <- p +
      geom_text(aes(x, y, label = l, color = I(col2)),
        data = d1,
        size = 3.5 * size, fontface = "bold"
      )
  }
  return(p)
}
