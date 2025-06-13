#' @title Use ggplot to draw ruler
#' @description You can draw a ruler that uses ggplot2.
#' Also It's funny and you can get a lot of methods from
#'  this function.
#' @param len the length of ruler
#' @importFrom  ggplot2 ggplot geom_text geom_rect  geom_label
#' geom_linerange  annotate theme_void aes
#' @author Chai
#' @return A ggplot2 object.
#' @export
show_ruler <- function(len = 5) {
  len <- floor(len)
  d1 <- data.frame(
    x = 1:len,
    y = rep(c(0, 2), each = len)
  )
  d2 <- data.frame(
    x = seq(0, len, 0.1),
    y = rep(c(0, 2), each = 10 * len + 1)
  )
  d3 <- data.frame(
    x = seq(0.5, len, 1),
    y = rep(c(0, 2), each = len)
  )
  d4 <- data.frame(
    x = seq(0.05, len, 0.1),
    y = rep(c(0, 2), each = len * 10)
  )
  x <- 1:len
  y <- 1
  h <- 0.6
  h1 <- 0.3
  h2 <- 0.45
  h3 <- 0.2
  y0 <- rep(c(h, -h), each = len)
  y1 <- rep(c(h1, -h1), each = 10 * len + 1)
  y2 <- rep(c(h2, -h2), each = len)
  y3 <- rep(c(h3, -h3), each = len * 10)
  ggplot() +
    geom_linerange(
      aes(
        x = x, ymin = y,
        ymax = y + y1
      ),
      d2
    ) +
    geom_linerange(
      aes(
        x = x, ymin = y,
        ymax = y + y0
      ),
      d1
    ) +
    geom_linerange(
      aes(
        x = x, ymin = y,
        ymax = y + y2
      ),
      d3
    ) +
    geom_linerange(
      aes(
        x = x, ymin = y,
        ymax = y + y3
      ),
      d4
    ) +
    geom_text(aes(x, y, label = x),
      family = "sans",
      size = 10, color = "brown"
    ) +
    annotate("text",
      x = 0.5, y = 1.5,
      label = "0.5mm"
    ) +
    annotate("text",
      x = 0.25, y = 0.45,
      label = "mm"
    ) +
    theme_void() -> p
  return(p)
}
