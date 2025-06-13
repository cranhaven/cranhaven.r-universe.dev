#' @description You can use plot_table to  draw table in ggplot2,
#' but it only applies to expressions(see \code{\link[base]{expression}}).
#' For more information , you can see \code{\link[grDevices]{plotmath}}.
#' But it's a ggplot2 object!
#' @param  Str some expressions
#' @param  ncol the number f col
#' @param byrow logical, the default is
#' @title Use ggplot2 to plot a table

#' @return  It will output a ggplot object that
#' contains a table.
#' @importFrom  ggplot2 ggplot geom_text geom_rect  geom_label
#' geom_linerange  annotate theme_void  scale_y_reverse
#' scale_fill_discrete  scale_colour_discrete aes
#'  geom_segment
#' @author Chai
#' @examples
#' a1 <- c(
#'   "x %*% y", "x %/% y", "alpha", "sigma", "beta",
#'   "x == y", "frac(x,y)", "x %up% y", "hat(x)",
#'   "symbol(a)", "underline(x)"
#' )
#' plot_table(a1, 2)
#' plot_table(a1, 3)
#' @export
plot_table <- function(Str, ncol, byrow = TRUE) {
  len <- length(Str)
  row_col <- vector("list", 2)
  names(row_col) <- c("row", "col")
  nrow <- ifelse(len %% ncol == 0, len / ncol, len %/% ncol + 1)
  n <- nrow * ncol
  Str <- c(Str, rep("", abs(n - len)))
  x <- numeric(n)
  y <- numeric(n)
  if (byrow == TRUE) {
    x <- rep(seq(1, by = 2, length.out = ncol), nrow)
    y <- rep(seq(-1, by = -1, length.out = nrow), each = ncol)
  } else {
    x <- rep(seq(1, by = 2, length.out = ncol), each = nrow)
    y <- rep(seq(-1, by = -1, length.out = nrow), ncol)
  }

  y1 <- rep(0, ncol * 2)
  x1 <- seq(1, by = 1, length.out = ncol * 2)
  ite <- rep(c("expression", "effect"), ncol)
  d <- data.frame(x = x, y = y, Str = Str)

  d1 <- data.frame(x1 = x1, y1 = y1, ite = ite)

  x3 <- c(
    seq(0.5, by = 1, length.out = ncol * 2 + 1),
    rep(0.5, nrow + 2)
  )
  y3 <- c(
    rep(0.5, ncol * 2 + 1),
    seq(0.5, by = -1, length.out = nrow + 2)
  )
  xend <- c(
    seq(0.5, by = 1, length.out = ncol * 2 + 1),
    rep(ncol * 2 + 0.5, nrow + 2)
  )
  yend <- c(
    rep(-(nrow + 0.5), ncol * 2 + 1),
    seq(0.5, by = -1, length.out = nrow + 2)
  )
  d2 <- data.frame(
    x = x3, y = y3,
    xend = xend, yend = yend
  )

  ggplot(d, aes(x, y)) +
    geom_text(aes(label = Str)) +
    geom_text(aes(x = x + 1, y = y, label = Str),
      parse = TRUE, colour = "blue"
    ) +
    geom_text(aes(x1, y1, label = ite), d1,
      fontface = "bold"
    ) +
    geom_segment(
      aes(x, y, xend = xend, yend = yend),
      d2
    ) +
    theme_void() -> p
  return(p)
}

#++++++++++++++++++++++++
