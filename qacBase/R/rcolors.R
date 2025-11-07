#' @title R Colors
#'
#' @description
#' Plot a grid of R colors and their associated names
#'
#' @param color character. A text string used to search for specific color variations (see examples.)
#' @param cex numeric. text size for color labels.
#' @details
#' By default \code{rcolors} plots the basic 502 distinct colors provided by the
#' \code{colors} function. If a color name or part of a name is provided, only
#' colors with matching names are plotted.
#'
#' @seealso \link{colors}
#' @return
#' No return value, called for side effects
#'
#' @importFrom grDevices col2rgb colors rgb
#' @importFrom graphics par rect text
#'
#' @references
#' This function is adapted from code published by
#' \href{https://github.com/kbroman/broman}{Karl W. Broman}.
#' @export
#' @examples
#' rcolors()
#' rcolors("blue")
#' rcolors("red")
#' rcolors("dark")
#'
rcolors <- function(color=NULL, cex=.6){
  Colors <- colors(distinct=TRUE)
  if(!is.null(color)){
    Colors<- Colors[grepl(color, Colors, ignore.case=TRUE)]
    if(length(Colors)==0) stop("No such color. Check spelling.")
  }

  n <- length(Colors)
  col <- character(n)
  col2hex <- function(cname){
    colMat <- col2rgb(cname)
    rgb(red = colMat[1, ]/255,
        green = colMat[2, ]/255,
        blue = colMat[3, ]/255)
  }
  for(i in seq_along(Colors)){
    col[i] <- col2hex(Colors[i])
  }

  columns<-7
  rows <- ceiling(n/7)

  names(col) <- Colors
  colval <- t(col2rgb(col))
  ord <- hclust(dist(colval))$order
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mar = rep(0.1, 4))
  x <- (1:columns) - 1
  y <- (1:rows) - 1
  x <- rep(x, each = length(y))
  y <- rep(y, length(x))
  plot(0, 0, type = "n", xlab = "", ylab = "", xaxs = "i",
       yaxs = "i", xlim = c(0, max(x) + 1), ylim = c(max(y) +
                                                       0.5, -0.5))
  dx <- 0.2
  dy <- 0.4
  for (i in seq(along = ord)) {
    rect(x[i] + dx/4, y[i] - dy, x[i] + dx, y[i] + dy, border = "black",
         col = col[ord[i]])
    text(x[i] + dx * 1.2, y[i], names(col)[ord[i]], cex = cex,
         adj = c(0, 0.5))
  }
  return(invisible(NULL))
}
