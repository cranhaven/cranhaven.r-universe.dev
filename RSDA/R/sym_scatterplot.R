#' Symbolic Scatter Plot
#' @name sym.scatterplot
#' @aliases sym.scatterplot
#' @author Oldemar Rodriguez Rojas
#' @description This function could be use to plot two symbolic variables in a X-Y plane.
#' @usage sym.scatterplot(sym.var.x, sym.var.y, labels = FALSE, ...)
#' @param sym.var.x First symbolic variable
#' @param sym.var.y Second symbolic variable.
#' @param labels As in R plot function.
#' @param ... As in R plot function.
#'
#' @return Return a graphics.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @seealso sym.scatterplot3d
#' @examples
#' \dontrun{
#' data(example3)
#' sym.data <- example3
#' sym.scatterplot(sym.data[, 3], sym.data[, 7], col = "blue", main = "Main Title")
#' sym.scatterplot(sym.data[, 1], sym.data[, 4],
#'   labels = TRUE, col = "blue",
#'   main = "Main Title"
#' )
#' sym.scatterplot(sym.data[, 2], sym.data[, 6],
#'   labels = TRUE,
#'   col = "red", main = "Main Title", lwd = 3
#' )
#'
#' data(oils)
#' sym.scatterplot(oils[, 2], oils[, 3],
#'   labels = TRUE,
#'   col = "red", main = "Oils Data"
#' )
#' data(lynne1)
#'
#' sym.scatterplot(lynne1[, 2], lynne1[, 1],
#'   labels = TRUE,
#'   col = "red", main = "Lynne Data"
#' )
#' }
#' @keywords Symbolic Plot
#' @importFrom graphics plot rect text
#' @export
#'
sym.scatterplot <- function(sym.var.x, sym.var.y, labels = FALSE, ...) {
  sym.var.x <- to.v2(sym.var.x)
  sym.var.y <- to.v2(sym.var.y)
  if (((sym.var.x$sym.var.types != "$C") || (sym.var.y$sym.var.types != "$C")) &&
    ((sym.var.x$sym.var.types != "$I") || (sym.var.y$sym.var.types != "$I"))) {
    stop("Impossible to plot this type of variable")
  }


  if ((sym.var.x$sym.var.types == "$C") && (sym.var.y$sym.var.types == "$C")) {
    if (labels == FALSE) {
      plot(sym.var.x$data[, 1], sym.var.y$data[, 1],
        xlab = sym.var.x$sym.var.names,
        ylab = sym.var.y$sym.var.names, ...
      )
    } else {
      ltext <- sym.var.x$sym.var.names
      plot(sym.var.x$data[, 1], sym.var.y$data[, 1],
        type = "n", xlab = sym.var.x$sym.var.names,
        ylab = sym.var.y$sym.var.names, ...
      )
      text(sym.var.x$data[, 1], sym.var.y$data[, 1], ltext)
    }
  }
  if ((sym.var.x$sym.var.types == "$I") && (sym.var.y$sym.var.types == "$I")) {
    xmin1 <- min(sym.var.x$data[, 1])
    xmin2 <- min(sym.var.x$data[, 2])
    xmin <- min(xmin1, xmin2)
    xmax1 <- max(sym.var.x$data[, 1])
    xmax2 <- max(sym.var.x$data[, 2])
    xmax <- max(xmax1, xmax2)
    ymin1 <- min(sym.var.y$data[, 1])
    ymin2 <- min(sym.var.y$data[, 2])
    ymin <- min(ymin1, ymin2)
    ymax1 <- max(sym.var.y$data[, 1])
    ymax2 <- max(sym.var.y$data[, 2])
    ymax <- max(ymax1, ymax2)
    plot(c(xmin, xmax), c(ymin, ymax),
      type = "n", xlab = sym.var.x$sym.var.names,
      ylab = sym.var.y$sym.var.names, ...
    )
    for (i in 1:sym.var.x$N) {
      x1 <- sym.var.x$data[i, 1]
      y1 <- sym.var.y$data[i, 1]
      x2 <- sym.var.x$data[i, 2]
      y2 <- sym.var.y$data[i, 2]
      rect(x1, y1, x2, y2, lwd = 2, border = i + 1)
    }
    if (labels == TRUE) {
      ltext <- sym.var.x$sym.obj.names
      text(jitter(sym.var.x$data[, 1]), jitter(sym.var.y$data[, 1]), ltext)
    }
  }
}
