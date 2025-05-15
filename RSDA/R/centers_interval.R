#' Compute centers of the interval
#' @name centers.interval
#' @author Jorge Arce.
#' @aliases centers.interval
#' @description Compute centers of the interval
#' @usage centers.interval(sym.data)
#' @param sym.data Symbolic interval data table.
#'
#' @return Centers of teh intervals.
#' @references Arce J. and Rodriguez O. (2015) 'Principal Curves and Surfaces to Interval Valued Variables'.
#' The 5th Workshop on Symbolic Data Analysis, SDA2015, Orleans, France, November.
#'
#' Hastie,T. (1984).Principal Curves and Surface. Ph.D Thesis Stanford University.
#'
#' Hastie,T. & Weingessel,A. (2014).
#' princurve - Fits a Principal Curve in Arbitrary Dimension.R package version 1.1--12
#' http://cran.r-project.org/web/packages/princurve/index.html.
#'
#' Hastie,T. & Stuetzle, W. (1989). Principal Curves.
#' Journal of the American Statistical Association, Vol. 84-406, 502--516.
#'
#' Hastie, T., Tibshirani, R. & Friedman, J. (2008).
#' The Elements of Statistical Learning; Data Mining, Inference and Prediction. Springer, New York.
#'
#' @seealso sym.interval.pc
#' @keywords Principal Curve
#' @import princurve
centers.interval <- function(sym.data) {
  idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
  if (idn == FALSE) {
    stop("All variables have to be of the same type")
  }

  if ((sym.data$sym.var.types[1] != "$I")) {
    stop("Variables have to be continuos or Interval")
  } else {
    nn <- sym.data$N
  }
  mm <- sym.data$M
  centers <- matrix(0, nn, mm)
  centers <- as.data.frame(centers)
  rownames(centers) <- sym.data$sym.obj.names
  colnames(centers) <- sym.data$sym.var.names
  for (i in 1:nn) {
    for (j in 1:mm) {
      centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[
        i,
        1
      ] + sym.var(sym.data, j)$var.data.vector[i, 2]) / 2
    }
  }

  return(centers)
}
