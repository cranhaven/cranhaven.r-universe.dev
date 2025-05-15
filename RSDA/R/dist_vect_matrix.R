#' Compute the distance vector matrix
#' @name dist.vect.matrix
#' @aliases dist.vect.matrix
#' @author Jorge Arce.
#' @description Compute the distance vector matrix.
#'
#' @usage dist.vect.matrix(vector, Matrix)
#' @param vector An n dimensional vector.
#' @param Matrix An n x n matrix.
#'
#' @return The distance.
#' @references Arce J. and Rodriguez O. (2015) 'Principal Curves and Surfaces
#' to Interval Valued Variables'. The 5th Workshop on Symbolic
#' Data Analysis, SDA2015, Orleans, France, November.
#'
#' Hastie,T. (1984).
#' Principal Curves and Surface. Ph.D Thesis Stanford University.
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
#'
#' @keywords Principal Curve
#'
dist.vect.matrix <- function(vector, Matrix) {
  num.col.vect <- ncol(vector)
  num.col.matrix <- ncol(Matrix)
  num.row.matrix <- nrow(Matrix)
  dist.matrix <- rep(-1, num.row.matrix)
  if (num.col.vect == num.col.matrix) {
    for (i in 1:num.row.matrix) {
      w <- as.matrix(Matrix[i, ])
      dist.matrix[i] <- dist.vect(vector, t(w))
    }
  }
  return(dist.matrix)
}
