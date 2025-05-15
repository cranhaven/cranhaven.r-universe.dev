#' Compute a distance vector
#' @name dist.vect
#' @aliases dist.vect
#' @author Jorge Arce
#' @description Compute a distance vector
#' @usage dist.vect(vector1, vector2)
#' @param vector1 First vector.
#' @param vector2 Second vector.
#'
#' @return Eclidean distance between the two vectors.
#' @references Arce J. and Rodriguez O. (2015) 'Principal Curves and Surfaces
#' to Interval Valued Variables'. The 5th Workshop on Symbolic
#' Data Analysis, SDA2015, Orleans, France, November.
#'
#' Hastie,T. (1984).
#' Principal Curves and Surface. Ph.D. Thesis Stanford University.
#'
#' Hastie,T. & Weingessel,A. (2014).
#' princurve - Fits a Principal Curve in Arbitrary Dimension.R package version 1.1--12
#' http://cran.r-project.org/web/packages/princurve/index.html.
#'
#' Hastie,T. & Stuetzle, W. (1989). Principal Curves.
#' Journal of the American Statistical Association, Vol. 84-406, 502--516.
#'
#' Hastie, T., Tibshirani, R. & Friedman, J. (2008).
#' The Elements of Statistical Learning; Data Mining, Inference and Prediction.
#' Springer, New York.
#'
#' @seealso sym.interval.pc
#' @keywords Principal Curve
#'
dist.vect <- function(vector1, vector2) {
  dist <- 0
  if (ncol(vector1) == ncol(vector2)) {
    dist <- norm.vect(vector1 - vector2)
  }
  return(dist)
}
