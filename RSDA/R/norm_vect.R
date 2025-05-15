#' Compute the norm of a vector.
#' @name norm.vect
#' @aliases norm.vect
#' @author Jorge Arce
#'
#' @usage norm.vect(vector1)
#' @param vector1 An n dimensional vector.
#' @return The L2 norm of the vector.
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
#' The Elements of Statistical Learning; Data Mining, Inference and Prediction.
#'  Springer, New York.
#'
#' @seealso sym.interval.pc
#' @keywords Principal Curve
#'
norm.vect <- function(vector1) {
  norm <- sqrt(sum(vector1^2))
  return(norm)
}
