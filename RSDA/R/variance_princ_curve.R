#' Variance of the principal curve
#' @name variance.princ.curve
#' @aliases  variance.princ.curve
#' @author Jorge Arce.
#' @description Variance of the principal curve
#' @usage variance.princ.curve(data,curve)
#' @param data Classic data table.
#' @param curve The principal curve.
#' @return The variance of the principal curve.
#' @references
#' Arce J. and Rodriguez O. (2015) 'Principal Curves and Surfaces
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
#' @seealso sym.interval.pc
#' @keywords Principal Curve
#' @import princurve
#' @export
#'
variance.princ.curve <- function(data, curve) {
  var.data <- diag(var(data))
  var.curve <- diag(var(curve))
  dist <- sum((data - curve)^2) / dim(data)[1]
  ord <- order(x = var.data, decreasing = TRUE)
  var.data.cum <- cumsum(var.data[ord])
  var.curve.cum <- cumsum(var.curve[ord])
  return(list(
    var.data = var.data, var.data.cum = var.data.cum, var.curve = var.curve,
    var.curve.cum = var.curve.cum, dist = dist, var.order = ord
  ))
}
