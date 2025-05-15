#' Compute neighbors vertex
#' @name neighbors.vertex
#' @author Jorge Arce
#' @aliases neighbors.vertex
#' @description Compute neighbors vertex
#' @usage neighbors.vertex(vertex, Matrix, num.neig)
#' @param vertex Vertes of the hipercube
#' @param Matrix Interval Data Matrix.
#' @param num.neig Number of vertices.
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
#' The Elements of Statistical Learning; Data Mining, Inference and Prediction.
#' Springer, New York.
#'
#' @seealso sym.interval.pc
#' @keywords Principal Curve
#'
neighbors.vertex <- function(vertex, Matrix, num.neig) {
  dist <- dist.vect.matrix(t(vertex), Matrix)
  index <- order(dist, decreasing = FALSE)
  neighbors <- Matrix[index[1:num.neig], ]
  return(list(neighbors = neighbors, order = index, distance = dist))
}
