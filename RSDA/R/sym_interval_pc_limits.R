#' Symbolic interval principal curves limits
#' @name sym.interval.pc.limits
#' @aliases sym.interval.pc.limits
#' @author Jorge Arce.
#' @description Symbolic interval principal curves limits.
#' @usage sym.interval.pc.limits(sym.data, prin.curve, num.vertex, lambda, var.ord)
#' @param sym.data Symbolic interval data table.
#' @param prin.curve Principal curves.
#' @param num.vertex Number of vertices of the hipercube.
#' @param lambda Lambda.
#' @param var.ord Order of the variables.
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
#'
sym.interval.pc.limits <- function(sym.data, prin.curve, num.vertex, lambda, var.ord) {
  num.vars <- sym.data$M
  num.ind <- sym.data$N

  res <- as.data.frame(prin.curve)
  res$lambda <- lambda

  sym.indiv <- rep("X", sum(num.vertex))

  start <- 1
  finish <- num.vertex[1]
  sym.indiv[start:finish] <- sym.data$sym.obj.names[1]

  for (i in 2:num.ind) {
    previous <- num.vertex[i - 1]
    start <- start + previous
    finish <- num.vertex[i] + finish
    sym.indiv[start:finish] <- sym.data$sym.obj.names[i]
  }

  res$symindiv <- sym.indiv
  var.type <- rep("$I", num.vars + 1)
  variables <- rep("X", num.vars)

  for (i in 1:num.vars) {
    variables[var.ord[i]] <- paste0("prin_surface_", as.character(i))
  }
  colnames(res)[1:num.vars] <- variables
  variables <- c(variables[var.ord], "lambda")
  sym.res <- classic.to.sym(x = res, concept = symindiv)
  return(sym.res)
}
