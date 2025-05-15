#' Lower boundary root-mean-square error
#' @name RMSE.L
#' @aliases RMSE.L
#' @author Oldemar Rodriguez Rojas.
#' @description Compute the lower boundary root-mean-square error.
#' @usage RMSE.L(ref, pred)
#' @param ref Variable that was predicted.
#' @param pred The prediction given by the model.
#'
#' @return The lower boundary root-mean-square error.
#' @references
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#' @seealso sym.glm
#' @keywords lower root-mean-square
#' @export
#'
RMSE.L <- function(ref, pred) {
  res <- sqrt(sum((min(ref) - pred[, 1])^2) / length(ref))
  return(res)
}

#' Upper boundary root-mean-square error
#' @name RMSE.U
#' @aliases RMSE.U
#' @author Oldemar Rodriguez Rojas
#' @description Compute the upper boundary root-mean-square error.
#' @usage RMSE.U(ref, pred)
#' @param ref Variable that was predicted.
#' @param pred The prediction given by the model.
#'
#' @return The upper boundary root-mean-square error.
#' @references
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#' @seealso sym.glm
#' @keywords upper root-mean-square
#' @export
#'
RMSE.U <- function(ref, pred) {
  res <- sqrt(sum((max(ref) - pred[, 2])^2) / length(ref))
  return(res)
}
