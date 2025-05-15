#' Lower boundary correlation coefficient.
#' @name R2.L
#' @aliases R2.L
#' @author Oldemar Rodriguez Rojas
#' @description Compute the lower boundary correlation coefficient for two interval variables.
#' @usage R2.L(ref, pred)
#' @param ref Variable that was predicted.
#' @param pred The prediction given by the model.
#'
#' @return The lower boundary correlation coefficient.
#' @references
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#' @seealso sym.glm
#' @examples
#' data(int_prost_train)
#' data(int_prost_test)
#' res.cm <- sym.lm(lpsa ~ ., sym.data = int_prost_train, method = "cm")
#' pred.cm <- sym.predict(res.cm, int_prost_test)
#' R2.L(int_prost_test$lpsa, pred.cm$Fitted)
#' @keywords lower correlation
#' @importFrom stats cor
#' @export
R2.L <- function(ref, pred) {
  out <- cor(min(ref), pred[, 1])^2
  return(out)
}

#' Upper boundary correlation coefficient.
#' @name R2.U
#' @aliases R2.U
#' @author Oldemar Rodriguez Rojas
#' @description Compute the upper boundary correlation coefficient for two interval variables.
#' @usage R2.U(ref, pred)
#' @param ref Variable that was predicted.
#' @param pred The prediction given by the model.
#'
#' @return The upper boundary correlation coefficient.
#' @references LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#' @seealso sym.glm
#' @examples
#' data(int_prost_train)
#' data(int_prost_test)
#' res.cm <- sym.lm(lpsa ~ ., sym.data = int_prost_train, method = "cm")
#' pred.cm <- sym.predict(res.cm, int_prost_test)
#' R2.U(int_prost_test$lpsa, pred.cm$Fitted)
#' @keywords upper correlation
#' @export
#' @importFrom stats cor
#'
R2.U <- function(ref, pred) {
  out <- cor(max(ref), pred[, 2])^2
  return(out)
}
