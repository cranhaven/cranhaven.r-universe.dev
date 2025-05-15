#' Compute the determination cosfficient
#' @aliases deter.coefficient
#' @name deter.coefficient
#' @author Oldemar Rodriguez Rojas
#' @description The determination coefficient represents a
#' goodness-of-fit measure commonly used in regression analysis to
#' capture the adjustment quality of a model.
#' @usage deter.coefficient(ref, pred)
#' @param ref Variable that was predicted.
#' @param pred The prediction given by the model.
#'
#' @return Return the determination cosfficient.
#' @references LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#' @seealso sym.glm
#' @examples
#' data(int_prost_test)
#' data(int_prost_train)
#' res.cm <- sym.lm(lpsa ~ ., sym.data = int_prost_train, method = "cm")
#' pred.cm <- sym.predict(res.cm, int_prost_test)
#' deter.coefficient(int_prost_test$lpsa, pred.cm$Fitted)
#' @keywords Symbolic Lasso Ridge Elastic Net
#' @export
#'
deter.coefficient <- function(ref, pred) {
  centers.pvar <- rep(0, length(ref))
  centers.pred <- rep(0, length(ref))
  centers.pvar <- (min(ref) + max(ref)) / 2
  centers.pred <- (pred[, 1] + pred[, 2]) / 2
  coef <- sum((centers.pred - mean(centers.pvar))^2) / sum((centers.pvar - mean(centers.pvar))^2)
  return(coef)
}
