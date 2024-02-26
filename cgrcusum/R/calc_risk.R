#' @title Calculate the Cox risk associated with the covariates of the individual
#'
#' @description  This function can be used to calculate the risk associated with the
#' covariates of an individual under a specified Cox PH model.
#'
#' @details The subject specific increased risk is given by: \deqn{e^{\beta  Z_i}}{exp(\beta * Z_i)}
#' with \eqn{\beta}{\beta} the Cox coefficients and Z_i the covariates of subject i.
#'
#'
#' @param data data frame containing the covariates to be used for risk-adjustment as named columns.
#' @param coxphmod (optional) a cox proportional hazards model generated using
#' \code{\link[survival:coxph]{coxph()}} or a list containing:
#' \itemize{
#' \item $formula (~ covariates),
#' \item $coefficients (named vector specifying risk adjustment coefficients
#' for covariates - names must be the same as in $formula and data colnames).
#' }
#'
#'
#'
#' @return A vector of nrow(data) specifying the increased risk of failure for each subject.
#'
#' @importFrom stats model.matrix
#' @export
#'
#' @author Daniel Gomon
#' @family utils
#' @examples
#' crdat <- data.frame(age = rnorm(10, 40, 5), BMI = rnorm(10, 24, 3))
#' crlist <- list(formula = as.formula("~age + BMI"), coefficients = c("age"= 0.02, "BMI"= 0.009))
#' calc_risk(crdat, crlist)


calc_risk <- function(data, coxphmod = NULL){
  #Calculate risk for dataset data according to Cox PH model using specified model
  #coxphmod must either be a COXPH model or a list containing $formula and named vector $coefficients
  if(nrow(data) == 0){
    return(1)
  } else if(is.null(coxphmod)){
    return(rep(1, nrow(data)))
  } else{
    mmatrix <- model.matrix(coxphmod$formula, data)[,-1] #removes the intercept
    coeffs <- coxphmod$coefficients[colnames(mmatrix)]
    if(nrow(data) == 1){
      coeffs <- coxphmod$coefficients[names(mmatrix)]
    }
    return(c(exp(mmatrix %*% coeffs)))
  }
}
