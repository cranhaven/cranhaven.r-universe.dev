#' Extract the number of observations from `survival_ln_mixture` fit.
#'
#' Extract the number of observations used in a `survival_ln_mixture` fit.
#'
#' @param object A fitted `survival_ln_mixture` object.
#' @param ... Not used.
#'
#' @return A single integer.
#'
#' @export
nobs.survival_ln_mixture <- function(object, ...) { # nolint: object_name_linter.
  rlang::check_dots_empty(...)
  return(object$nobs)
}

extract_posterior <- function(model) {
  return(model$posterior)
}

extract_formula <- function(model) {
  # Trocar NULL por 1 para caso onde so tem intercepto
  formula <- gsub("NULL", "1", deparse(model$blueprint$formula))
  return(formula)
}

npredictors <- function(model) {
  return(ncol(model$blueprint$ptypes$predictors) + model$blueprint$intercept)
}
