#' @method summary gorica
#' @importFrom stats qnorm
#' @export
summary.gorica <- function(object, ci = .95, ...){
  # Fix this in gorica; make sure that it always returns a matrix, even if
  # posterior is only one number
  if(inherits(object$posterior, what = "numeric")) object$posterior <- as.matrix(object$posterior)
  data.frame(
    Parameter = names(object$estimates),
    n = object$n,
    Estimate = object$estimates,
    lb = object$estimates + qnorm((1-ci)/2)*sqrt(diag(object$posterior)),
    ub = object$estimates - qnorm((1-ci)/2)*sqrt(diag(object$posterior)),
    row.names = NULL
  )
}
