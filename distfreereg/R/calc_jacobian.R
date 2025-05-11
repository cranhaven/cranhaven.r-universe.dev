calc_jacobian <-
function(ftheta, theta_hat, jacobian_args = NULL){
  stopifnot(is.function(ftheta), is.numeric(theta_hat),
            is.null(jacobian_args) || is.list(jacobian_args))
  func <- function(x) ftheta(x)
  arg_list <- c(list(func = func, x = theta_hat), jacobian_args)
  J <- tryCatch(do.call(numDeriv::jacobian, args = arg_list),
                error = function(e) stop("Unable to compute Jacobian: ", e))
  validate_numeric(J)
  stopifnot(is.matrix(J))
  return(J)
}
