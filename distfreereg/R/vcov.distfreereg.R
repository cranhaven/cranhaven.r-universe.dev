vcov.distfreereg <- function(object, ..., jacobian_args = NULL, hessian_args = NULL){
  stopifnot(is(object, "distfreereg"))
  test_mean <- object[["test_mean"]]
  theta_hat <- object[["theta_hat"]]
  Q <- object[["covariance"]][["Q"]]
  stopifnot(!is.null(test_mean), !is.null(theta_hat), !is.null(Q))
  if(is(test_mean, "function")){
    output <- vcov_function(f = test_mean,
                            Q = Q,
                            theta_hat = theta_hat,
                            X = object[["data"]][["X"]],
                            Y = object[["data"]][["Y"]],
                            jacobian_args = jacobian_args,
                            hessian_args = hessian_args)
    colnames(output) <- rownames(output) <- names(theta_hat)
  } else {
    if(!is.null(jacobian_args) || !is.null(jacobian_args))
      warning("jacobian_args and/or hessian_args ignored when 'test_mean' is not a function")
    output <- vcov(test_mean, ...)
  }
  return(output)
}