calc_theta_hat <-
  function(ftheta, Y, Q, optimization_fun, optimization_args, fun_to_optimize_arg){
    # optimization_args should already be formatted by
    # validate_args_distfreereg_...().
    stopifnot(is.function(ftheta), is.function(optimization_fun), is.numeric(Y),
              is.numeric(Q))
    if(is.matrix(Q)){
      target <- function(theta) crossprod(Q %*% (Y - ftheta(theta)))
    } else {
      target <- function(theta) crossprod(Q * (Y - ftheta(theta)))
    }
    # Add function to optimize to argument list.
    optimization_args[[fun_to_optimize_arg]] <- target
    optimization_output <-
      tryCatch(do.call(optimization_fun, args = optimization_args),
               error = function(e) stop("Unable to calculate theta_hat: ", e))
    return(optimization_output)
  }
