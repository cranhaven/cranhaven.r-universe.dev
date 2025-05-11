lm2function <- function(mean_fun, secondary_mean_fun = NULL, arg_list = list()){
  if(is.null(mean_fun[["x"]]) || is.null(mean_fun[["y"]])){
    m <- tryCatch(do.call(update,
                          args = combine_lists(arg_list[["control"]][["lm_args"]],
                                               list(object = mean_fun, x = TRUE, y = TRUE,
                                                    na.action = na.fail))),
                  error = function(e) stop("Error updating linear model: ", e))
  } else {
    m <- mean_fun
  }
  
  w <- weights(m)
  if(is.null(w)){
    true_covariance <- list(Sigma = sigma(m)^2)
  } else {
    true_covariance <- list(P = w)
  }
  
  return(list(mean_fun = function(x, theta) sum(x*theta),
              true_covariance = true_covariance,
              X = m[["x"]],
              Y = m[["y"]],
              theta = m[["coefficients"]],
              arg_list = arg_list))
}
