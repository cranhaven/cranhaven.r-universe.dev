generate_Y_mean.nls <- function(true_mean, theta, ...){
  func <- formula(true_mean)[[3]]
  envr <- c(as.list(theta), as.list(get_nls_data(true_mean)))
  output <- tryCatch(with(envr, eval(func)),
                     error = function(e) stop("Unable to generate Y_mean: ", e))
  return(output)
}
