eval_cov <- function(covariance_list, Y = NULL, X = NULL, data = NULL){
  # This function takes the "covariance" argument from compare() and evaluates
  # any elements that are functions. It does not do so cleverly; that is, it
  # evaluates any that are functions, regardless of whether there are multiple
  # redundant functions.
  stop("This error should not appear!")
  if(is.null(data)){
    output <- lapply(covariance_list,
                     function(x) if(is.function(x)) do.call(x, args = list(Y = Y, X = X)) else x)
  } else {
    output <- lapply(covariance_list,
                     function(x) if(is.function(x)) do.call(x, args = list(data = data)) else x)
  }
  return(output)
}
