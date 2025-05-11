get_n <- function(n = NULL, ...){
  n <- unique(c(n ,unlist(sapply(list(...), get_n_single))))
  if(length(n) > 1){
    stop("All sets of covariates must have the same number of observations")
  } else {
    if(length(n) == 0) stop("No covariates supplied")
  }
  return(n)
}
