# This does not verify that a matrix input for x is diagonal.
covariance2weights <- function(x, n){
  stopifnot(is.numeric(x))
  if(isTRUE(length(x) == 1)){
    output <- rep(x, times = n)
  } else {
    if(isTRUE(is.matrix(x))){
      output <- diag(x)
    } else {
      stopifnot(length(x) == n)
      output <- x
    }
  }
  return(output)
}
