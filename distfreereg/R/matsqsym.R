# This function assumes that mat, if a matrix, is symmetric.
matsqsym <-
function(mat){
  stopifnot(is.numeric(mat))
  if(is.matrix(mat)){
    stopifnot(identical(diff(dim(mat)), 0L))
    output <- crossprod(mat)
  } else {
    # This allows covariance specification in the form of a numeric vector.
    output <- mat * mat
  }
  return(output)
}
