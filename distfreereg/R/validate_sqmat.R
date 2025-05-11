validate_sqmat <- function(x, n, symmetric, message = NULL){
  validate_numeric(x)
  if(!is.matrix(x)) stop(message, deparse1(substitute(x)), " must be a matrix; ",
                         "it currently has class \"", class(x), "\"")
  if(any(dim(x) != n)) stop(message, "The dimensions of ", deparse1(substitute(x)),
                            " must be ", n, " by ", n)
  
  do_call_args <- combine_lists(symmetric,
                                list(tol = sqrt(.Machine[["double.eps"]]),
                                     object = unname(x)))

  if(!isFALSE(symmetric) && !do.call(isSymmetric.matrix, args = do_call_args))
    stop(message, deparse1(substitute(x)), " is not symmetric")
}
