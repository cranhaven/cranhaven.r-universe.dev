matinv <-
  function(mat, tol){
    # This function should be able to take a single number as input and output
    # the multiplicative inverse.
    stopifnot(is.numeric(mat), is.numeric(tol), tol > 0)
    if(is.matrix(mat)){
      output <-tryCatch(solve(mat, tol = tol),
                        error = function(e) stop("Unable to invert ",
                                                 deparse1(substitute(mat)), ": ", e))
    } else {
      stopifnot(all(mat != 0))
      output <- 1/mat
      stopifnot(all(is.finite(output)))
    }
    return(output)
  }
