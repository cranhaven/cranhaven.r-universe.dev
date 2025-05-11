matsqrt <-
function(mat, tol = 0){
  stopifnot(is.numeric(mat), is.numeric(tol), length(tol) == 1, tol <= 0)
  if(is.matrix(mat)){
    if(all(dim(mat) == 1)){
      # Note: this is needed when p = 1, in which case J^tJ is 1x1.
      output <- sqrt(mat)
    } else{
      stopifnot(identical(diff(dim(mat)), 0L))
      eig <- tryCatch(eigen(mat, symmetric = TRUE),
                      error = function(e) stop("Unable to compute eigendecomposition of ",
                                               deparse1(substitute(mat)), ": ", e))
      if(any(is.complex(eig[["values"]]))) stop(deparse1(substitute(mat)), " has complex eigenvalues")
      if(any(is.complex(eig[["vectors"]]))) stop(deparse1(substitute(mat)), " has complex eigenvectors")
      if(any(eig[["values"]] < tol)) stop(deparse1(substitute(mat)), " has eigenvalues less than the tolerance, ",
                                        "the smallest of which is about ", signif(min(eig[["values"]]), 3))
      e_vals <- pmax(0, eig[["values"]])
      output <- eig[["vectors"]] %*% (sqrt(e_vals) * t(eig[["vectors"]]))
    }
  } else {
    # This allows covariance specification in the form of a single numeric
    # value.
    stopifnot(all(mat > 0))
    output <- sqrt(mat)
  }
  return(output)
}
