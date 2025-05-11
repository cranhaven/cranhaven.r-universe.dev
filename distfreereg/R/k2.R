k2 <-
function(x, a, b, k2_tol){
  stopifnot(is.numeric(x), is.vector(x),
            is.numeric(a), is.vector(a),
            is.numeric(b), is.vector(b),
            identical(length(x), length(a)), identical(length(x), length(b)),
            is.numeric(k2_tol), k2_tol > 0)
  normalizer <- 1 - sum(a*b)
  if(isTRUE(all.equal(normalizer, 0, check.attributes = FALSE,
                      tolerance = k2_tol))){
    warning("Matching columns found in mu and r_tilde that have scalar product nearly equal to 1")
    output <- x
  } else {
    output <- x - sum((a-b)*x)*(a-b)/normalizer
  }
  return(output)
}