calc_k2_resid <-
  # This implements the function U_p^*.
  function(x, r_tilde, mu, k2_tol){
    stopifnot(is.numeric(x), is.vector(x),
              is.matrix(r_tilde), is.matrix(mu),
              isTRUE(all(dim(r_tilde) == dim(mu))),
              identical(dim(mu)[1], length(x)))
    output <- x
    for(i in rev(seq_len(ncol(r_tilde)))){
      output <- k2(output, r_tilde[,i], mu[,i], k2_tol = k2_tol)
    }
    return(output)
  }
