define_r <- function(n, p, res_order, gs_tol){
  stopifnot(isPermutation(res_order), is.numeric(gs_tol), gs_tol > 0)
  # First create orthogonal vectors, then normalize.
  validate_numeric(n, len = 1, pos_int = TRUE)
  validate_numeric(p, len = 1, pos_int = TRUE)
  n <- as.integer(n)
  p <- as.integer(p)
  r <- matrix(NA, nrow = n, ncol = p)
  # First column of r is unit vector with equal elements.
  r[,1] <- rep(1/sqrt(n), n)
  if(p > 1){
    # Create second column, which also serves as a template for all remaining
    # columns.
    r[,2] <- sqrt(12/n) * ((1:n)/n - (n+1)/(2*n))
    # All remaining columns are powers of second column.
    for(i in (seq_len(p-2))){
      r[,i+2] <- r[,2]^(i+1)
    }
  }
  r <- tryCatch(qr.Q(qr(x = r, tol = gs_tol)),
                error = function(e) stop("qr() unable to orthogonalize r: ", e))
  r <- r[res_order, , drop = FALSE]
  return(r)
}
