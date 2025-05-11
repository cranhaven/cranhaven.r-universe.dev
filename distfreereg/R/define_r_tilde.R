# This function converts the r matrix into the "r tilde" matrix, as described in
# (23) on page 5 of the notes.
define_r_tilde <- function(r, mu, k2_tol){
  # Verify that there are as many columns in r as elements in theta_hat.
  stopifnot(is.matrix(r), is.matrix(mu), isTRUE(all(dim(r) == dim(mu))))
  # Define a matrix of the required size.
  r_tilde <- matrix(NA, nrow = nrow(r), ncol = ncol(r))
  # The first column is the same as that of r.
  r_tilde[,1] <- r[,1]
  # Now, iterate through the other columns of r to define the other columns of
  # r_tilde.
  for(i in seq_len(ncol(r) - 1) + 1){
    r_tilde[,i] <- r[,i]
    for(j in seq_len(i-1)){
      r_tilde[,i] <- k2(r_tilde[,i], mu[,j], r_tilde[,j], k2_tol = k2_tol)
    }
  }
  return(r_tilde)
}
