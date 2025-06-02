################################################################################
#
# Check if constraint matrix is reducible
# Returns constraints that can be removed safely
#
################################################################################

check_cmat <- function(Cmat){
  tCmat <- t(Cmat)
  ncons <- ncol(tCmat)
  redundant <- rep(FALSE, ncons)
  for (i in seq_len(ncons)){
    y <- tCmat[,i]
    x <- tCmat[,-c(i, which(redundant)),drop = FALSE]
    res <- limSolve::nnls(x, y)
    redundant[i] <- isTRUE(all.equal(y, drop(x %*% res$X)))
  }
  which(redundant)
}