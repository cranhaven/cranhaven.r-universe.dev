from_B_to_laplacian <- function(B) {
  A <- from_B_to_adjacency(B)
  return(diag(rowSums(A)) - A)
}

from_B_to_adjacency <- function(B) {
  r <- nrow(B)
  q <- ncol(B)
  zeros_rxr <- matrix(0, r, r)
  zeros_qxq <- matrix(0, q, q)
  return(rbind(cbind(zeros_rxr, B), cbind(t(B), zeros_qxq)))
}

