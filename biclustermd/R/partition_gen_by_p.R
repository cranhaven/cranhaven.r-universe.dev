#' Create a partition matrix with a partition vector p
#'
#' @param N Rows in a partition matrix
#' @param K Number of prototypes to create
#' @param p Integer vector containing the cluster each row in a partition matrix is to be assigned to.
#'
#' @return A partition matrix.
#'
partition_gen_by_p <- function(N, K, p) {
  P <- matrix(0, nrow = N, ncol = K)
  for (i in 1:K) {
    ind <- which(p == i)
    P[, i] <- binary_vector_gen(N, ind)
  }
  P1 <- format_partition(P)

  return(P1)
}
