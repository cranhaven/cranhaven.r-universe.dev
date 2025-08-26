#' Calculate the sum cluster SSE in each iteration
#'
#' @param data The data being biclustered. Must to be a data matrix with only numbers and missing values in the data set. It should have row names and column names.
#' @param P Matrix for column prototypes.
#' @param Q Matrix for row prototypes.
#'
#' @return The SSE for the parameters specified.
#'
cluster_iteration_sum_sse <- function(data, P, Q) {
  np <- ncol(P)
  nq <- ncol(Q)

  cluster_sses <- numeric(np * nq)  # allocates necessary memory
  ind <- 1
  for(i in 1:nq) {
    for(j in 1:np) {
      q_proto <- which(Q[, i] == 1)
      p_proto <- which(P[, j] == 1)

      proto_mean <- mean(data[q_proto, p_proto], na.rm = TRUE)

      cluster_sses[ind] <- sum((data[q_proto, p_proto] - proto_mean) ^ 2, na.rm = TRUE)

      ind <- ind + 1
    }
  }

  return(sum(cluster_sses, na.rm = TRUE))
}
