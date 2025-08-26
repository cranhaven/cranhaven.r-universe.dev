#' Generate an intial, random partition matrix with N objects into K subsets/groups.
#'
#' @param N Number of objects/rows in a partition matrix
#' @param K Desired number of partitions
#'
#' @return A partition matrix.
#' @description This function is used to randomly generate a partition matrix and assign rows or columns to prototypes. Must be the case that N > K.
partition_gen <- function(N, K) {
  a <- sample(1:K, N, replace = TRUE)

  P <- matrix(0, nrow = N, ncol = K)

  for (i in 1:K) {
    ind <- which(a == i)
    av <- rep(0, N)
    av[ind] <- 1

    P[, i] <- av
  }

  count_in_proto <- apply(P, 2, sum)

  if(all(count_in_proto > 0)) {
    P <- P
  } else if(0 %in% count_in_proto) {
    empty_protos <- which(count_in_proto == 0)
    empties <- length(empty_protos)

    for(i in 1:empties) {
      count_in_proto <- colSums(P)

      max_count <- which.max(count_in_proto)
      empty_protos <- which(count_in_proto == 0)

      rows_to_change <- which(P[, max_count] == 1)

      dummy_v <- rep(0, ncol(P))
      dummy_v[empty_protos[1]] <- 1

      P[rows_to_change[1],] <- dummy_v

    }

  }

  P1 <- format_partition(P)
  return(P1)
}
