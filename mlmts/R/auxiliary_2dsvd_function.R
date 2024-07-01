

auxiliary_2dsvd_function_1 <- function(X, var_u = 0.90, var_v = 0.90) {

  series_length <- nrow(X[[1]])
  c <- ncol(X[[1]])


  # Computing average matrix

  l <- length(X)
  sum_matrix <- Reduce('+', X)
  avg_matrix <- sum_matrix/l

  # Computing F and G matrices

  F_matrix_factors <- list()
  G_matrix_factors <- list()

  for (i in 1 : l) {

    F_matrix_factors[[i]] <- (X[[i]] - avg_matrix) %*% t(X[[i]] - avg_matrix)
    G_matrix_factors[[i]] <- t(X[[i]] - avg_matrix) %*% (X[[i]] - avg_matrix)

  }

  F_matrix <- (1/l) * Reduce('+', F_matrix_factors)
  G_matrix <- (1/l) * Reduce('+', G_matrix_factors)


  # Computing  and V matrices

  U_prev <- eigen(F_matrix)
  V_prev <- eigen(G_matrix)

  percentage_variability_U <- cumsum(U_prev$values/sum(U_prev$values))
  percentage_variability_V <- cumsum(V_prev$values/sum(V_prev$values))

  selected_eigenvectors_U <- sum(percentage_variability_U < var_u) + 1
  selected_eigenvectors_V <- sum(percentage_variability_V < var_v) + 1

  U_matrix <- U_prev$vectors[, 1 : selected_eigenvectors_U]
  V_matrix <- V_prev$vectors[, 1 : selected_eigenvectors_V]


  # Constructing the feature matrices

  feature_matrices <- list()

  for (i in 1 : l) {

    feature_matrices[[i]] <- t(U_matrix) %*% X[[i]] %*% V_matrix

  }

  return(feature_matrices)


}



auxiliary_2dsvd_function_2 <- function(M1, M2) {

  n_cols <- ncol(M1)

  vector_distances <- numeric()

  for (i in 1 : n_cols) {

      vector_distances[i] <- TSdist::EuclideanDistance(M1[,i], M2[,i])

  }

  return(sum(vector_distances))

  }












