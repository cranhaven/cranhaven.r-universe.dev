

# Auxiliary function for computation of common space

auxiliary_common_space_function <- function(X, var_rate = 0.90) {

  l <- length(X)


  # Centering the columns of each MTS

  X_centered_columns <- lapply(X, function(x) {scale(x, scale = F)})


  # Constructing the covariance matrix associated with each series

  covariance_matrices <- lapply(X, stats::cov)


  # Computing the average covariance matrix

  avg_covariance_matrix <- Reduce('+', covariance_matrices)/l


  # Performing SVD over the average covariance matrix

  auxiliary_svd <- eigen(avg_covariance_matrix)
  auxiliary_eigenvalues <- auxiliary_svd$values
  auxiliary_eigenvectors <- auxiliary_svd$vectors

  # Obtaining the common space based on var_rate

  percentage_variability <- cumsum(auxiliary_eigenvalues/sum(auxiliary_eigenvalues))
  retained_eigenvalues <- sum(percentage_variability < var_rate) + 1
  S_matrix_common_space <- as.matrix(auxiliary_eigenvectors[, 1 : retained_eigenvalues])

  return(S_matrix_common_space)


}


# Auxiliary function for computation of reconstructed series

auxiliary_reconstructed_series_function <- function(X_matrix, S_matrices) {


  # Obtaining the list of reconstructed MTS

  l_S_matrices <- length(S_matrices)
  auxiliary_list_series <- rep(list(X_matrix), l_S_matrices)
  list_reconstructed_matrices_prev <- mapply('%*%', auxiliary_list_series, S_matrices, SIMPLIFY = F)
  t_S_matrix_list <- lapply(S_matrices, t)
  list_reconstructed_series <- mapply('%*%', list_reconstructed_matrices_prev,
                                        t_S_matrix_list, SIMPLIFY = F)

  # Computing the reconstruction error from the MTS to all the reconstructed MTS

  reconstruction_error_vector_prev <- mapply(function(x, y) {TSdist::EuclideanDistance(c(x), c(y))^2},
                                        auxiliary_list_series,
                                        list_reconstructed_series, SIMPLIFY = F)
  reconstruction_error_vector <- unlist(reconstruction_error_vector_prev)

  assigned_cluster <- which.min(reconstruction_error_vector)
  min_error <- min(reconstruction_error_vector)
  return_list <- list(cluster = assigned_cluster, min_error = min_error)

  return(return_list)


}



