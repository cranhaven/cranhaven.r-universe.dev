



auxiliary_eros_function_1 <- function(X, method = 'mean', normalization = F, cor = T) {

  l <- length(X)
  c <- ncol(X[[1]])
  matrix_eigenvalues <- matrix(0, nrow = c, ncol = l)
  list_matrices_eigenvectors <- list()

  if (cor == T) {

  for (i in 1 : l) {

    auxiliary_eigen <- eigen(cor(X[[i]]))
    matrix_eigenvalues[,i] <- auxiliary_eigen$values
    list_matrices_eigenvectors[[i]] <- auxiliary_eigen$vectors


  }

  } else {

    for (i in 1 : l) {

      auxiliary_eigen <- eigen(stats::cov(X[[i]]))
      matrix_eigenvalues[,i] <- auxiliary_eigen$values
      list_matrices_eigenvectors[[i]] <- auxiliary_eigen$vectors


    }

  }


  # Matrix containing the eigenvalues by rows and the MTS by columns

  if (normalization == T) {

    matrix_eigenvalues <- matrix_eigenvalues/colSums(matrix_eigenvalues)[col(matrix_eigenvalues)]

  }

  function_aggregation <- get(method)
  vector_aggregated <- apply(matrix_eigenvalues, 1, function_aggregation)

  # Returning the vector of weights and the eigenvectors matrices

  vector_weights <- vector_aggregated/sum(vector_aggregated)
  returned_list <- list(eigenvectors_matrices = list_matrices_eigenvectors, weights = vector_weights)
  return(returned_list)

}



# Second auxiliary function. Computing the EROS distance measure


auxiliary_eros_function_2 <- function(matrix_eigenvectors_1, matrix_eigenvectors_2, vector_weights) {

  cross_products <- abs(colSums(matrix_eigenvectors_1 * matrix_eigenvectors_2))
  eros_similarity <- sum(vector_weights * cross_products)
  eros_dissimilarity <- sqrt(2 - 2 * eros_similarity)
  return(eros_dissimilarity)

}






