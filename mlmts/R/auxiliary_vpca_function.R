

auxiliary_vpca_function_1 <- function(X, var_rate = 0.90) {


  N <- ncol(X[[1]]) # Number of variables
  L <- nrow(X[[1]]) # Series length
  M <- length(X)  # Number of MTS

  # Constructing the variable matrices

  V <- list()

  for (i in 1 : N) {

    V[[i]] <- matrix(0, nrow = M, ncol = L)

    for (j in 1 : M) {

      V[[i]][j,] <- X[[j]][,i]

    }
  }


  # Computing covariance matrix for each Vn

  list_covariance_matrices <- list()

  for (i in 1 : N) {

    list_covariance_matrices[[i]] <- stats::cov(V[[i]])

  }

  # Computing eigenvalues (vector) and eigenvectors (matrix) for each covariance matrix

  list_eigenvalues <- list()
  list_eigenvectors <- list()

  for (i in 1 : N) {

    auxiliary_eigen <- eigen(list_covariance_matrices[[i]])
    list_eigenvalues[[i]] <- auxiliary_eigen$values
    list_eigenvectors[[i]] <- auxiliary_eigen$vectors

  }

  # Computing the number pn for each covariance matrix

  p_n <- numeric()

  for (i in 1 : N) {

  percentage_variability_n <- cumsum(list_eigenvalues[[i]]/sum(list_eigenvalues[[i]]))
  p_n[i] <- sum(percentage_variability_n < var_rate) + 1

  }

  # Computing p_s

  p_s <- min(p_n)

  # Computing the matrices F_n

  F_matrices <- list()

  for (i in 1 : N) {

    F_matrices[[i]] <- V[[i]] %*% list_eigenvectors[[i]][, c(1 : p_s)]

  }

  # Computing the reduced MTS

  Y <- list()

  for (i in 1 : M) {

    Y[[i]] <- matrix(0, nrow = p_s, ncol = N)

    for (j in 1 : N) {

      Y[[i]][,j] <- F_matrices[[j]][i,]

    }
  }

  return_list <- list(Y = Y, p_s = p_s)
  return(return_list)

}


auxiliary_vpca_function_2 <- function(N, p_s) {

  auxiliary_matrix <- matrix(0, nrow = N, ncol = p_s)
  vector_rows <- rep(1 : N, each = p_s)
  vector_columns <- rep(1 : p_s, N)
  vector_row_columns <- cbind(vector_rows, vector_columns)

  distance_matrix_rows_columns <- matrix(0, N*p_s, N*p_s)

  for (i in (1 : (N*p_s))) {

    for (j in (1 : (N*p_s))) {

      distance_matrix_rows_columns[i, j] <- TSdist::EuclideanDistance(vector_row_columns[i,],
                                                              vector_row_columns[j,])^2

    }

  }

  denominator <- 2 * (1 - p_s^(-1))^2
  S_prev <- exp(-distance_matrix_rows_columns/denominator)

  multiply_by <- 1/(2 * pi * (1 - p_s^(-1))^2)

  return(multiply_by * S_prev)

}


auxiliary_vpca_function_3 <- function(Y_1, Y_2, S) {

  Y_1 <- t(Y_1)
  Y_2 <- t(Y_2)
  y_1 <- c(t(Y_1))
  y_2 <- c(t(Y_2))
  y_diff <- y_1 - y_2

  return_number <- sqrt(y_diff %*% S %*% y_diff)
  return(return_number)


}


