

auxiliary_lpp_function_1 <- function(X, approach = 1, k, t = 1) {

  l <- length(X)
  c <- ncol(X[[1]])


  # Computing the corresponding features by means of Li's first or second approach

  eigen_list <- lapply(X, function(x) {eigen(stats::cov(x))})
  eigen_list_values <- lapply(eigen_list, function(x) {x$values})
  eigen_list_vectors <- lapply(eigen_list, function(x) {x$vectors})
  eigen_list_normalized_values <- lapply(eigen_list_values, function(x) {x/norm(x, type = '2')})

  if (approach == 1) {

    matrix_li <- matrix(0, nrow = l, ncol = 2 * c)

    for (i in 1 : l){

      matrix_li[i,] <- c(eigen_list_vectors[[i]][,1], eigen_list_normalized_values[[i]])

    }

  } else {

    matrix_li <- matrix(0, nrow = l, ncol = 2 * c)

    for (i in 1 : l){

      weight_1 <- eigen_list_normalized_values[[i]][1]/sum(eigen_list_normalized_values[[i]])
      weight_2 <- eigen_list_normalized_values[[i]][2]/sum(eigen_list_normalized_values[[i]])
      matrix_li[i,] <- c(weight_1 * eigen_list_vectors[[i]][,1], weight_2 * eigen_list_vectors[[i]][,2])

    }



  }


  # Computing the matrix of k nearest neighbors

  if (k == 1) {

    matrix_knn <- diag(l)

  } else {

  auxiliary_matrix_1 <- as.matrix(stats::dist(matrix_li, upper = T, diag = T))
  auxiliary_matrix_2 <- t(apply(auxiliary_matrix_1, 1, sort))
  vector_division <- auxiliary_matrix_2[, 1 + (k - 1)]
  auxiliary_matrix_3 <- auxiliary_matrix_1/vector_division
  auxiliary_matrix_3[auxiliary_matrix_3 <= 1] <- 1
  auxiliary_matrix_3[auxiliary_matrix_3 > 1] <- 0
  matrix_knn <- auxiliary_matrix_3

  }


  # Constructing the matrix S

  S_matrix <- matrix(0, nrow = l, ncol = l)

  for (i in 1 : l) {

    for (j in 1 : l) {

      if (matrix_knn[i,j] == 1 | matrix_knn[j,i] == 1) {

        S_matrix[i,j] <- exp(-TSdist::EuclideanDistance(matrix_li[i,], matrix_li[j,])^2/2)

      }

    }

  }


  # Constructing the matrix D

  D_matrix <- diag(rowSums(S_matrix))


  # Constructing the matrix L

  L_matrix <- D_matrix - S_matrix

  return_list <- list(matrix_li = matrix_li, D_matrix = D_matrix, L_matrix = L_matrix)

  return(return_list)

}


auxiliary_lpp_function_2 <- function(matrix_li, D_matrix, L_matrix) {

  # Solving the generalized eigenvalue problem

  X_matrix <- t(matrix_li)
  left_matrix <- X_matrix %*% L_matrix %*% t(X_matrix)
  rigth_matrix <- X_matrix %*% D_matrix %*% t(X_matrix)
  geigen_solution <- geigen::geigen(left_matrix, rigth_matrix, symmetric = F)
  order_geigen_values <- order(geigen_solution$values)

  # Obtaining the matrix A_LPP

  A_LPP_matrix <- geigen_solution$vectors[,order_geigen_values]

  # Obtaining the feature matrix

  matrix_p_i <- A_LPP_matrix  %*% t(matrix_li)
  feature_matrix <- t(matrix_p_i)

  return(feature_matrix)

}
