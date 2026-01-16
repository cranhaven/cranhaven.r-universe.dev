

auxiliary_dtw_mahalanobis_function <- function(X, Y, M) {

  c <- ncol(X)
  l_1 <- nrow(X)
  l_2 <- nrow(Y)

cost_matrix <- matrix(0, nrow = l_1, ncol = l_2)

  for (i in 1 : l_1) {

    for (j in 1 : l_2) {

      cost_matrix[i, j] <- t(X[i,] - Y[j,]) %*% M %*% (X[i,] - Y[j,])

    }

  }

return(cost_matrix)

}

auxiliary_dtw_mahalanobis_function_extra <- function(X, Y) {

  c <- ncol(X)
  l_1 <- nrow(X)
  l_2 <- nrow(Y)
  cov_x <- stats::cov(X)
  cov_y <- stats::cov(Y)

  cost_matrix <- matrix(0, nrow = l_1, ncol = l_2)

  for (i in 1 : l_1) {

    for (j in 1 : l_2) {

      dis_mah_1 <-  t(X[i,] - Y[j,]) %*% cov_x %*%  (X[i,] - Y[j,])
      dis_mah_2 <-  t(X[i,] - Y[j,]) %*% cov_y %*%  (X[i,] - Y[j,])
      dis_mah_1_2 <- 0.5*(dis_mah_1 + dis_mah_2)
      cost_matrix[i, j] <- dis_mah_1_2

    }

  }

  return(cost_matrix)

}



