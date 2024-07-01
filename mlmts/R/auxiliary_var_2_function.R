

auxiliary_function_bic <- function(X, Y, max_p = 2, criterion = 'BIC',...) {

  auxiliary_x <- numeric()
  auxiliary_y <- numeric()

  for (i in 1 : max_p) {

    var_x <- MTS::VARMA(X, p = i, details = F)
    var_y <- MTS::VARMA(Y, p = i, details = F)

    if (criterion == 'BIC') {

      auxiliary_x[i] <- var_x$bic
      auxiliary_y[i] <- var_y$bic

    } else {

      auxiliary_x[i] <- var_x$aic
      auxiliary_y[i] <- var_y$aic

    }

  }


  min_pos_x <- which(auxiliary_x == min(auxiliary_x), arr.ind = T)
  min_pos_y <- which(auxiliary_y == min(auxiliary_y), arr.ind = T)
  k_max <- max(min_pos_x, min_pos_y)


  # Returning the maximum order

  return(k_max)

}


auxiliary_function_combined_model <- function(X, Y, k_max) {

  c <- ncol(X) # Number of variables of both series
  series_length <- nrow(X) # Length of both series
  auxiliary_vector <- (k_max + 1) : series_length

  # Creating the matrices of X and Y

  X_matrix <- t(X[auxiliary_vector,])
  Y_matrix <- t(Y[auxiliary_vector,])


  # Creating the vector Z_v

  Z_vector <- c(c(X_matrix), c(Y_matrix))


  # Creating the matrix B

  B_x <- matrix(0, c * k_max, series_length - k_max)
  B_y <- matrix(0, c * k_max, series_length - k_max)
  count <- 1

  for (j in 1 : k_max) {

    for (i in 1 : c) {

      B_x[count,] <- X[,i][rev((series_length - j) : (k_max - j + 1))]
      B_y[count,] <- Y[,i][rev((series_length - j) : (k_max - j + 1))]
      count <- count + 1

    }

  }

  auxiliary_zero_matrix <- matrix(0, (series_length - k_max) * c, (c * k_max) * c)
  kronecker_1 <- kronecker(t(B_x), diag(c))
  kronecker_2 <- kronecker(t(B_y), diag(c))
  B_v <- rbind(cbind(kronecker_1, auxiliary_zero_matrix), cbind(auxiliary_zero_matrix, kronecker_2))

  lm_x <- stats::lm(c(X_matrix)~kronecker_1)
  lm_y <- stats::lm(c(Y_matrix)~kronecker_2)
  residuals_x <- lm_x$residuals
  residuals_y <- lm_y$residuals
  matrix_residuals_x <- matrix(residuals_x, nrow = c, ncol = series_length - k_max, byrow = F)
  matrix_residuals_y <- matrix(residuals_y, nrow = c, ncol = series_length - k_max, byrow = F)
  cov_x <- (matrix_residuals_x %*% t(matrix_residuals_x))/((series_length - k_max))  # Añadir * c?
  cov_y <- (matrix_residuals_y %*% t(matrix_residuals_y))/((series_length - k_max)) # Añadir * c?
  cov_x_y <- (matrix_residuals_x %*% t(matrix_residuals_y))/((series_length - k_max)) # Añadir * c?


  return_list <- list(Z_v = Z_vector, B_v = B_v, cov_x = cov_x, cov_y = cov_y, cov_x_y = cov_x_y)
  return(return_list)

}


auxiliary_function_p_value <- function(Z_v, B_v, series_length, c, k_max, cov_x, cov_y, cov_x_y) {


  # Fitting a multiple regression model by ordinary least squares

 # linear_model <- lm(Z_v~B_v)
  # vector_residuals <- linear_model$residuals # Vector of residuals
  # l_residuals <- length(vector_residuals)


  # Obtaining estimates of A_x and A_y

  # vector_residuals_x <- vector_residuals[1 : (l_residuals/2)]
  # vector_residuals_y <- vector_residuals[(l_residuals/2 + 1) : l_residuals]
  # matrix_residuals_x <- matrix(vector_residuals_x, nrow = c, ncol = series_length - k_max, byrow = F)
  # matrix_residuals_y <- matrix(vector_residuals_y, nrow = c, ncol = series_length - k_max, byrow = F)


  # Obtaining the estimated covariance matrices

  # cov_x <- (matrix_residuals_x %*% t(matrix_residuals_x))/(c * (series_length - k_max))
  # cov_y <- (matrix_residuals_y %*% t(matrix_residuals_y))/(c * (series_length - k_max))
  # cov_x_y <- (matrix_residuals_x %*% t(matrix_residuals_y))/(c * (series_length - k_max))


  # Obtaining the estimate of V

  element_1 <- kronecker(cov_x, Matrix::Diagonal(series_length - k_max))
  element_2 <- kronecker(cov_x_y, Matrix::Diagonal(series_length - k_max))
  element_3 <- element_2
  element_4 <- kronecker(cov_y, Matrix::Diagonal(series_length - k_max))
  V_estimate <- rbind(cbind(element_1, element_2), cbind(element_3, element_4))


  # Obtaining generalized least squares estimate of gamma_v

  gls <- solve(t(B_v) %*% solve(V_estimate) %*% B_v) %*% t(B_v) %*% solve(V_estimate) %*% Z_v

  # Obtaining the matrix R

  auxiliary_identity <- diag(c^2 * k_max)
  R_matrix <- cbind(diag(c^2 * k_max), -diag(c^2 * k_max))

  # Obtaining the test statistic

  test_statistic <- t(R_matrix %*% gls) %*% solve(R_matrix %*% solve(t(B_v) %*% solve(V_estimate) %*% B_v) %*% t(R_matrix)) %*% (R_matrix %*% gls)

  # Obtaining the p-value

  1 - stats::pchisq(as.numeric(test_statistic), df = c^2 * k_max)

}


auxiliary_var_2_function <- function(X, Y, max_p = 2, criterion = 'BIC',...) {

  series_length <- nrow(X)
  c <- ncol(X)

  max_k <- auxiliary_function_bic(X , Y, max_p = max_p, criterion = criterion)
  combined_model <- auxiliary_function_combined_model(X, Y, max_k)
  p_value <- auxiliary_function_p_value(Z_v = combined_model$Z_v, B_v = combined_model$B_v,
                                        series_length = series_length, c = c, k_max = max_k, cov_x = combined_model$cov_x,
                                        cov_y = combined_model$cov_y, cov_x_y = combined_model$cov_x_y)

  return(p_value)

}
