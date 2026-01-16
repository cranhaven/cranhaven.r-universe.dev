

auxiliary_ppca_function_1 <- function(X, w = 2) {


  series_length <- nrow(X)
  d <- ncol(X)


  # Constructing the segments in the time direction

  length_segments <- series_length/w
  indexes_series <- 1 : series_length
  indexes_segments <- base::split(indexes_series, ceiling(seq_along(indexes_series)/length_segments))


  # Constructing a list with the segments

  list_segments <- list()

  for(i in 1 : w) {

    list_segments[[i]] <- X[indexes_segments[[i]],]

  }


  # Constructing a list with the covariance matrix of the segments

  list_covariance_matrices <- list()

  for(i in 1 : w) {

    list_covariance_matrices[[i]] <- stats::cov(list_segments[[i]])

  }


  # Constructing the average covariance matrix

  sigma_a <- Reduce(`+`, list_covariance_matrices)/w

  return(sigma_a)

}




auxiliary_ppca_function_2 <- function(X, w = 2, var_rate = 0.90) {


  series_length <- nrow(X)
  d <- ncol(X)


  # Centering the series and computing the average covariance matrix

  X_centered <- base::scale(X, center = T, scale = F)
  sigma_a <- auxiliary_ppca_function_1(X = X_centered, w = w)


  # SVD for the average covariance matrix

  svd_a <- eigen(sigma_a)
  auxiliary <- cumsum(svd_a$values)/sum(svd_a$values)
  retained_components <- which.max(auxiliary >= var_rate)
  v_a_r <- svd_a$vectors[, 1 : retained_components]


  # Computing the reduced series

  reduced_series <- X %*% v_a_r

  return(reduced_series)

}
