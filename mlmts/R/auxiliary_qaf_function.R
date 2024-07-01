

auxiliary_qaf_function <- function(X, Y, levels = c(0.1, 0.5, 0.9), max_lag = 1) {

  k <- length(levels)
  nx <- length(X)
  qx <- stats::quantile(X, probs = levels)
  qy <- stats::quantile(Y, probs = levels)
  gamma <- matrix(0, k, max_lag*k) # Future vector gamma for X, regarding lags

  for (i in 1 : k) {
    for (j in 1 : k) {
      for(p in (1 : max_lag)) {

      index_1 <- which((1 : max_lag) %in% p)
      index_2 <- k * index_1 - (k-1)
      ax <- (X <= qx[i])[1 : (nx - p)]
      bx <- (Y[(1 + p) : nx] <= qy[j])
      gamma[(1 : k), (index_2 : (index_2 + (k-1)))][i, j] = (1/(nx - p))*sum(ax * bx) - levels[i] * levels[j]

      }
    }
  }

  as.vector(gamma)

}


# Auxiliary function 1. Function which computes the quantile cross-covariance

auxiliary_qaf_function_1 <- function(X, j_1, j_2, tau_1, tau_2, l) {


  # Constructing the corresponding UTS

  X_1 <- X[, j_1]
  X_2 <- X[, j_2]
  series_length <- length(X_1)

  # Computing the corresponding quantiles

  q1 <- stats::quantile(X_1, probs = tau_1)
  q2 <- stats::quantile(X_2, probs = tau_2)

 # Computing the series of indicator functions

  indicator_series_1 <- 1 * (X_1 <= q1)
  indicator_series_2 <- 1 * (X_2 <= q2)

 # Computing the corrected series of indicator functions to compute the cross-correlation

  if (l >= 0) {

  indicator_series_corrected_1 <- indicator_series_1[1 : (series_length - l)]
  indicator_series_corrected_2 <- indicator_series_2[(l + 1) : series_length]

  } else {

  indicator_series_corrected_1 <- indicator_series_1[(-l + 1) : series_length]
  indicator_series_corrected_2 <- indicator_series_2[1 : (series_length + l)]

  }

  # Computing the covariance

  cov_indicators <- stats::cov(indicator_series_corrected_1, indicator_series_corrected_2)

  return(cov_indicators)

}



# Auxiliary function 2. Feature extraction

auxiliary_qaf_function_2 <- function(X, levels, max_lag) {


  # Feature extraction when l is not equal to zero

  n_cols <- ncol(X)
  vector_lags <- -max_lag : max_lag
  vector_lags <- vector_lags[vector_lags != 0]

  features_1 <- numeric()
  count <- 1

  for (j_1 in 1 : n_cols) {

    for (j_2 in 1 : n_cols) {

      for (i_1 in levels) {

        for (i_2 in levels) {

          for (k in vector_lags) {

            features_1[count] <- auxiliary_qaf_function_1(X, j_1, j_2, i_1, i_2, k)
            count <- count + 1

         }

        }

      }

    }

  }


  #Feature extraction when l equals 0

  features_2 <- numeric()
  count <- 1

  for (j_1 in 1 : n_cols) {

    for (j_2 in 1 : n_cols) {

      if (j_1 > j_2) {

      for (i_1 in  levels) {

        for (i_2 in levels) {

            features_2[count] <- auxiliary_qaf_function_1(X, j_1, j_2, i_1, i_2, 0)
            count <- count + 1

        }

      }

      }

    }

  }


  vector_features <- c(features_1, features_2)
  return(vector_features)



}
