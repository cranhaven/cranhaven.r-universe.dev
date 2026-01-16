


auxiliary_cor_function <- function(X, lag_max = 1) {

  # First, we extract the autocorrelations from each UTS

  c <- ncol(X)
  autocorrelations <- matrix(0, nrow = c, ncol = lag_max)


  for (i in 1 : c) {

    autocorrelations[i,] <- as.vector(stats::acf(X[,i], plot = F,
                                          lag.max = lag_max)$acf)[-1]

  }

  features_autocorrelations <- c(autocorrelations)


  # Second, we extract the cross-correlations between each nonordered
  # pair of UTS


  cross_correlations <- list()

  k <- 0
  for (i in 1 : c) {
    for (j in (i + 1) : c) {

      if ((i + 1) <= c) {

        k <- k + 1
        cross_correlations[[k]] <- as.vector(stats::ccf(X[,i],
                                                 X[,j], lag.max = lag_max,
                                                 plot = F)$acf)
      }

    }

  }

  features_cross_correlations <- unlist(cross_correlations)
  features_cross_correlations <- features_cross_correlations[!is.na(features_cross_correlations)]

  vector_features <- c(features_autocorrelations, features_cross_correlations)
  vector_features

}
