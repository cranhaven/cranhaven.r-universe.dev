

auxiliary_zagorecki_function_1 <- function(X, freq) {

  spectral_density <- freqdom::spectral.density(X, freq = freq)
  spectral_values <- c(spectral_density$operators)
  spectral_frequencies <- spectral_density$freq

  max_value <- max(Mod(spectral_values))
  associated_frequency <- which.max(base::Mod(spectral_values))

  c(max_value, associated_frequency)

}




auxiliary_zagorecki_function_2 <- function(X){


  n_rows <- base::nrow(X)
  c <- base::ncol(X)

  # Means

  means <- base::colMeans(X)


  # Maximal values

  maxs <- apply(X, 2, base::max)


  # Minimal values

  mins <- apply(X, 2, base::min)


  # Ranges

  ranges <- maxs - mins


  # Sums of squares

  sums_sq <- apply(X, 2, function(x) {base::sum(x^2)})


  # Logarithm of the sum of squares

  log_sums_sq <- base::log(sums_sq)


  # Standard deviations

  sds <- apply(X, 2, stats::sd)


  # Skewnesses

  skewnesses <- apply(X, 2, e1071::skewness)


  # Kurtosises

  kurtosises <- apply(X, 2, e1071::kurtosis)


  # 5 th central moment

  moments_5 <- apply(X, 2, e1071::moment, order = 5, center = T)


  # Maximal differences between consecutive measurements

  max_diffs <- apply(X, 2, function(x) {base::max(diff(x))})


  # Autocorrelations at 1, 2, 5 20 and 50

  autocorr <- as.vector(apply(X, 2, function(x) {TSA::acf(x, lag.max = 50, plot = F)$acf[c(1, 2, 5, 20, 50)]}))


  # Correlations between each pair of components

  cors <- numeric()

  count <- 1
  for (i in 1 : c) {

    for (j in (i + 1) : c) {

      if (i + 1 <= c){

        cors[count] <- stats::cor(X[,i], X[,j])
        count <- count + 1

      }

    }

  }


  # Slope and intercept and MSE for the linear regression with each pair of UTS as explanatory
  # and response variable, respectively, and MSE

  intercepts <- numeric()
  slopes <- numeric()
  mses <- numeric()

  count <- 1
  for (i in 1 : c) {

    for (j in 1 : c) {

      if (i != j) {

        model <- stats::lm(X[,i]~X[,j])
        coef_1 <- as.numeric(model$coefficients[1])
        coef_2 <- as.numeric(model$coefficients[2])
        coef_3 <- as.numeric(mean(model$residuals^2))
        intercepts[count] <- coef_1
        slopes[count] <- coef_2
        mses[count] <- coef_3
        count <- count + 1

      }



    }

  }

  coefs_linear_regression <- c(intercepts, slopes, mses)


  # Parameters for polynomial fitting

  vector_coefs_1 <- numeric()
  vector_coefs_2 <- numeric()
  vector_coefs_3 <- numeric()

  count <- 1
  for (i in 1 : c) {

    for (j in 1 : c) {

      if (i != j) {

        model <- stats::lm(X[,i]~poly(X[,j], 2, raw = T))
        coef_1 <- as.numeric(model$coefficients[1])
        coef_2 <- as.numeric(model$coefficients[2])
        coef_3 <- as.numeric(model$coefficients[3])
        vector_coefs_1[count] <- coef_1
        vector_coefs_2[count] <- coef_2
        vector_coefs_3[count] <- coef_3
        count <- count + 1


      }





    }

  }

  coefs_polynomial_fitting <- c(vector_coefs_1, vector_coefs_2, vector_coefs_3)



  # Maximum values (modulus) and corresponding frequencies of Fourier Transform

  fourier_frequencies <- c((2 * pi * (1 : ceiling((n_rows/2))))/n_rows) # Fourier frequencies excluding 0th frequency
  spectral_values <- c(apply(X, 2, auxiliary_zagorecki_function_1, freq = fourier_frequencies[fourier_frequencies < pi]))


  # Concatenating the features

  features <- c(means, maxs, mins, ranges, sums_sq, log_sums_sq, sds,
                skewnesses, kurtosises, moments_5, max_diffs, autocorr,
                cors, coefs_linear_regression, coefs_polynomial_fitting, spectral_values)

  return(features)

}




