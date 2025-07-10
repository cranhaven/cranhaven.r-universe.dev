#' FFT: Compute F_n X_n with the real-valued Fourier matrix F_n
#' @keywords internal
fast_ft <- function(x) {
  
  # Function computes FZ (i.e. fast Fourier transformed data)
  # Outputs coefficients in correct order and rescaled
  # NOTE: x must be mean-centered
  
  n <- length(x)
  sqrt2 <- sqrt(2)
  sqrtn <- sqrt(n)
  
  # Cyclically shift so last observation becomes first
  x <- c(x[n], x[-n])  # Important since fft() uses 0:(n-1) but we use 1:n
  
  # FFT
  fourier <- stats::fft(x)
  
  # Extract non-redundant real and imaginary coefficients in correct order and rescale
  FZ <- rep(NA, n)
  
  FZ[1] <- Re(fourier[1]) # First coefficient
  
  if (n %% 2) {  # Odd length time series
    N <- (n - 1) / 2
    FZ[2 * (1:N)] <- sqrt2 * Re(fourier[2:(N + 1)]) # Real coefficients
    FZ[2 * (1:N) + 1] <- sqrt2 * Im(fourier[2:(N + 1)]) # Imaginary coefficients
  } 
  else {  # Even length time series
    FZ[n] <- Re(fourier[n / 2 + 1]) # Last coefficient
    FZ[2 * 1:(n / 2 - 1)] <- sqrt2 * Re(fourier[2:(n / 2)]) # Real coefficients
    FZ[2 * 1:(n / 2 - 1) + 1] <- sqrt2 * Im(fourier[2:(n / 2)]) # Imaginary coefficients
  }
  
  return(FZ / sqrtn)
  
}

#' Help function: Uniform maximum
#' @keywords internal
uniformmax <- function(sample) {
  max(abs(sample - stats::median(sample)) / stats::mad(sample), na.rm = TRUE)
}

#' Help function: Fuller Logarithm
#' @keywords internal
logfuller<-function(x, xi = 0.001){
  log(x + xi) - xi / (x + xi)
}

#' Analytical spectral density for mean-centred ARMA(p,q) model
#' @keywords internal
psd_arma <- function(freq, ar, ma, sigma2) {
  
  # Analytical spectral density for ARMA(p,q) model
  # Assumes no intercept terms - i.e., mean centred
  
  # freq: frequencies defined on [0, pi]
  # ar: AR coefficients of length p - must be named
  # ma: MA coefficients of length q - must be named
  # sigma2: Variance of white noise
  
  # MA component (numerator)
  if (any(is.na(ma))) {
    numerator <- rep(1, length(freq))
  }
  else {
    numerator <- matrix(NA, ncol = length(ma), nrow = length(freq))
    for (j in 1:length(ma)) {
      numerator[, j] <- ma[j] * exp(-1i * j * freq)
    }
    numerator <- Mod(1 + apply(numerator, 1, sum)) ^ 2  # Note the PLUS
  }
  
  # AR component (denominator)
  if (any(is.na(ar))) {
    denominator <- rep(1, length(freq))
  }
  else {
    denominator <- matrix(NA, ncol = length(ar), nrow = length(freq))
    for (j in 1:length(ar)) {
      denominator[, j] <- ar[j] * exp(-1i * j * freq)
    }
    denominator <- Mod(1 - apply(denominator, 1, sum)) ^ 2  # Note the MINUS
  }
  
  psd <- (sigma2 / (2 * pi)) * (numerator / denominator)
  
  return(psd)
  
}









