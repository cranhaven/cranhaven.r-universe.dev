

# j_divergence

j_divergence <- function(X, Y){

  n <- nrow(X)
  specx <- freqdom::spectral.density(X, freq = ((2 * pi * (0 : floor((nrow(X)/2))))/nrow(X)))$operators
  specy <- freqdom::spectral.density(Y, freq = ((2 * pi * (0 : floor((nrow(Y)/2))))/nrow(Y)))$operators
  d <- dim(specx)[3]

  coeff <- (1/2)*n^(-1)
  vector <- numeric(d)

  for (i in 1 : d) {

    vector[i] <- psych::tr(specx[,,i] %*% solve(specy[,,i])) + psych::tr(specy[,,i] %*% solve(specx[,,i]))  - 2 * ncol(X)

  }

  Re(coeff * sum(vector))

}


# chernoff_divergence

chernoff_divergence <- function(X, Y, alpha = 0.5){

  n <- nrow(X)
  specx <- freqdom::spectral.density(X, freq = ((2 * pi * (0 : floor((nrow(X)/2))))/nrow(X)))$operators
  specy <- freqdom::spectral.density(Y, freq = ((2 * pi * (0 : floor((nrow(Y)/2))))/nrow(Y)))$operators
  d <- dim(specx)[3]

  coeff <- (1/2)*n^(-1)
  vector <- numeric(d)

  for (i in 1 : d) {

    a <-  log(complexplus::Det(alpha*specx[,,i] + (1-alpha) * specy[,,i])/complexplus::Det(specy[,,i]))
    b <-  log(complexplus::Det(alpha*specy[,,i] + (1-alpha) * specx[,,i])/complexplus::Det(specx[,,i]))
    vector[i] <- a + b

  }

  Re(coeff * sum(vector))

}


# spectral features

spectral_features <- function(X) {

  n <- nrow(X)
  specx <- freqdom::spectral.density(X, freq = ((2 * pi * (0 : ceiling((nrow(X)/2))))/nrow(X)))$operators
  features_complex_vector <- c(specx)
  features_complex_vector_re_im <- c(Re(features_complex_vector), Im(features_complex_vector))
  features_complex_vector_re_im

}



