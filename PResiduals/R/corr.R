#' Calculates the weighted correlation given a data set and a set of weights.
#'
#' This is a copy of corr function from the boot package. It calculates the correlation coefficient in weighted form.
#' @param d a matrix with two columns corresponding to the two variables whose correlation we wish to calculate.
#' @param w a vector of weights to be applied to each pair of observations. The default is equal weights for each pair. Normalization takes place within the function so sum(w) need not equal 1.
#' @return the correlation coefficient between d[,1] and d[,2].
#' @keywords correlation


corr <- function (d, w = rep(1, nrow(d))/nrow(d)) {
  s <- sum(w)
  m1 <- sum(d[, 1L] * w)/s
  m2 <- sum(d[, 2L] * w)/s
  (sum(d[, 1L] * d[, 2L] * w)/s - m1 * m2)/sqrt((sum(d[, 1L]^2 *  w)/s - m1^2) * (sum(d[, 2L]^2 * w)/s - m2^2))
}



