#' Computes jackknife variance
#'
#' @param j integer index for deleted observation in the jackknife procedure.
#' @param kernMatrix \code{(n-1)} by \code{m} matrix of kernel values centered at missing
#' observation \code{j} where \code{n} is the total number of observations and \code{m} is the
#' number of candidate bandwidths.
#' @param delta Binary vector of length \code{n} indicating missingness.
#' \code{1} where \code{y} is observed and \code{0} where \code{y} is missing.
#' @param y numeric vector of length \code{n} of observed values and imputed values.
#' @return returns a single numeric value for the estimate of the jackknife variance.

jackknifeVariance = function(j, kernMatrix, delta, y){
  n = nrow(kernMatrix)
  m = ncol(kernMatrix)
  newKernMatrix = kernMatrix[-j,]
  newDelta = delta[-j]
  newy = y[-j]
  newWeight.numerator = newKernMatrix
  newWeight.denominator = colSums(newWeight.numerator)
  newWeightMatrix = newWeight.numerator / matrix(newWeight.denominator,
                                              nrow = n - 1, ncol = m,
                                              byrow = TRUE)
  pihat.jk = colSums(newKernMatrix * newDelta) / colSums(newKernMatrix)
  muhat.jk = colSums(newWeightMatrix * newDelta * newy) / pihat.jk
  mu2hat.jk = colSums(newWeightMatrix * newDelta * newy^2) / pihat.jk
  mu2hat.jk - muhat.jk^2
}
