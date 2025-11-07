#' Jackknife method for selection bandwidth
#'
#' @param i integer referring to the index of the missing value to be imputed.
#' @param bwGrid numeric vector of candidate bandwidth values
#' @param preds numeric vector of predicted values for missing observations
#' @param y numeric vector of length \code{n} of observed and imputed values.
#' @param delta Binary vector of length \code{length(y)} indicating missingness.
#' \code{1} where \code{y} is observed and \code{0} where \code{y} is missing.
#' @param kernel one of \code{gaussian}, \code{uniform}, or \code{triangular}.
#' Specifies the kernel to be used in estimating the distribution around a missing value.
#' @return bandwidth

jackknifeBandwidthSelection = function(i, bwGrid, preds, y, delta, kernel){
  if(kernel[1] == "gaussian"){
    kernGrid = lapply(bwGrid, gaussianKernel, x = preds,
                        xcenter = preds[i], lambda = NULL)
  }
  else if(kernel[1] == "uniform"){
    kernGrid = lapply(bwGrid, uniformKernel, x = preds,
                        xcenter = preds[i], lambda = NULL)
  }
  else if(kernel[1] == "triangular"){
    kernGrid = lapply(bwGrid, triangularKernel, x = preds,
                        xcenter = preds[i], lambda = NULL)
  }
  kernMatrix = do.call(cbind, kernGrid)
  n = nrow(kernMatrix)
  m = length(bwGrid)

  weight.numerator = kernMatrix
  weight.denominator = colSums(weight.numerator)
  weightMatrix = weight.numerator / matrix(weight.denominator, nrow = n,
                                           ncol = m, byrow = TRUE)

  pihat.fullData = colSums(kernMatrix * delta) / colSums(kernMatrix)
  muhat.fullData = colSums(weightMatrix * delta * y) / pihat.fullData
  mu2hat.fullData = colSums(weightMatrix * delta * y^2) / pihat.fullData
  sig2hat.fullData = mu2hat.fullData - muhat.fullData^2

  sig2hat.jk = lapply((1:n)[delta == 1], jackknifeVariance,
                      kernMatrix = kernMatrix, delta = delta, y = y)

  sig2hat.jk = do.call(rbind, sig2hat.jk)

  bias2 = (sig2hat.fullData - colMeans(sig2hat.jk))^2
  s2 = colSums((sig2hat.jk - matrix(colMeans(sig2hat.jk),
                                    nrow = nrow(sig2hat.jk), ncol = m,
                                    byrow = TRUE))^2) / (n * (n - 1))
  mse = bias2 + s2

  return(bwGrid[which.min(mse)])
}
