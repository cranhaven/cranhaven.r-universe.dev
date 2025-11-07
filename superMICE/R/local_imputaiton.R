#' Function to generate imputations using non-parametric and semi-parametric local imputation methods.
#'
#' @param i integer referring to the index of the missing value to be imputed.
#' @param preds numeric vector of predictions of missing values from SuperLearner.
#' @param y numeric vector for variable to be imputed.
#' @param delta binary vector of length \code{length(y)} indicating missingness.
#' \code{1} where \code{y} is observed and \code{0} where \code{y} is missing.
#' @param bw \code{NULL} or numeric value for bandwidth of kernel function (as standard deviations of the kernel).
#' @param kernel one of \code{gaussian}, \code{uniform}, or \code{triangular}.
#' Specifies the kernel to be used in estimating the distribution around a missing value.
#' @return numeric vector of randomly drawn imputed values.
#'
#' @importFrom stats rnorm



localImputation <- function(i, preds, y, delta, bw = NULL,
                            kernel = c("gaussian", "uniform", "triangular")){

  if(kernel[1] == "gaussian"){
    kernVals = gaussianKernel(x = preds, xcenter = preds[delta == 0][i],
                              bw = bw[[i]], lambda = NULL)
  }
  else if(kernel[1] == "uniform"){
    kernVals = uniformKernel(x = preds, xcenter = preds[delta == 0][i],
                             bw = bw[[i]], lambda = NULL)
  }
  else if(kernel[1] == "triangular"){
    kernVals = triangularKernel(x = preds, xcenter = preds[delta == 0][i],
                                bw = bw[[i]], lambda = NULL)
  }

  weights = kernVals / sum(kernVals)
  pihat = sum(kernVals * delta) / sum(kernVals)
  muhat = sum(weights * delta * y / pihat)
  sig2hat = sum(weights * delta * y^2 / pihat) - muhat^2
  rnorm(1, preds[delta == 0][i], sqrt(sig2hat))
}
