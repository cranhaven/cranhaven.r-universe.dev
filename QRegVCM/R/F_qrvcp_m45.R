#' QRVCP for models 4 and 5
#'
#' This function calculates the Betas using the alphas obtained from intpoint_gl().
#'
#' @section Note:
#' Some warning messages are related to the function \code{\link{rq.fit.sfn}} 
#' (See http://www.inside-r.org/packages/cran/quantreg/docs/sfnMessage).
#' 
#' @section Author(s):
#' Mohammed Abdulkerim Ibrahim
#' 
#' @section Refrences:
#' Gijbels, I., Ibrahim, M. A., and Verhasselt, A. (2017). Testing the 
#' heteroscedastic error structure in quantile varying coefficient models. 
#' {\it Submitted}.
#' 
#' Andriyana, Y. (2015). P-splines quantile regression in varying coefficient 
#' models. {\it PhD Dissertation}. KU Leuven, Belgium. ISBN 978-90-8649-791-1.
#' 
#' Andriyana, Y. and Gijbels, I. & Verhasselt, A. (2014). P-splines quantile 
#' regression estimation in varying coefficient models. {\it Test}, 23, 153-194.
#' 
#' Andriyana, Y., Gijbels, I. and Verhasselt, A. (2017). Quantile regression 
#' in varying-coefficient models: non-crossing quantile curves and 
#' heteroscedasticity. {\it Statistical Papers}, to appear.
#' DOI:10.1007/s00362-016-0847-7
#' 
#' He, X. (1997). Quantile curves without crossing. {\it The American Statistician},
#'  51, 186-192.
#'
#' @seealso \code{\link{rq.fit.sfn}} \code{\link{as.matrix.csr}} 
#'
#' @export
qrvcp_gl = function(times, subj, y, X, tau, kn, degree, lambda, d,range){
  dim = length(subj)
  X = matrix(X, nrow = dim)
  px = ncol(X)
  dim = nrow(X)
  if (px != length(kn) || px != length(degree) || px != length(d)) 
    stop("the number of covariate(s) and the length of kn, degree, and d must match")
  if (dim != length(y) || dim != length(subj)) 
    stop("dimension of X, y, subj must match")
  m = numeric(0)
  B = list()
  for (k in 1:px) {
    m = c(m, kn[k] + degree[k])
    B[[k]] = bbase(times, min(times), max(times), kn[k], degree[k])
  }
  cum_mB = cumsum(m)
  cum_mA = c(1, c(cum_mB + 1))
  U = NULL
  for (k in 1:px) {
    U = cbind(U, X[, k] * B[[k]])
  }
  alpha = intpoint_gl(subj, U, y, kn, degree, d, lambda, tau, px,range)$alpha
  coef.X = matrix(NA, dim, px)
  for (k in 1:px) {
    coef.X[, k] = B[[k]] %*% alpha[cum_mA[k]:cum_mB[k]]
  }
  hat_bt = c(coef.X)
  out = list(hat_bt = hat_bt, alpha = alpha)
  return(out)
}