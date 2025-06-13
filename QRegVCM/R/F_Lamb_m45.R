#' The global lambda for models 4 and 5 
#'
#' This function searches for the smoothing parameter using SIC.
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
# This function searches for the smoothing parameter using SIC
Lamb_gl <- function(times, subj, X, y, d, tau, kn, degree, lambda,range){
  dim = length(subj)
  X = matrix(X, nrow = dim)
  px = ncol(X)
  n = length(unique(subj))
  lambda = unique(lambda)
  nlam = length(lambda)
  dim = length(y)
  W = Weight_Ni(y, subj)$W
  yhatsic = matrix(NA, dim, nlam)
  ressic = matrix(NA, dim, nlam)
  SIC = NULL
  plam = NULL
  for (i in 1:nlam) {
    qvcsic = qrvcp_gl(times, subj, y, X, tau, kn, degree, 
                      lambda = lambda[i], d,range)
    hat_btsic = qvcsic$hat_bt
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    yhatsic[, i] = rowSums(yhatsic_k)
    ressic[, i] = y - yhatsic[, i]
    plam[i] = length(ressic[which(abs(ressic[, i]) < (10^(-2)))])
    SIC[i] = log(sum(W * ressic[, i] * (tau - 1 * (ressic[,i] < 0)))/n) +
      log(dim) * plam[i]/(dim * 2)
  }
  lambdasic = lambda[which(SIC == min(SIC))]
  Lout = list(lambdasic = lambdasic)
  return(Lout)
}
