#' The lambdas for each coavariate for models 2 and 3 
#'
#' This function calculates the range of the Beta_hats using B-splines and then
#'  using Lamb_grl() it searches for the global smoothing parameter
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
lambdak_grl <- function(times, subj, X, y, d, tau, kn, degree, lambda, gam){
  dim = length(subj)
  X = matrix(X, nrow = dim)
  px = ncol(X)-1
  qvc2_indiv = qrvcp_grl(times, subj, y, X, tau, kn = kn, 
                         degree = degree, lambda = 0, d,rep(1,px))$hat_bt
  range = NULL
  for (k in 1:px) {
    range[k] =(max(qvc2_indiv[seq((k - 1) * dim + 1, k * dim)]) -
                 min(qvc2_indiv[seq((k - 1) * dim + 1, k * dim)]))^(-(gam))
  }
  lambdasic = Lamb_grl(times, subj, X, y, d, tau, kn, degree, 
                       lambda,range)$lambdasic
  out = list(lambdasic = lambdasic, range = range)
  return(out)
}
