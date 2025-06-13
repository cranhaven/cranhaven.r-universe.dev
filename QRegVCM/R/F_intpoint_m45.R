#' Construct Interior Point Algorithm for models 4 and 5
#'
#' This function calculates the estimated alphas (minimizing the P-splines
#'  objective function in Section 3.2 of Gijbels etal 2017). 
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
###### The functions for estimating the median and the full model (Model 4 in
#      the paper) and Model 5
######################################
# Construct Interior Point Algorithm #
######################################
#  This function calculates the estimated alphas (minimizing the P-splines
#  objective function in Section 3.1)
intpoint_gl <- function(subj, U, y, kn, degree, d, lambda, tau, px,range){
  # d is the value of the differencing order
  # degree is degree of splines
  # lambda is the smoothing parameter
  # tau is the quantile of interest (eg. tau=0.5 for median)
  # range is the range of coefficients for each covariate including the intercept
  dim = length(y)
  if (px == length(lambda)) 
    lambdapx = lambda
  else lambdapx = rep(lambda, px)
  W = Weight_Ni(y, subj)$W
  md = numeric(0)
  m = numeric(0)
  vb0 = numeric(0)
  for (k in 1:px) {
    m = c(m, kn[k] + degree[k])
    md = c(md, m[k] - d[k])
    vb0 = c(vb0, rep(0, ((m[k] - d[k]))))
  }
  b = c(W * y, vb0)
  mtot = sum(m)
  dtot = sum(d)
  cum_mB = cumsum(m)
  cum_mA = c(1, c(cum_mB + 1))
  D = list()
  D0 = list()
  DD = matrix(NA, (mtot - dtot), mtot)
  cum_mdB = cumsum(md)
  cum_mdA = c(1, c(cum_mdB + 1))
  rhs_pen = matrix(NA, mtot, px)
  for (k in 1:px) {
    D[[k]] = matrix(0, (m[k] - d[k]), mtot)
    D0[[k]] = diff(diag(m[k]), diff = d[k])
    D[[k]][(1:(m[k] - d[k])), (cum_mA[k]:cum_mB[k])] = lambdapx[k] *range[k]*D0[[k]]
    DD[cum_mdA[k]:cum_mdB[k], ] = D[[k]]
    rhs_pen[, k] = t(D[[k]]) %*% rep(1/2, (m[k] - d[k]))
  }
  DD = as.matrix.csr(DD)
  WU = as.matrix.csr(W * U)
  Fidelity = WU
  Penalty = DD
  FP = rbind(Fidelity, Penalty)
  rhs = (1 - tau) * (t(WU) %*% rep(1, dim)) + rowSums(rhs_pen)
  tmpmax = floor(1e+05 + exp(-12.1) * (WU@ia[mtot] - 1)^2.35)
  fit = rq.fit.sfn(FP, b, rhs = rhs, control = list(tmpmax = tmpmax))
  alpha = fit$coef
  intout = list(alpha = alpha, D = D)
  return(intout)
}

