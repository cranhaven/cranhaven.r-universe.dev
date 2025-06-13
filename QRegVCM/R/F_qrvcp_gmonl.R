#' @export
qrvcp_gmonl <- function(times, subj, y, X, tau, kn, degree, lambda, d,omega,range,mv){
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
    B[[k]] = bbase(times, min(times), max(times), kn[k], 
                   degree[k])
  }
  cum_mB = cumsum(m)
  cum_mA = c(1, c(cum_mB + 1))
  U = NULL
  for (k in 1:px) {
    U = cbind(U, X[, k] * B[[k]])
  }
  alpha = intpoint_gmonl(subj, U, y, kn, degree, d, lambda, 
                         tau, px,omega,range,mv)$alpha
  coef.X = matrix(NA, dim, px)
  for (k in 1:px) {
    coef.X[, k] = B[[k]] %*% alpha[cum_mA[k]:cum_mB[k]]
  }
  hat_bt = c(coef.X)
  out = list(hat_bt = hat_bt, alpha = alpha)
  return(out)
}