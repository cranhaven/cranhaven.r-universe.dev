#' @export
lambdak_gconl <- function(times, subj, X, y, d, tau, kn, degree, lambda, gam,omega,v){
  dim = length(subj)
  X = matrix(X, nrow = dim)
  px = ncol(X)
  qvc2_indiv = qrvcp_gconl(times, subj, y, X, tau, kn = kn, 
                           degree = degree, lambda = 0, d,omega,rep(1,px),v)$hat_bt
  range = NULL
  for (k in 1:px) {
    range[k] =(max(qvc2_indiv[seq((k - 
                                     1) * dim + 1, k * dim)]) - min(qvc2_indiv[seq((k - 
                                                                                      1) * dim + 1, k * dim)]))^(-(gam))
  }
  lambdasic = Lamb_gconl(times, subj, X, y, d, tau, kn, degree, 
                         lambda,omega,range,v)$lambdasic
  out = list(lambdasic = lambdasic, range = range)
  return(out)
}
