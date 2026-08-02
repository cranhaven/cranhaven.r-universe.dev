#' eigp_sampling
#' @title The Random Number Generation Function for EIGP
#' @description  Create a EIGP random sample.
#' @param n Number of observations. (n>=1)
#' @param theta The location parameter for the parent IGP distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The parameter should be positive.
#' @import mistr
#' @return This function returns a numerical vector of size n.
#' @examples
#' eigp_sampling(100,1,1)
#' @export
eigp_sampling = function(n,theta,eta){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298
  a = alpha - k
  C=mistr::compdist((1/mistr::gammadist(shape = alpha,rate = (k*theta))), mistr::paretodist(scale = theta,shape = a), weights =     c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); gamma(beta = k*theta:rate; alpha:shape)
  sim = mistr::r(C,n)
  return(sim)
}

#' eep_sampling
#' @title The Random Number Generation Function for EIGP
#' @description  Create a EEP random sample.
#' @param n Number of observations. (n>=1)
#' @param theta The location parameter for the parent EP distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The parameter should be positive.
#' @import mistr
#' @return returns a numerical vector of size n.
#' @examples
#' eep_sampling(100,1,1)
#' @export
eep_sampling = function(n,theta,eta){
  c = 0.574
  alpha = 0.349976
  exp_pareto=mistr::compdist(mistr::expdist(rate = (alpha+1)/theta), mistr::paretodist(scale = theta,shape = alpha), weights = c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); exp: rate = (alpha+1)/theta
  sim = mistr::r(exp_pareto,n)
  return(sim)
}
