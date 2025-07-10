#' @import stats
cb = function(x,beta.post,mu2.post,qun){
  vec.fp = rep(x,length(beta.post))
  sroc = pnorm(-(qnorm(1-x)*exp(-beta.post/2)-mu2.post)/exp(beta.post/2))
  quantile = quantile(sroc,qun)
  return(quantile)
}
