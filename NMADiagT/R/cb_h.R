#' @import stats
cb_h= function(x,alpha.post,beta.post,cova,vari,qun){
  b=qnorm(1-pnorm(beta.post))
  vec.fp = rep(x,length(alpha.post))
  varn = rep(vari,length(alpha.post))
  covn = rep(cova,length(alpha.post))
  sroc = pnorm(alpha.post+covn/varn*(qnorm(x)-b))
  quantile = quantile(sroc,qun)
  return(quantile)
}
