survp.dist <- function(t, base.dist, currentage, parms, xbeta, alpha, res){
  
  A = surv.dist(t=t, base.dist=base.dist, parms=parms, xbeta=xbeta, alpha=alpha, res=0)
  B = surv.dist(t=currentage, base.dist=base.dist, parms=parms, xbeta=xbeta, alpha=alpha, res=0)
  return((A-B)/(1-B)-res)  

} 
  	