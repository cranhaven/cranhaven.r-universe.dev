inv_surv_c <- function(val, base.dist, parms, alpha){
	out<-try(uniroot(surv_dist_c, lower=0,upper=100000, base.dist=base.dist, parms=parms, alpha=alpha, xbeta=c(val[1], val[2]), res=val[3])$root)
  if(is.null(attr(out,"class"))) return(out)
	else print(c(parms, val))
	}