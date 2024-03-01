inv.surv <- function(val, base.dist, parms, alpha){
	out<-try(uniroot(surv.dist, lower=0,upper=100000, base.dist=base.dist, parms=parms, alpha=alpha, xbeta=val[1], res=val[2])$root)
  if(is.null(attr(out,"class"))) return(out)
	else print(c(parms, val))
	}
