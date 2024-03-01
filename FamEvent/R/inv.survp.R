inv.survp <- function(val, base.dist, parms, alpha){
	out<-try(uniroot(survp.dist, lower=0,upper=100000, base.dist=base.dist, parms=parms, 
	                 alpha=alpha, xbeta=val[1], currentage=val[2], res=val[3])$root)
  if(is.null(attr(out,"class"))) return(out)
	else print(c(parms, val))
	}
