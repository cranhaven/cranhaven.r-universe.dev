dmsal <-
function(x,alpha,sig,mu,pi.g){
  n <- nrow(x)
  G <- length(pi.g)
  unwtd.dens <- matrix(NA,n,G)
  for(g in 1:G) unwtd.dens[,g] <- dsal(x=x,alpha=alpha[g,],sig=sig[,,g],mu=mu[g,])
  val <- apply(unwtd.dens,1,function(z,wt){sum(z*wt)}, wt=pi.g)
  return(val)
  }
