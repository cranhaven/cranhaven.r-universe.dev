rmsal <-
function(n,p,alpha,sig,mu,pi.g){ 
  x <- matrix(0,n,p+1)
  G <- length(pi.g)
  for(i in 1:n){
    g <- sample(size=1,x=c(1:G),prob=pi.g)
    x[i,] <- c(g,rsal(1,p=p,alpha=alpha[g,],sig=sig[,,g],mu=mu[g,]))
  }
  colnames(x) <- c("Group",paste("V",1:p,sep=""))
  return(x)
}
