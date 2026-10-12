EM <-
function(x,G,prmtrs,max.it=10000,eps=1e-2,mu.tol=1e-14,print.it="FALSE",print.warn="FALSE",print.prmtrs="FALSE"){
  n <- prmtrs$n
  curr.mu <- matrix(NA,G,prmtrs$p)
  loglik <- NULL
  i <- 1;
  a1 <- 0;
  a2 <- eps + runif(1,0,1);
  while(abs(a1 - a2) > eps){
    if(print.it=="TRUE") cat("Iteration",i,"\n")
    exp.vals <- e.step(x=x,G=G,prmtrs=prmtrs);
    prmtrs$n.g <- rowSums(exp.vals$zig.hat)
    prmtrs$pi.g <- rowMeans(exp.vals$zig.hat)
    loglik[i] <- exp.vals$loglik;
    if(i > 1){ if(loglik[i] < loglik[i-1]){ stop(paste("Likelihood Decreased On Iteration",i,sep=" ")) } };
    a1 <- aitkens(lval=loglik,i=i,eps=eps); #print("a1"); print(a1)
    if(i > 2) a2 <- loglik[i-1]; #print("a2"); print(a2)		
    for(g in 1:G) curr.mu[g,] <- prmtrs$mu[g,];
    prmtrs <- m.step(x=x,G=G,exp.vals=exp.vals,prmtrs=prmtrs);
    for(g in 1:G){
      if(any(as.numeric(apply(abs(sweep(x,2,prmtrs$mu[g,],"-")),1,sum)) < mu.tol)){
        if(print.warn=="TRUE"){
          print(paste("Warning, the value of mu in component ",g," is very close to observation ",
                      which(as.numeric(apply(abs(sweep(x,2,prmtrs$mu[g,],"-")),1,sum)) < mu.tol),"'s value",sep=""))
        }
        prmtrs$mu[g,] <- curr.mu[g,]
        prmtrs$alpha[g,] <- set.back(x=x,g=g,exp.vals=exp.vals,prmtrs=prmtrs);
        prmtrs$talpha[,g] <- prmtrs$alpha[g,]
      }
      if(rcond(prmtrs$sig[,,g]) <= sqrt(.Machine$double.eps)) stop(paste("Singular Scale Matrix Detected In Group",g,"On Iteration",i,sep=" "))
      if(!isSymmetric(prmtrs$sig[,,g])) stop(paste("On iteration",i,"the scale matrix in group",g,"is not symmetric",sep=" "))
      if(!isSymmetric(prmtrs$inv.sig[,,g])){ 
        if(print.warn == "TRUE") print(paste("On iteration",i,"the inverse of the scale matrix in group",g,"is not symmetric",sep=" "))
        prmtrs$inv.sig[,,g] <- solve(prmtrs$sig[,,g])
        if(!isSymmetric(prmtrs$inv.sig[,,g])) stop(paste("On iteration",i,"the inverse of the scale matrix in group",g,"is not symmetric",sep=" "))
      }
    }
    if(print.prmtrs=="TRUE") print(prmtrs)
    if(i == max.it) break
    i <- i + 1
  }	
  return(list(prmtrs=prmtrs,loglik=loglik))
}
