prmtrs.init <-
function(x,G,start=1){
  n <- nrow(x); 
  p <- ncol(x);
  a <- numeric(G)
  b <- matrix(NA,n,G)
  init.prmtrs <- list();
  if(length(start) == 1 && start == 1) pred <- kmeans(x=x,centers=G,iter.max=200,nstart=50)$cluster
  else if (length(start) == 1 && start == 2) pred <- sample(1:G,n,replace=TRUE)
  else if (length(start) > 1) pred <- start;
  wt <- matrix(0,n,G);
  init.prmtrs <- list(alpha=matrix(NA,G,p),talpha=matrix(NA,p,G),sig=array(NA,dim=c(p,p,G)),inv.sig=array(NA,dim=c(p,p,G)),log.det.sig=numeric(G),mu=matrix(NA,G,p));
  for(g in 1:G){
    wt[which(pred == g),g] <- 1;
    temp <- cov.wt(x=x,wt=wt[,g],center=TRUE,method="ML");
    init.prmtrs$alpha[g,] <- rep(0,p);
    init.prmtrs$talpha[,g] <- rep(0,p);
    init.prmtrs$mu[g,] <- temp$center;
    init.prmtrs$sig[,,g] <- temp$cov;
    init.prmtrs$inv.sig[,,g] <- solve(init.prmtrs$sig[,,g]);
    init.prmtrs$log.det.sig[g] <- log(det(init.prmtrs$sig[,,g]));
  }	
  init.prmtrs$n.g <- apply(wt,2,sum);
  init.prmtrs$pi.g <- apply(wt,2,mean);	
  init.prmtrs$n <- n;
  init.prmtrs$p <- p;
  init.prmtrs$nu <- (2-p)/2;
  return(init.prmtrs)
}
