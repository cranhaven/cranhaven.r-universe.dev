LP.struct.test<-function(W,m=NULL,n.iter=50){
   n=nrow(W)
   if(is.null(m)){
     LP.stat.obs <- sum(W)/n*sum(svd(Laplacian(W))$d^2)
   }else{
     L <- sqrt(sum(W)/m)*LPSpectral(W,k=1,m=m,sparse=FALSE)$LP
     LP.stat.obs <- sum(svd(L)$d^2)
   }
   LPstat=rep(NA,n.iter)
   for (iter in 1:n.iter){
      ind <- sample(1:n,n)
      W1 <- W[ind, ind] 
      if(is.null(m)){
         LPstat[iter]<- sum(W1)/n*sum(svd(Laplacian(W1))$d^2)
      }else{
         L1 <- sqrt(sum(W1)/m)*LPSpectral(W1,k=1,m=m,sparse=FALSE)$LP
         LPstat[iter]<-sum(svd(L1)$d^2)
      }

   }
   out<-list()
   out$statistics<-LP.stat.obs
   out$pval<-1-pnorm(LP.stat.obs, mean=mean(LPstat),sd=sd(LPstat))
   return(out)
}