e.step <-
function(x,G,prmtrs,w=NULL){
  n <- prmtrs$n; p <- prmtrs$p; nu <- prmtrs$nu; pi.g <- prmtrs$pi.g;
  if(is.null(w)) w <- matrix(0,n,G);
  ew.out <- array(NA,dim=c(n,2,G));
  for(g in 1:G){
    alpha <- prmtrs$alpha[g,]; talpha <- prmtrs$talpha[,g]; inv.sig <- prmtrs$inv.sig[,,g]; mu <- prmtrs$mu[g,];
    a <- 2 + alpha%*%inv.sig%*%talpha;
    a <- rep(a,n)
    b <- mahalanobis(x=x,center=mu,cov=inv.sig,inverted=TRUE);
    t1 = exp( (log(a) + log(b))/2 );
    t2 <- log(besselK(t1,nu+1,expon.scaled=TRUE)) - t1;
    t3 <- log(besselK(t1,nu,expon.scaled=TRUE)) - t1;
    dt23 = t2 - t3;
    dlogba = (log(b)-log(a))/2;
    val1 <- dlogba + dt23;
    val1 <- exp(val1);
    val2 <- exp( -1*dlogba+dt23 ) - sign(nu)*exp( log(2)+log(abs(nu)) - log(b));
    val2[is.nan(val2)] <- Inf;
    ew.out[,1,g] <- as.numeric(val1)
    ew.out[,2,g] <- as.numeric(val2)
    w[,g] <- dsal.em(x=x,prmtrs=prmtrs,g,maha=b,ata=a)
  }
  l1 <- apply(w,1,function(z,wt){sum(z*wt)}, wt=pi.g)
  w1 <- apply(w,1,function(h,wt){h*wt/sum(h*wt)}, wt=pi.g);
  return(list(zig.hat=rbind(w1),ew.hat=ew.out,loglik=sum(log(l1))))
}
