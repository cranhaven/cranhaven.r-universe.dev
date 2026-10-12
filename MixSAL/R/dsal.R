dsal <-
function(x,alpha,sig,mu){
  x <- as.matrix(x)
  talpha <- as.matrix(alpha,nrow=1,ncol=2);
  alpha <- t(talpha);
  n <- nrow(x);
  p <- ncol(x);
  nu <- (2-p)/2;
  log.det.sig <- log(det(sig))
  if(log.det.sig == "NaN") stop('Degenerate Solution Reached - The Determinate of this matrix is Zero');
  ty.t <- sweep(x,2,mu,"-");
  inv.sig <- solve(sig)
  t1.num <- sweep(ty.t%*%inv.sig%*%talpha,2,log(2),"+");
  t1.den <- (p/2)*log(2*pi)+0.5*log.det.sig;
  t1 <- sweep(t1.num,2,t1.den,"-");
  maha <- mahalanobis(x=x,center=mu,cov=inv.sig,inverted=TRUE); 
  ata <- 2 + alpha%*%inv.sig%*%talpha;
  l.t2.num <- log(maha)
  l.t2.den <- log(ata)
  l.t2.den <- rep(l.t2.den,n)
  t2 <- 0.5*nu*(l.t2.num-l.t2.den)
  u <- exp( 0.5*(l.t2.den + l.t2.num) )
  t3 <- log(besselK(u,nu,expon.scaled=TRUE)) - u
  val1 <- t1+t2+t3
  val <- exp(val1)
  return(c(val))
}
