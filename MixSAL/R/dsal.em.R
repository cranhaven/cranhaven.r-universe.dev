dsal.em <-
function(x,prmtrs,g,maha=NULL,ata=NULL){
  alpha <- prmtrs$alpha[g,]; talpha <- prmtrs$talpha[,g]; log.det.sig <- prmtrs$log.det.sig[g]; inv.sig <- prmtrs$inv.sig[,,g]; mu <- prmtrs$mu[g,]; 
  p <- prmtrs$p; nu <- prmtrs$nu; n <- prmtrs$n
  if(log.det.sig == "NaN") stop('Degenerate Solution Reached - The Determinate of this matrix is Zero');
  ty.t <- sweep(x,2,mu,"-");
  t1.num <- sweep(ty.t%*%inv.sig%*%talpha,2,log(2),"+");
  t1.den <- (p/2)*log(2*pi)+0.5*log.det.sig;
  t1 <- sweep(t1.num,2,t1.den,"-");
  l.t2.num <- log(maha)
  l.t2.den <- log(ata)
  t2 <- 0.5*nu*(l.t2.num-l.t2.den)
  u <- exp( 0.5*(l.t2.den + l.t2.num) )
  t3 <- log(besselK(u,nu,expon.scaled=TRUE)) - u
  val1 <- t1+t2+t3
  val <- exp(val1)
  return(val)
}
