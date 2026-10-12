msal <-
function(x,G,start=1,max.it=10000,eps=1e-2,print.it=F,print.warn=F,print.prmtrs=F){
  x <- scale(x)
  init.prmtrs <- prmtrs.init(x=x,G=G,start=start); p <- init.prmtrs$p; n <- init.prmtrs$n;
  em <- EM(x=x,G=G,prmtrs=init.prmtrs,max.it=max.it,eps=eps,print.it=print.it,print.warn=print.warn,
           print.prmtrs=print.prmtrs);
  zig <- e.step(x=x,G=G,prmtrs=em$prmtrs,w=NULL)$zig.hat
  final.loglik <- em$loglik[length(em$loglik)]
  bic <- bic.calc(loglik=final.loglik,g=G,p=p,n=n)
  icl <- icl.calc(bic=bic,wt=zig)
  cluster <- apply(zig,2,function(z){
    val = c(1:length(z))[z==max(z)]
    return(val)})
  return(list(loglik=em$loglik,alpha=em$prmtrs$alpha,sig=em$prmtrs$sig,mu=em$prmtrs$mu,pi.g=em$prmtrs$pi.g,bic=bic,icl=icl,cluster=cluster))
}
