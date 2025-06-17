cma.delta.ts.arp.error.lm.ts.logLik <-
function(dat,delta=0,p=p,error.indep=FALSE,error.var.equal=FALSE)
{
  re<-cma.delta.ts.arp.error.lm.ts(dat,delta=delta,p=p,error.indep=error.indep,error.var.equal=error.var.equal)
  return(re$logLik.lm)
}
