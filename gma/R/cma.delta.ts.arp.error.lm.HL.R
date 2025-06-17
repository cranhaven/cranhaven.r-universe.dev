cma.delta.ts.arp.error.lm.HL <-
function(dat,delta=0,p=1,max.itr=500,tol=1e-4,error.indep=FALSE,error.var.equal=FALSE,Sigma.update=FALSE,
                                       var.constraint=FALSE)
{
  re<-cma.delta.ts.arp.error.lm(dat,delta,p=p,max.itr,tol,error.indep,error.var.equal,Sigma.update,var.constraint)
  return(re$HL)
}
