gma <-
function(dat,model.type=c("single","twolevel"),method=c("HL","TS","HL-TS"),delta=NULL,p=1,
              single.var.asmp=TRUE,sens.plot=FALSE,sens.delta=seq(-1,1,by=0.01),legend.pos="topright",
              xlab=expression(delta),ylab=expression(hat(AB)),
              cex.lab=1,cex.axis=1,lgd.cex=1,lgd.pt.cex=1,plot.delta0=TRUE,
              interval=c(-0.9,0.9),tol=1e-4,max.itr=500,conf.level=0.95,error.indep=TRUE,error.var.equal=FALSE,
              Sigma.update=TRUE,var.constraint=TRUE,...)
{
  if(model.type[1]=="single")
  {
    if(is.null(delta)==TRUE)
    {
      delta<-0
    }
    run.time<-system.time(re1<-cma.uni.delta.ts.arp.error(dat,delta=delta,p=p,conf.level=conf.level,var.asmp=single.var.asmp))
    
    single.ll<-cma.uni.ts.arp.error.ll(dat,Theta=re1$D,W=re1$W,Sigma=re1$Sigma,p=p)
    
    re<-re1
    re$LL<-single.ll
    
    # sensitivity plot
    if(sens.plot==TRUE)
    {
      re.cma.sens<-cma.uni.sens.ts.arp.error(dat,delta=sens.delta,p=p,conf.level=conf.level)
      cma.uni.plot.ts.arp.error(re.cma.sens,re.cma=re1,delta=delta,legend.pos=legend.pos,
                                xlab=xlab,ylab=ylab,cex.lab=cex.lab,cex.axis=cex.axis,
                                lgd.cex=lgd.cex,lgd.pt.cex=lgd.pt.cex,plot.delta0=plot.delta0)
    }
  }else
    if(model.type[1]=="twolevel")
    {
      if(is.null(delta)==TRUE)
      {
        if(method[1]=="TS")
        {
          t1<-system.time(re1<-optimize(cma.delta.ts.arp.error.lm.HL,interval=interval,dat=dat,p=p,max.itr=0,tol=tol,
                                        error.indep=error.indep,error.var.equal=error.var.equal,Sigma.update=Sigma.update,
                                        var.constraint=var.constraint,maximum=TRUE))
          t2<-system.time(re<-cma.delta.ts.arp.error.lm(dat,delta=re1$maximum,p=p,max.itr=0,tol=tol,error.indep=error.indep,
                                                        error.var.equal=error.var.equal,Sigma.update=Sigma.update,
                                                        var.constraint=var.constraint))
          
          run.time<-t1+t2
        }else
        {
          t1<-system.time(re1<-optimize(cma.delta.ts.arp.error.lm.HL,interval=interval,dat=dat,p=p,max.itr=max.itr,tol=tol,
                                        error.indep=error.indep,error.var.equal=error.var.equal,Sigma.update=Sigma.update,
                                        var.constraint=var.constraint,maximum=TRUE))
          if(method[1]=="HL")
          {
            t2<-system.time(re<-cma.delta.ts.arp.error.lm(dat,delta=re1$maximum,p=p,max.itr=max.itr,tol=tol,error.indep=error.indep,
                                                          error.var.equal=error.var.equal,Sigma.update=Sigma.update,
                                                          var.constraint=var.constraint))
          }
          if(method[1]=="HL-TS")
          {
            t2<-system.time(re<-cma.delta.ts.arp.error.lm(dat,delta=re1$maximum,p=p,max.itr=0,tol=tol,error.indep=error.indep,
                                                          error.var.equal=error.var.equal,Sigma.update=Sigma.update,
                                                          var.constraint=var.constraint))
          }
          
          run.time<-t1+t2
        }
      }else
      {
        if(method[1]=="TS")
        {
          run.time<-system.time(re<-cma.delta.ts.arp.error.lm(dat,delta=delta,p=p,max.itr=0,tol=tol,error.indep=error.indep,
                                                              error.var.equal=error.var.equal,Sigma.update=Sigma.update,
                                                              var.constraint=var.constraint))
        }
        if(method[1]=="HL")
        {
          run.time<-system.time(re<-cma.delta.ts.arp.error.lm(dat,delta=delta,p=p,max.itr=max.itr,tol=tol,error.indep=error.indep,
                                                              error.var.equal=error.var.equal,Sigma.update=Sigma.update,
                                                              var.constraint=var.constraint))
        }
      }
    }
  
  re$time<-run.time
  
  return(re)
}
