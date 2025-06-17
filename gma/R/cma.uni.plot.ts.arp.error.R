cma.uni.plot.ts.arp.error <-
function(re.cma.sens,re.cma=NULL,delta=NULL,legend.pos="topright",
                                    xlab=expression(delta),ylab=expression(hat(AB)),
                                    cex.lab=1,cex.axis=1,lgd.cex=1,lgd.pt.cex=1,plot.delta0=TRUE,...)
{
  dt<-re.cma.sens$coefficients[,"delta"]
  AB.p<-re.cma.sens$coefficients[,"AB.p.Estimate"]
  AB.p.ub<-re.cma.sens$coefficients[,"AB.p.UB"]
  AB.p.lb<-re.cma.sens$coefficients[,"AB.p.LB"]
  idx<-sort(AB.p,index.return=TRUE)$ix
  
  #################################################
  # plot delta = 0 result or not
  if(!is.null(re.cma))
  {
    if(re.cma$delta==0)
    {
      plot.delta0<-FALSE
    }
  }
  if(!is.null(delta))
  {
    if(length(which(delta==0))>0)
    {
      plot.delta0<-FALSE
    }
  }
  #################################################
  
  plot(range(dt[idx]),range(c(AB.p.lb,AB.p.ub)),type="n",xlab=xlab,ylab=ylab,cex.lab=cex.lab,cex.axis=cex.axis)
  polygon(c(rev(dt),dt),c(rev(AB.p.ub),AB.p.lb),col="grey80",border=NA)
  abline(v=0)
  abline(h=0)
  if(plot.delta0)
  {
    abline(h=AB.p[which(dt==0)],lty=2,col=2) 
  }
  lines(dt,AB.p,lwd=2)
  lines(dt,AB.p.lb,lty=2,lwd=1,col=8)
  lines(dt,AB.p.ub,lty=2,lwd=1,col=8)
  
  if(!is.null(re.cma))
  {
    points(re.cma$delta,re.cma$Coefficients[5,1],pch=16,col=4,cex=0.75)
    lines(rep(re.cma$delta,2),re.cma$Coefficients[5,c(3,4)],lty=2,col=4)
    lines(c(re.cma$delta-0.02,re.cma$delta+0.02),rep(re.cma$Coefficients[5,3],2),col=4)
    lines(c(re.cma$delta-0.02,re.cma$delta+0.02),rep(re.cma$Coefficients[5,4],2),col=4)
    
    if(plot.delta0)
    {
      legend(legend.pos,legend=c(expression(delta==0),
                                 substitute(delta==d,list(d=round(re.cma$delta,digits=3)))),
             lty=2,col=c(2,4),pch=c(NA,16),pt.cex=lgd.pt.cex,bty="n",cex=lgd.cex) 
    }else
    {
      legend(legend.pos,legend=substitute(delta==d,list(d=round(re.cma$delta,digits=3))),
             lty=2,col=4,pch=16,pt.cex=lgd.pt.cex,bty="n",cex=lgd.cex)
    }
  }else
    if(!is.null(delta))
    {
      for(j in 1:length(delta))
      {
        idx.tmp<-which.min(abs(dt-delta[j]))
        
        points(dt[idx.tmp],AB.p[idx.tmp],pch=16,col=4,cex=0.75)
        lines(rep(dt[idx.tmp],2),c(AB.p.lb[idx.tmp],AB.p.ub[idx.tmp]),lty=2,col=4)
        lines(c(dt[idx.tmp]-0.02,dt[idx.tmp]+0.02),rep(AB.p.lb[idx.tmp],2),col=4)
        lines(c(dt[idx.tmp]-0.02,dt[idx.tmp]+0.02),rep(AB.p.ub[idx.tmp],2),col=4)
      }
      
      if(plot.delta0)
      {
        legend(legend.pos,legend=expression(delta==0),lty=2,col=2,bty="n",cex=lgd.cex) 
      }
    }
}
