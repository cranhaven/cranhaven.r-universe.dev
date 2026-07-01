circ.plugin.hdr<-function(sample,bw=bw.CV(circular(sample),upper=100),tau=NULL,tau.method="quantile",
                          level=NULL,conf=.95,plot.hdr=TRUE,plot.hdrconf=TRUE,boot=FALSE,k=3,
                          col=NULL,lty=NULL,shrink=NULL,lwd=NULL,pch=NULL,cex=NULL){
  if(!is.numeric(sample)|any(sample<0)|any(sample>(2*pi))){
    stop("argument 'sample' must be a numeric vector of angles from 0 to 2*pi")
  }else if((!is.numeric(bw))|(length(bw)>1)){
    stop("argument 'bw' is a bandwidth parameter that must take a positive value")
  }else{
    sample=circular(sample,type="angles",units="radians")
    fn <- kern.den.circ(sample, bw=bw,n=1000)
    fnx <- kern.den.circ(sample, z=sample, bw=bw)$y
    x=fn$x
    f=fn$y
    if((!is.null(level))&(!is.numeric(level)) ){
      stop("argument 'level' must be a numeric value")
    }else if((!is.null(level))&(is.numeric(level)) ){
      if((level>max(f))){
        warning("level set is equal to the emptyset","\n")
        return(list(hdr="emptyset",level=level,bw=bw))
      }else if((level<min(f))){
        warning("level set is equal to the support distribution","\n")
        return(list(hdr="unit circle",level=level,bw=bw))
      }
    }else{
      if((tau<1)&(tau>0)){
        if(tau.method=="quantile"){
          level=quantile(fnx,prob=(tau),type=1)
        }else if(tau.method=="trapezoidal"){
          step=x[2]-x[1]
          y<-seq(0,max(f),by=step)
          level=uniroot(g<-function(y){return(trapezoidal.rule(f,step,y)-(1-tau))},lower=0,upper=max(f))$root
        }else{
          stop("argument 'tau.method' must take the values quantile or trapezoidal.rule")

        }
      }else{
        stop("argument 'tau' is a probability that must take a value larger than 0 and smaller than 1")
      }
    }
  }


  hdr=find.circ.hdr(x,f,level)

  if((!is.null(tau))&(boot==FALSE)){
    if(!((conf<1)&(conf>0)))stop("argument 'conf' must be between 0 and 1")
    alpha <- 1 - conf
    nint <- length(hdr)
    delta <- range(as.numeric(sample))/1000
    f2 <- approx(x, f, hdr + delta)$y
    fprime <- (f2 - level)/delta
    g <- level * sum(abs(1/fprime))
    var.level <- tau* (1 - tau)/(length(sample) * g * g)
    z <- abs(qnorm(0.5 - conf/2))
    level.ci <- z * sqrt(var.level)
    level.ci <- level + c(-level.ci, level.ci)
    hdr1=find.circ.hdr(x,f,level.ci[1])
    hdr2=find.circ.hdr(x,f,level.ci[2])

  }

  if(!is.logical(plot.hdr)){
    stop("argument 'plot.hdr' must be logical")
  }else{
    if(plot.hdr){
      if(is.null(col)){col="darkgray"}
      if(is.null(shrink)){shrink=2}
      if(is.null(lty)){lty=2}
      if(is.null(lwd)){lwd=2}
      if(is.null(pch)){pch=19}
      if(is.null(cex)){cex=.5}
      plot.circular(circular(seq(0,2*pi,length=100),type="angles",units="radians"),shrink=shrink,type="l")
      lines.circular(x, f,shrink=shrink,col=1)
      lines.circular(x,rep(level,times=length(x)),col=col,lty=lty,shrink=shrink)
      points.circular(x[(f>=level)],col=col,shrink=shrink,pch=pch,cex=cex)
      if(is.null(tau)){plot.hdrconf=FALSE}
      if(plot.hdrconf){
        if(!all(is.na(hdr1))){
          x.1=sort(as.numeric(x[(f>=level.ci[1])]))
          d.x.1=abs(x.1[-1]-x.1[-length(x.1)])
          b.x.1=which(round(d.x.1,3)!=round(min(d.x.1),3))
          if(length(b.x.1)!=0){
            i.x.1=sort(union(c(1,length(x.1)),b.x.1))
            for(i in 1:(length(i.x.1)-1)){
              lines.circular(circular(x.1[(i.x.1[i]+1):i.x.1[i+1]]), rep(-level.ci[1]/k,times=length(x.1[(i.x.1[i]+1):i.x.1[i+1]])),shrink=shrink,col="darkred",lwd=lwd)
            }
          }else{
            lines.circular(circular(x.1), rep(-level.ci[1]/k,times=length(x.1)),shrink=shrink,col="darkred",lwd=lwd)
          }
        }
        if(!all(is.na(hdr2))){
          x.2=sort(as.numeric(x[(f>=level.ci[2])]))
          d.x.2=abs(x.2[-1]-x.2[-length(x.2)])
          b.x.2=which(round(d.x.2,3)!=round(min(d.x.2),3))
          if(length(b.x.2)!=0){
            i.x.2=sort(union(c(1,length(x.2)),b.x.2))
            for(i in 1:(length(i.x.2)-1)){
              lines.circular(circular(x.2[(i.x.2[i]+1):i.x.2[i+1]]), rep(level.ci[1]/k,times=length(x.2[(i.x.2[i]+1):i.x.2[i+1]])),shrink=shrink,col="darkred",lwd=lwd)
            }
          }else{
            lines.circular(circular(x.2), rep(level.ci[1]/k,times=length(x.2)),shrink=shrink,col="darkred",lwd=lwd)
          }

        }
      }

    }
  }
  if((!is.null(tau))&(!boot)){
    return(list(hdr=matrix(hdr,ncol=2,byrow=TRUE),prob.content=(1-tau),level=level,bw=bw,hdr.lo=matrix(hdr1,ncol=2,byrow=TRUE),level.lo=level.ci[1],hdr.hi=matrix(hdr2,ncol=2,byrow=TRUE),level.hi=level.ci[2]))
  }else{
        return(list(levelset=matrix(hdr,ncol=2,byrow=TRUE),prop.content=(sum(fnx>=level)/length(sample)),level=level,bw=bw))
  }
}
