##
##  scaleboot: R package for multiscale bootstrap
##  Copyright (C) 2006-2007 Hidetoshi Shimodaira
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
######################################################################
### MAIN: PLOT

##
## mb : output of sbfit
## xval : x-axis variable
## yval : y-axis variable
## xlog : plot in log for x-axis
## ylog : plot in log for y-axis
##

plot.scaleboot <- function(x,
                       models=NULL,
                       select=NULL,
                       sort.by=c("aic","none"),
                       k=NULL,s=NULL,sp=NULL,lambda=NULL,
                       bpk=NULL,
                       ## parameters for x-y axises
                       xval=c("square","inverse","sigma"),
                       yval=c("psi","zvalue","pvalue"),
                       xlab=NULL,ylab=NULL,
                       log.xy="",
                       xlim=NULL,ylim=NULL,
                       add=F,
                       length.x=300,
                       main=NULL,
                       ## lines
                       col=1:6,lty=1:5,lwd=par("lwd"),
                       ## extrapolation
                       ex.pch=2:7,
                       ## points
                       pch=1,cex=1,pt.col=col[1],pt.lwd=lwd[1],
                       ## legend
                       legend.x=NULL, inset=0.1, cex.legend=1,
                       ...
                       ) {

  ## option
  op <- sboptions()
  ## check log
  if(length(log.xy) && log.xy != "") a <- strsplit(log.xy,"")[[1]] else a <- ""
  xlog <- "x" %in% a
  ylog <- "y" %in% a

  ## calculate akaike weights
  aic <- sapply(x$fi,"[[","aic")

  ## specify models
  if(is.null(models)) models <- names(x$fi)
  else if(is.character(models)) {
    a <- match(models,names(x$fi))
    a <- a[!is.na(a)]
    if(length(a)>0) models <- names(x$fi)[a] else models <- character(0)
  }
  sort.by <- match.arg(sort.by)
  if(sort.by == "aic") {
    if(is.numeric(models)) models <- names(x$fi)[order(aic)[models]]
    else models <- models[order(aic[models])]
  } else {
    if(is.numeric(models)) models <- names(x$fi)[models]
  }

  ## specify select for extrapolation
  if(!is.null(k) && length(models)>0) {
    a <- aic[models] - min(aic[models])
    aicw <- exp(-a/2) # akaike weights
    aicw <- aicw/sum(aicw)

    select <- match.arg(select,c("best","average",models))
    if(select=="best") models2 <- models[order(aic[models])[1]]
    else if(select=="average") models2 <- models[aicw>op$th.aicw]
    else models2 <- select
    models <- models2
    aicw <- aicw[models]/sum(aicw[models])
  }
  
  ## check if models are valid
  i <- match(models,names(x$fi))
  models <- names(x$fi)[i[!is.na(i)]]
  
  ## x-axis
  ## s : sigma^2
  ## xfun(s) : x-axis function
  ## xinv(x) : s (inverse of xfun)
  xval <- match.arg(xval)
  if(xval=="sigma") {
    xlab0 <- expression(sigma)
    xfun <- function(s) s^0.5
    xinv <- function(x) x^2
  } else if(xval=="inverse") {
    xlab0 <- expression(1/sigma)
    xfun <- function(s) s^(-0.5)
    xinv <- function(x) x^(-2)
  } else if(xval=="square") {
    xlab0 <- expression(sigma^2)
    xfun <- function(s) s
    xinv <- function(x) x
  } else stop("xval is ",xval)
  if(is.null(xlab)) xlab <- xlab0

  ## y-axis
  ## p : psi-value
  ## yfun(p,s) : conversion p -> y
  yval <- match.arg(yval)
  if(!is.null(bpk)) { #for 2-step bootstrap probabilities
    if(bpk>=2) x$bp <- x$bpm[bpk-1,] # alter the bp values
    ylab0 <- paste("Bootstrap Probability : k =",bpk)
    yfun <- function(p,s) pnorm(-p/sqrt(s))
    k <- NULL
  } else if(yval=="zvalue") {
    ylab0 <- expression(z(sigma^2))
    yfun <- function(p,s) p/sqrt(s)
  } else if(yval=="pvalue") {
    ylab0 <- expression(alpha(sigma^2))
    yfun <- function(p,s) pnorm(-p/sqrt(s))
  } else if(yval=="psi") {
    ylab0 <- expression(psi(sigma^2))
    yfun <- function(p,s) p
  } else stop("yval is ",yval)
  if(is.null(ylab)) ylab <- ylab0

  ## observed points
  sa <- x$sa # sigma squared
  bp <- x$bp # bootstrap probabilities
  ss <- sqrt(sa) # scales
  bs <- -qnorm2(bp)*ss # observed psi-value
  by <- yfun(bs,sa) # observed y-axis
  bx <- xfun(sa) # observed x-axis
  u <- is.finite(if(ylog) log(by) else by)
  by.u <- by[u]; bx.u <- bx[u]

  ## extrapolation
  if(!is.null(k)) {
    if(is.null(main)) main <- c(paste("extrapolation k=",
                                      paste(k,collapse=","),sep=""),
                                paste(models,collapse="+"))
  } else {
    if(is.null(main)) main <- c("model fitting",
                                paste(models,collapse=","))
  }
  if(!is.null(k) && length(models)>0) {
    zex <- matrix(0,length(k),length(models))
    for(i in seq(along=models)) {
      f <- x$fi[[models[i]]]
      beta <- f$par*f$mag      
      psi <- sbpsiget(f$model)      
      for(j in seq(along=k)) 
        zex[j,i] <- psi(beta,s=s,k=k[j],sp=sp,lambda=lambda)
    }
    if(length(models)>1) {
      py <- yfun(wsumzval(zex,aicw),1)
    } else {
      py <- yfun(drop(zex),1)
    }
    px <- rep(xfun(sp),length(py))
  } else {
    px <- NULL; py <- NULL
  }

  ## xlim and ylim
  if(is.null(xlim)) {
    xlim <- if(length(bx.u)>0) range(bx.u,px) else range(bx,px)
  }
  if(is.null(ylim)) {
    py.u <- py[is.finite(py)]
    ylim <- if(length(bx.u)>0) range(by.u,py.u) else c(-1,1)
  }

  ## plot
  if(add) {
    points(bx.u,by.u,pch=pch,cex=cex,col=pt.col,lwd=pt.lwd,...)
  }
  else {
    plot(bx.u,by.u,
         xlab=xlab,ylab=ylab,log=log.xy,xlim=xlim,ylim=ylim,
         pch=pch,cex=cex,col=pt.col,lwd=pt.lwd,main=main,...)
  }
  if(!is.null(k))
    points(c(NA,px),c(NA,py),pch=ex.pch,cex=cex,col=col,lwd=pt.lwd,...)

  ## return value (passed to lines)
  z1 <- list(sa=sa,bp=bp,bx=bx,by=by,use=u,
             px=px,py=py,
             xlim=xlim,ylim=ylim,xlog=xlog,ylog=ylog,
             xfun=xfun,xinv=xinv,yfun=yfun,xlab=xlab,ylab=ylab,
             length.x=length.x,col=col,lty=lty,lwd=lwd)

  ## lines
  z2 <- lines(x,z1,models=models,k=k,s=s,sp=sp,lambda=lambda,bpk=bpk)


  ## legend
  if(!is.null(legend.x)) sblegend(legend.x,z=c(z1,z2),inset=inset,cex=cex.legend)

  invisible(c(z1,z2))
}

plot.summary.scaleboot <-
  function(x,select="average",
           k=x$parex$k,s=x$parex$s,sp=x$parex$sp,lambda=x$parex$lambda,
           ...)
  plot.scaleboot(x,select=select,k=k,s=s,sp=sp,lambda=lambda,...)


##
## mb : output of sbfit
## z : output of plot.sbfit
##
lines.scaleboot <- function(x,z,
                        models=names(x$fi),
                        k=NULL,s=NULL,sp=NULL,lambda=NULL,
                        bpk=NULL,
                        length.x=z$length.x,
                        col=z$col,lty=z$lty,lwd=z$lwd,...
                        ) {
  if(is.null(models)||length(models)==0) return(invisible(NULL))
  
  ## sequence in x-axis
  rx <- z$xlim
  if(z$xlog) rx <- log(rx)
  a <- (rx[2]-rx[1])*0.05
  if(z$xlog || !is.null(k)) rx[1] <- rx[1]-a
  else rx[1] <- if(rx[1]>a*3) rx[1] - a else rx[1]*(1-1/3)
  rx[2] <- rx[2]+a
  xx <- seq(from=rx[1],to=rx[2],length=length.x)
  if(z$xlog) xx <- exp(xx)
  sa <- z$xinv(xx)
  u1 <- sa >= 0  # for psi function
  u2 <- sa <= s  # for extraplolation
  if(!is.null(bpk)) { ## tentative...
    v1 <- which(u1)
    sam <- sa
    for(i in seq(along=sa)) {
      ## find x$sa[j] closest to sa[i]
      a <- Inf
      for(j in seq(along=x$sa)) {
        b <- abs(sa[i] - x$sa[j])
        if(b<a) { a <- b; j0 <- j}
      }
      ## use the x$sam value at this j
      sam[i] <- x$sam[j0]
    }
  }
  
  if(is.null(k)) {### fitting models (without extrapolations)
    yy <- matrix(0,length(xx),length(models))
    yy[] <- NA
    for(i in seq(along=models)) {
      f <- x$fi[[models[i]]]
      if(!is.null(f)) {
        beta <- f$par*f$mag
        if(is.null(bpk)) {
          psi <- sbpsiget(f$model)
          py <- sapply(sa[u1],function(s) psi(beta,s))
          yy[u1,i] <- z$yfun(py,sa)
        } else {
          prb <- sbprbget(f$model)
          for(j in seq(along=v1))
            yy[v1[j],i] <-  prb(beta,sa[v1[j]],sam[v1[j]])[bpk]
        }
      }
    }
    labels <- models
    
  } else { ### fitting models and extrapolation
    ## first, prepare the containers
    yy <- matrix(NA,length(xx),length(k)+1) # for matlines
    yy1 <- matrix(NA,sum(u1),length(models)) # for psi
    yy2 <- array(NA,dim=c(sum(u2),length(k),length(models))) # for ext
    ## compute psi values for models
    for(i in seq(along=models)) {
      f <- x$fi[[models[i]]]
      beta <- f$par*f$mag
      psi <- sbpsiget(f$model)
      yy1[,i] <- sapply(sa[u1],function(s0) psi(beta,s0))
      for(j in seq(along=k)) {
        yy2[,j,i] <- sapply(sa[u2],function(s1)
                            psi(beta,s=s,k=k[j],sp=s1,lambda=lambda))
      }
    }
    if(length(models)>1) { ## then, average models...
      aic <- sapply(x$fi[models],"[[","aic")
      w <- exp(-(aic-min(aic))/2) # akaike weights
      w <- w/sum(w)
      ## averaged fitting model
      y1 <- wsumzval(yy1/sqrt(sa[u1]),w)*sqrt(sa[u1])
      yy[u1,1] <- z$yfun(y1,sa[u1])
      ## averaged extrapolation
      y2 <- wsumzval(yy2,w)
      yy[u2,-1] <- z$yfun(y2,1)
    } else { ## if only one model...
      yy[u1,1] <- z$yfun(yy1,sa[u1]) # fitting
      yy[u2,-1] <- z$yfun(yy2,1) # extrapolation
    }
    labels <- c("fitting", paste("k",k,sep="."))
  }

  if(!all(is.na(yy))) matlines(xx,yy,col=col,lty=lty,lwd=lwd)
  invisible(list(col=col,lty=lty,lwd=lwd,labels=labels,do.lines=T))
}

##
## x,y : legend position
## z : output of plot.sbfit or plot.sbconf
##

sblegend <- function(x="topright",y=NULL,z,inset=0.1,...) {
  if(length(z)==1) z <- z[[1]]
  if(is.null(z$labels)) return(invisible(NULL))
  if(is.null(do.lines <- z$do.lines)) do.lines <- F 
  if(is.null(do.points <- z$do.points)) do.points <- F
  if(do.lines) lty <- rep(z$lty,length=length(z$labels))
  if(do.points) pch <- rep(z$pch,length=length(z$labels))
  if(do.lines && ! do.points) 
    legend(x,y,z$labels,col=z$col,lwd=z$lwd,lty=lty,inset=inset,...)
  else if(!do.lines &&  do.points) 
    legend(x,y,z$labels,col=z$col,pch=pch,inset=inset,...)
  else if(do.lines &&  do.points) 
    legend(x,y,z$labels,col=z$col,lwd=z$lwd,lty=lty,pch=pch,inset=inset,...)
  else invisible(NULL)
}

##
## scalebootv
##

plot.scalebootv <- function(x,models=attr(x,"models"),sort.by="none",...) {
  ## preliminary
  n <- length(x)
  m <- n2mfrow(n)
  s <- matrix(c(1:n,rep(0,m[1]*m[2]-n)),m[1],m[2],byrow=T)
  def.par <- par(no.readonly = TRUE) # save default
  on.exit(par(def.par))
  layout(s)

  # plots
  z <- NULL
  for(i in seq(length=n)) {
    z[[i]] <- plot(x[[i]],main=names(x)[[i]],models=models,sort.by=sort.by,...)
  }
  invisible(z)
}

plot.summary.scalebootv <- function(x, select="average",...)
  plot.scalebootv(x,select=select,...)


##
## plot (beta0,beta1)
##
##  x is M times 2 matrix  with beta[,1] is beta0 and  beta[,2] is beta1
##
sbplotbeta <- function(beta, p=0.05, col.contour=c("blue","red","green"),
                     drawcontours = TRUE, drawlabels = TRUE,
                     labcex=1,length=100, cex=1, col="black",
                     xlim=NULL, ylim=NULL, lim.countourexpand=0) {
  
  
  beta0 <- beta[,1]; beta1 <-  beta[,2]; na <- rownames(beta);
  if(is.null(xlim)) xlim <- range(c(0,beta0),na.rm=TRUE)
  if(is.null(ylim)) ylim <- range(c(0,beta1),na.rm=TRUE)
  plot(0,0,xlim=xlim,ylim=ylim,type="n",xlab="beta0", ylab="beta1")
  abline(h=0, lty=2)
  abline(v=0, lty=2)  
  
  if(drawcontours) {
    calc_bp <- function(v,c) pnorm(-v-c)
    calc_au <- function(v,c) pnorm(-v+c)
    calc_sia <- function(v,c) 1 - pnorm(v-c)/pnorm(-c)  # region=alternative
    calc_sin <- function(v,c) pnorm(-v+c)/pnorm(c)  # region=null
    calc_all <- function(v,c) {
      ans <- c(calc_bp(v,c),calc_au(v,c),calc_sia(v,c), calc_sin(v,c))
      names(ans) <- c("bp","au","sia","sin")
      ans
    }
    expandlim <- function(lim) {
      add <- (lim[2]-lim[1])*lim.countourexpand
      lim[1] <- lim[1] - add
      lim[2] <- lim[2] + add
      lim
    }
    xlim <- expandlim(xlim)
    ylim <- expandlim(ylim)
    vv <- seq(xlim[1],xlim[2],length=length)
    cc <- seq(ylim[1],ylim[2],length=length)
    vc <- as.matrix(expand.grid(vv,cc))
    pp <- apply(vc,1,function(x) calc_all(x[1],x[2]))
    vz =  matrix(apply(vc,1,function(x) x[1]),length(vv),length(cc))
    cz =  matrix(apply(vc,1,function(x) x[2]),length(vv),length(cc))
    bpz = matrix(pp["bp",],length(vv),length(cc))
    auz = matrix(pp["au",],length(vv),length(cc))
    siaz = matrix(pp["sia",],length(vv),length(cc))
    sinz = matrix(pp["sin",],length(vv),length(cc)) 
    
    contour(vv,cc,bpz, levels = p, drawlabels=drawlabels,
            labels=paste("bp =",p), labcex=labcex,col=col.contour[3],add=T)
    contour(vv,cc,auz, levels = p, drawlabels=drawlabels,
            labels=paste("au =",p), labcex=labcex,col=col.contour[2],add=T)
    contour(vv,cc,sinz, levels = p, drawlabels=drawlabels,
            labels=paste("si =",p), labcex=labcex,col=col.contour[1],add=T)
    contour(vv,cc,bpz, levels = 1-p, drawlabels=drawlabels,
            labels=paste("bp =",1-p), labcex=labcex,col=col.contour[3],add=T)
    contour(vv,cc,auz, levels = 1-p, drawlabels=drawlabels,
            labels=paste("au =",1-p), labcex=labcex,col=col.contour[2],add=T)
    contour(vv,cc,siaz, levels = 1-p, drawlabels=drawlabels,
            labels=paste("si =",1-p), labcex=labcex,col=col.contour[1],add=T)
  }
  
  text(beta0, beta1, na, cex=cex, col=col)
  invisible(list(beta0=vv,beta1=cc,pvalue=pp))
}
