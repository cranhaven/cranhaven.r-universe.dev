##
##  scaleboot: R package for multiscale bootstrap
##  Copyright (C) 2006 Hidetoshi Shimodaira
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
### MAIN: CONFIDENCE INTERVALS
### --- experimental code ---

## general
sbconf <- function(x,...) UseMethod("sbconf")

### default
##
## x : list of vectors of replicates (or matrix)
## sa : vector of sigma^2's
## probs : vector of p-values for computing quantiles
## model : model name
## k,s,sp : p-value parameter
## nocv: whether computing cv or not
## cluster: for parallel computing (snow package)
##

sbconf.default <- function(x,sa,
                           probs=c(0.05,0.95),
                           model="poly.2",
                           k=2,s=1,sp=-1, # p-value parameters
                           cluster=NULL,...
                           ) {
  op <- sboptions()
  
  ## convert to list if x is a matrix
  if(is.matrix(x)) {
    a <- vector("list",ncol(x))
    for(i in seq(along=a)) a[[i]] <- x[,i]
    x <- a
  } else if(is.list(x[[1]])) {
    if(length(x[[1]][[1]])!=1) stop("only scalar is allowed")
    for(i in seq(along=x)) x[[i]] <- simplist(x[[i]])
  }
  
  ## count nb
  nb <- sapply(x,length)

  ## compute ts (= initial grid of threshoulds)
  a <- sapply(x,function(s) quantile(s,probs=op$probs0))
  ts <- apply(a,1,median)

  ## compute ts0 (= a starting value)
  a <- sapply(x,function(s) quantile(s,probs=op$prob0))
  ts0 <- median(a)

  ## compute bps (= matrix of bps for thereshoulds)
  bps <- sapply(x,function(s) sapply(ts,function(t)
                                      sum(s<=t))/length(s))
  ## fitting
  fits <- sbfit(bps,nb,sa,models=op$models,cluster=cluster)

  ##
  z <- list(stat=x,nb=nb,sa=sa,ts=ts,ts0=ts0,fits=fits,
            probs=probs,model=model,k=k,s=s,sp=sp)
  class(z) <- "sbconf"

  sbconf(z)
}

### sbconf
sbconf.sbconf <- function(x,
                          probs=x$probs,
                          model=x$model,k=x$k,s=x$s,sp=x$sp,
                          nofit=FALSE, # whether call sbfit or not
                          ...
                          )  {

  ## print only if debug
  op <- sboptions()
  sbconfcat <- function(...) {
    if(op$debug) cat(...)
  }

  ## save
  x$probs <- probs
  x$model <- model
  x$k <- k; x$s <- s; x$sp <-sp

  ## extract information
  y <- summary(x$fits,k=k,s=s,sp=sp)
  pv <- sapply(y,function(s) s$pv[model,1])
  pe <- sapply(y,function(s) s$pe[model,1])
  zv <- -qnorm(pv)
  ze <- pe/dnorm(zv)
  tol.mono <- op$tol.mono
  tol.conv <- op$tol.conv
  tol.relconv <- op$tol.relconv
  max.loop <- op$max.loop
  models <- attr(x$fits,"models")

  ## additional fitting if necessary
  if(!nofit) {
    for(i in seq(along=probs)) {
      z0 <- -qnorm(probs[i])
      for(j in seq(max.loop)) {
        a <- sbfindroot(x$ts0,z0,x$ts,zv,w=ze^(-2),tol=tol.mono)
        sbconfcat("p[",i,"]=",probs[i]," iter=",j,sep="")
        if(is.null(a)) break  # error (monotonicity or limit)
        sbconfcat(" t=",a$xv," e=",a$ye," r=",a$re,sep="")
        if(a$ye<tol.conv || a$re<tol.relconv) break # convergence
        ## add a new t-value
        t.new <- a$xv; x$ts <- c(x$ts,t.new)
        bp.new <- sapply(x$stat,function(s) sum(s<=t.new)/length(s))
        f <- sbfit(bp.new,x$nb,x$sa,models=models)
        x$fits[[length(x$fits)+1]] <- f
        b <- summary(f,k=k,s=s,sp=sp)
        pv.new <- b$pv[model,1]
        pe.new <- b$pe[model,1]
        zv.new <- -qnorm(pv.new); zv <- c(zv,zv.new)
        ze.new <- pe.new/dnorm(zv.new); ze <- c(ze,ze.new)
        sbconfcat(" pv=",pv.new,"\n",sep="")
      }
      sbconfcat("\n")
    }
  }

  ## compute confidence intervals
  cv <- rep(NA,length(probs))
  names(cv) <- probs
  for(i in seq(along=probs)) {
    z0 <- -qnorm(probs[i])
    a <- sbfindroot(x$ts0,z0,x$ts,zv,w=ze^(-2),tol=tol.mono)
    if(!is.null(a)) cv[[i]] <- a$xv
  }
  x$cv <- cv

  x
}


### print
print.sbconf <- function(x,...) {
  print(x$cv)
}

### plot
plot.sbconf <- function(x,model=x$model,k=x$k,s=x$s,sp=x$sp,
                        models = attr(x$fits,"models"),
                        log.xy = "",xlab="test statistic",ylab=NULL,
                        type.plot = c("p","l","b"),
                        yval=c("aic","zvalue","pvalue"),
                        sd=2,add=FALSE,
                        ## points and lines
                        col=1:6,pch=NULL,lty=1:5,lwd=par("lwd"),
                        ## marks
                        mk.col=col[1], mk.lwd=lwd[1], mk.lty=lty[1],
                        ...) {
  ## x-axis
  o <- order(x$ts)
  ts <- x$ts[o]

  ## models
  if(is.numeric(models)) models <- attr(x$fits,"models")[models]
  
  ## plot type
  type.plot <- match.arg(type.plot)
  do.points <- type.plot=="p" || type.plot=="b"
  do.lines <- type.plot=="l" || type.plot=="b"
  
  ## y-axis
  yval <- match.arg(yval)
  if(yval=="aic") {
    ## AIC
    if(is.null(ylab)) ylab <- "AIC"
    aic <- trmat(sapply(x$fits,function(s) sapply(s$fi,"[[","aic")[models]))
    if(is.null(pch)) pch <- c(paste(c(1:9, 0)), letters)
    if(add) matpoints(ts,aic[o,],col=col,pch=pch,lty=lty,lwd=lwd,type=type.plot,...)
    else matplot(ts,aic[o,],log=log.xy,xlab=xlab,ylab=ylab,col=col,pch=pch,lty=lty,lwd=lwd,type=type.plot,...)
    ans <- list(col=col,pch=pch,lwd=lwd,lty=lty,labels=models,do.lines=do.lines,do.points=do.points)
  } else {
    ## pvaule or zvalue
    b <- summary(x$fits,k=k,s=s,sp=sp)
    pv <- sapply(b,function(s) s$pv[model,1])
    pe <- sapply(b,function(s) s$pe[model,1])
    zv <- -qnorm(pv)
    ze <- pe/dnorm(zv)
    if(is.null(pch)) pch <- c("o","U","L")
    if(yval=="zvalue") {
      y <- zv; ye <- ze; ylab0 <- "corrected z-value"
    } else {
      y <- pv; ye <- pe; ylab0 <- "corrected p-value"
    }
    if(is.null(ylab)) ylab <- ylab0
    y <- y[o]; ye <- ye[o]
    if(sd>0) yy <- cbind(y,y+ye*sd,y-ye*sd)
    else yy <- y
    if(add) matpoints(ts,yy,col=col,pch=pch,lty=lty,lwd=lwd,type=type.plot,...)
    else matplot(ts,yy,log=log.xy,xlab=xlab,ylab=ylab,col=col,pch=pch,lty=lty,lwd=lwd,type=type.plot,...)
    ## marks
    for(i in seq(along=x$probs)) {
      tv <- x$cv[[i]]
      if(!is.na(tv)) {
        abline(v=tv,col=mk.col,lwd=mk.lwd,lty=mk.lty)
        if(yval=="zvalue") yv <- -qnorm(x$probs[[i]])
        else yv <- x$probs[[i]]
        abline(h=yv,col=mk.col,lwd=mk.lwd,lty=mk.lty)
      }
    }
    ## labels
    labs <- paste(model,":k.",k,sep="")
    if(sd>0) labs <- c(labs,sprintf("%+d * sd",sd),sprintf("%+d * sd",-sd))
    ans <- list(col=col,pch=pch,lwd=lwd,lty=lty,labels=labs,do.lines=do.lines,do.points=do.points)
  }
  invisible(ans)
}

