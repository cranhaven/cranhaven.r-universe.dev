plot.cint <- function(x,cints=TRUE,pints=TRUE,rugged=TRUE,
                       type=c("both","upper","lower"),...) {
#
# Function plot.cint().  To plot confidence prediction intervals
# for a (Gaussian) scalar mixture model. The argument "x" is an object
# of class "cint" as returned by the function cint().
#
type   <- match.arg(type)
if(!(cints | pints))
    stop("At least one of \"cints\" or \"pints\" must be TRUE.\n")

bnds   <- x$bnds
theta  <- x$theta
alpha  <- x$alpha
nmy    <- x$nmy
K      <- length(theta)
ylim   <- if(rugged) {
              range(c(x[["y"]],unlist(bnds)),na.rm=TRUE)
          } else {
              range(unlist(bnds),na.rm=TRUE)
          }

ic <- if(type=="both") 1:2 else 1
ip <- if(type=="both") 3:4 else 2
main <- list(...)$main
if(is.null(main)) {
    pctype <- cints + 2*pints
    switch(EXPR=pctype,
        {tight <- if(type=="both")
		     paste("Confidence intervals, level = ",
                            100*(1-alpha),"%.",sep="")
	else if(type=="upper")
		paste("Upper confidence intervals, level = ",
                             100*(1-alpha),"%.",sep="")
	else
		paste("Lower confidence intervals, level = ",
                             100*(1-alpha),"%.",sep="")},
        {tight <- if(type=="both")
		paste("Prediction intervals, level = ",
                               100*(1-alpha),"%.",sep="")
	else if(type=="upper")
		paste("Upper prediction intervals, level = ",
                              100*(1-alpha),"%.",sep="")
	else
		paste("Lower prediction intervals, level = ",
                              100*(1-alpha),"%.",sep="")},
        {tight <- if(type=="both")
		paste("Confidence and Prediction intervals, level = ",
                               100*(1-alpha),"%.",sep="")
	else if(type=="upper")
		paste("Upper confidence and prediction intervals, level = ",
                       100*(1-alpha),"%.",sep="")
	else
		paste("Lower confidence and prediction intervals, level = ",
                       100*(1-alpha),"%.",sep="")}
    )
} else {
    tight <- main
}
xlab <- list(...)$xlab
if(is.null(xlab)) xlab <- "Component"
ylab <- list(...)$ylab
if(is.null(ylab)) ylab <- nmy
cols <- list(...)$col
if(is.null(cols)) cols <- 1:K
pch <- list(...)$pch
if(is.null(pch)) pch <- 1
xlim <- c(-0.5,K+1.5)
hdl  <- 0.075

plot(0,0,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,axes=FALSE)
axis(side=2)
axis(side=1,at=1:K)
box()
ic   <- switch(EXPR=type,both=3:4,lower=c(1,9),upper=c(9,2))
ip   <- switch(EXPR=type,both=7:8,lower=c(5,9),upper=c(9,6))
kode <- switch(EXPR=type,both=3,lower=1,upper=2)
wtp  <- cints + 2*pints
for(k in 1:K){
    mn   <- theta[[k]][["beta"]]
    ends <- c(bnds[[k]],mn)
    switch(EXPR=wtp,
        {points(k,mn,pch=pch,col=cols[k])
         aa <- c(k,ends[ic][1],k,ends[ic][2])
         arrows(aa[1],aa[2],aa[3],aa[4],
                code=kode,angle=90,col=cols[k],length=hdl)
         },
        {points(k,mn,pch=pch,col=cols[k])
         aa <- c(k,ends[ip][1],k,ends[ip][2])
         arrows(aa[1],aa[2],aa[3],aa[4],
                code=kode,angle=90,col=cols[k],length=hdl)
         },
        {points(k-0.25,mn,pch=pch,col=cols[k])
         aa <- c(k-0.25,ends[ic][1],k-0.25,ends[ic][2])
         arrows(aa[1],aa[2],aa[3],aa[4],
                code=kode,angle=90,col=cols[k],length=hdl)
         points(k+0.25,mn,pch=pch,col=cols[k])
         aa <- c(k+0.25,ends[ip][1],k+0.25,ends[ip][2])
         arrows(aa[1],aa[2],aa[3],aa[4],
                code=kode,angle=90,col=cols[k],length=hdl)
         })
}
title(main=tight)
if(rugged) rug(x[["y"]],side=4)
invisible()
}
