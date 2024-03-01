plot.cband <- function(x,cbands=TRUE,pbands=TRUE,
                       type=c("both","upper","lower"),
                       legPos="topleft",sepFac=0.8,...) {
#
# Function plot.cband().  To plot data, fitted lines, confidence
# and prediction bands for a mixture of (single variable) regressions.
# The argument "x" is an object of class "cband" as returned by
# the function cband().
#
type   <- match.arg(type)

y      <- x$y
xf     <- x$xf
bnds   <- x$bnds
theta  <- x$theta
int    <- x$intercept
alpha  <- x$alpha
varnms <- x$varnms
x      <- x$x
K      <- length(theta)
ylim   <- range(c(y,unlist(bnds)),na.rm=TRUE)
xt     <- range(x)

ic <- if(type=="both") 1:2 else 1
ip <- if(type=="both") 3:4 else 2
main <- list(...)$main
if(is.null(main)) {
    pctype <- 1 + cbands + 2*pbands
    switch(EXPR=pctype,
        {tight <- "Fitted lines only."},
        {tight <- if(type=="both")
		     paste("Confidence bands, level = ",
                            100*(1-alpha),"%.",sep="")
	else if(type=="upper")
		paste("Upper confidence bands, level = ",
                             100*(1-alpha),"%.",sep="")
	else
		paste("Lower confidence bands, level = ",
                             100*(1-alpha),"%.",sep="")},
        {tight <- if(type=="both")
		paste("Prediction bands, level = ",
                               100*(1-alpha),"%.",sep="")
	else if(type=="upper")
		paste("Upper prediction bands, level = ",
                              100*(1-alpha),"%.",sep="")
	else
		paste("Lower prediction bands, level = ",
            100*(1-alpha),"%.",sep="")},
        {tight <- if(type=="both")
		paste("Prediction and confidence bands, level = ",
                               100*(1-alpha),"%.",sep="")
	else if(type=="upper")
		paste("Upper prediction and confidence bands, level = ",
                              100*(1-alpha),"%.",sep="")
	else
		paste("Lower prediction and confidence bands, level = ",
            100*(1-alpha),"%.",sep="")}
    )
} else {
    tight <- main
}
xlab <- list(...)$xlab
if(is.null(xlab)) xlab <- varnms[["nmx"]]
ylab <- list(...)$ylab
if(is.null(ylab)) ylab <- varnms[["nmy"]]
ltys <- list(...)$lty
if(is.null(ltys)) {
    ltys <- c(mdl=1)
    if(cbands) ltys <- c(ltys,cbands=2)
    if(pbands) ltys <- c(ltys,pbands=3)
} else {
    if(length(ltys) >= 3) {
        ltys <- ltys[1:3]
        names(ltys) <- c("mdl","cbands","pbands")
    } else if(length(ltys)==2) {
        if(cbands & pbands) stop("Argument \"lty\" should be of length 3.\n")
        if(cbands) names(ltys) <- c("mdl","cbands")
        if(pbands) names(ltys) <- c("mdl","pbands")
    } else {
        ltys <- rep(ltys,3)
        names(ltys) <- c("mdl","cbands","pbands")
    }
}
cols <- list(...)$col
if(is.null(cols)) {
    cols <- 1:(K+1)
} else {
    if(length(cols) > 1) {
        cols <- c(cols[1],rep(cols[-1],K))
    } else {
         cols <- rep(cols,K+1)
    }
}
pch <- list(...)$pch
if(is.null(pch)) pch <- 1

plot(0,0,type="n",xlim=range(x),ylim=ylim,xlab=xlab,ylab=ylab)
points(x,y,pch=pch,col=cols[1])
for(k in 1:K) {
    beta <- theta[[k]]$beta
    yt  <- if(int) cbind(1,xt)%*%beta else beta*xt
    lines(xt,yt,lty=ltys["mdl"],col=cols[k+1])
    if(cbands) {
        jv <- switch(EXPR=type,both=3:4,lower=1,upper=2)
        for(j in jv) lines(xf,bnds[[k]][,j],lty=ltys["cbands"],col=cols[k+1])
    }
    if(pbands) {
        jv <- switch(EXPR=type,both=7:8,lower=5,upper=6)
        for(j in jv) lines(xf,bnds[[k]][,j],lty=ltys["pbands"],col=cols[k+1])
    }
}
title(main=tight)
if(!is.null(legPos)) {
    leg1 <- "fitted model"
    if(cbands) leg1 <- c(leg1,"confidence bands")
    if(pbands) leg1 <- c(leg1,"prediction bands")
    if(length(leg1) > 1) {
        lp1 <- legend(legPos,lty=ltys,legend=leg1,bty="n")
        x   <- lp1$rect$left
        y   <- lp1$rect$top - sepFac*lp1$rect$h
    } else {
        x <- legPos
        y <- NULL
    }
    if(length(unique(cols[-1]))>1) {
        legend(x=x,y=y,lty=1,col=cols[-1],legend=paste0("Component",1:K),bty="n")
    }
}

invisible()
}
