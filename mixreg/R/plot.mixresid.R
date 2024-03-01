plot.mixresid <- function(x,vsFit=FALSE,whichx=1,digits=2,
                          shape=c("disc","lozenge","square","none"),
                          ngon=20,size=1,gexp=1,polycol=NULL,xlab=NULL,...) {
K      <- ncol(x$resid)
resid  <- as.vector(x$resid)
gamma  <- as.vector(x$gamma)
if(vsFit) {
    if(x$noPred) { 
        uxp <- 1:ncol(x$fvals)
        xp  <- rep(uxp,each=nrow(x$fvals))
    }  else {
        xp <- as.vector(x$fvals)
    }
} else {
    xp <- rep(as.matrix(x$x)[,whichx],K)
    if(is.null(xlab)) xlab <- x$vnms[1+whichx]
}
if(gexp < 0) stop("Argument \"gexp\" must be non-negative.\n")
shape <- match.arg(shape)
if(is.null(xlab)) xlab <- "fitted values"
if(vsFit & x$noPred) {
    xlim <- c(0,1+ncol(x$fvals))
    plot(xp,resid,type="n",xlim=xlim,xlab=xlab,ylab="residuals",axes=FALSE)
    axis(side=2)
    lbls <- format(x$fvals[1,],digits=digits)
    axis(side=1,at=1:ncol(x$fvals),labels=lbls)
    box()
} else {
    plot(xp,resid,type="n",xlab=xlab,ylab="residuals",...)
}
if(shape=="none") {
    points(xp,resid,xlab=xlab,ylab="residuals",...)
} else {
    uin <- parUin()
    switch(EXPR=shape,
        disc = {
            phi   <- c(seq(0,2*pi,length=ngon),0)
            scale <- 0.03*size/uin
            sym   <- list(x=cos(phi)*scale[1],y=sin(phi)*scale[2])
        },
        lozenge = {
            scale <- 0.03*size/uin
            sym   <- list(x=c(0.6,0,-0.6,0,0.6)*scale[1],y=c(0,1.4,0,-1.4,0)*scale[2])
        },
        square = {
            scale <- 0.03*size/(sqrt(2)*uin)
            sym   <- list(x=c(1,-1,-1,1,1)*scale[1],y=c(1,1,-1,-1,1)*scale[2])
        }
    )
    N <- length(xp)
    for(i in 1:N) {
        px <- xp[i] + sym$x*gamma[i]^gexp
        py <- resid[i] + sym$y*gamma[i]^gexp
        polygon(px,py,border=polycol,col=polycol)
    }
}
invisible()
}
