plot.mixreg <- function(x,y,cMeth=c("none","distance","prob"),
                        legPos="topleft",...) {
#
# Method to plot the result of fitting a mixture of regressions
# Model.  Plots the data and fitted regression lines.
#
# If the number of components is one, we don't need a legend.
if(missing(legPos) & nrow(x$parmat)==1) legPos <- NULL

vnms   <- all.vars(x$formula)
nmy    <- vnms[1]
noPred <- length(vnms) == 1
if(noPred) {
    vnms <- c(vnms,"index")
    x$data[["index"]] <- 1:length(x$data[[nmy]])
}
    
if(length(vnms)==2) {
    nmx <- vnms[2]
    ok  <- ncol(as.matrix(x$data[[nmx]])) == 1
} else {
    ok <- FALSE
}
if(!ok) {
    stop("This plot method can only be used for models with at most one predictor.\n")
}

cMeth <- match.arg(cMeth)

x$data <- as.data.frame(x$data)
K    <- nrow(x$parmat)
xr   <- range(x$data[,nmx])
yr   <- range(x$data[,nmy])
xlim <- list(...)$xlim
if(is.null(xlim)) xlim <- xr
if(noPred) {
    xm <- matrix(c(1,1),ncol=1)
    jays <- 1
} else {
    xm   <- if(x$intercept) cbind(1,xlim) else matrix(xlim,ncol=1)
    jays <- if(x$intercept) 1:2 else 1
}
yh   <- xm%*%t(x$parmat[,jays,drop=FALSE])
ylim <- list(...)$ylim
if(is.null(ylim)) ylim <- range(yr,yh)
xlab <- list(...)$xlab
if(is.null(xlab)) xlab <- nmx
ylab <- list(...)$ylab
if(is.null(ylab)) ylab <- nmy
cols <- list(...)$col
if(is.null(cols)) {
    cols <- if(cMeth=="none") 1:(K+1) else 1:K
} else {
    cols <- if(cMeth=="none") rep(cols,K+1) else rep(cols,K)
}
ltys <- list(...)$lty
if(is.null(ltys)) ltys <- 1
ltys <- rep(ltys,K)
pch  <- list(...)$pch
if(is.null(pch)) pch <- 1
cex  <- list(...)$cex
if(is.null(cex)) cex <- 1
bg   <- list(...)$bg # If it's NULL it stays NULL.

# Classify the points into groups.
if(cMeth=="none") {
    gps <- NULL
} else {
    mm  <- model.matrix(x$formula,data=x$data)
    nmy <- as.character(x$formula[[2]])
    if(cMeth=="distance") {
        xx  <- if(x$intercept) mm[,2] else mm[,1]
        yy  <- x$data[[nmy]]
        dM  <- distMat(xx,yy,x$theta)
        gps <- factor(apply(dM,1,which.min))
    } else if(cMeth=="prob") {
        xxx <- gfun(mm,x$data[[nmy]],x$theta)
        gps <- factor(apply(xxx$gamma,1,which.max))
    } else {
         stop(paste("Classification method",cMeth,"not recognized.\n"))
    }
}

plot(0,0,type='n',xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
     main=list(...)$main)
kull <- if(cMeth=="none") cols[1] else cols[gps]
points(x$data[,nmx],x$data[,nmy], col=kull, pch=pch, cex=cex)
kull <- if(cMeth=="none") cols[-1] else cols
for(j in 1:K) {
    lines(xlim,yh[,j],col=kull[j],lty=ltys[j])
}
if(!is.null(legPos)) {
    legend(legPos,lty=ltys,col=kull,legend=paste("Component",1:K),bty="n")
}

if(cMeth=="none") {
    rslt <- NULL
} else {
    rslt <- cbind(x$data,groups=gps)
    attr(rslt,"cMeth") <- cMeth
}
invisible(rslt)
}
