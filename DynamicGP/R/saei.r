saEI <- function(xi,yi,yobs,nadd,candei,candest,func,...,
                 mtype=c("zmean","cmean","lmean"),
                 estsol=c("ESL2D","SL2D"),
                 frac=.95, nstarts=5, gstart=0.0001,
                 nthread=1, clutype="PSOCK")
{
    if(.Machine$sizeof.pointer != 8)
    {
        cat("the current version only supports 64-bit version of R\n")
        return(NULL)
    }
    xi <- as.matrix(xi)
    mtype <- match.arg(mtype)
    estsol <- match.arg(estsol)
    tlen <- length(yobs)
    maxinfo <- rep(0,nadd)
    for(i in 1:nadd)
    {
        lbasis <- buildBasis(yi,frac)
        barval <-  min(apply((yobs-yi)^2,2,sum))
        py <- svdgpsepms(candei,xi,yi,frac,nstarts=nstarts,
                         mtype=mtype,nthread=nthread,
                         clutype=clutype)
        info <- oeiinfo(py,yobs,barval)
        mm <- max(info,na.rm=TRUE)
        maxinfo[i] <- mm
        newidx <- which.max(info)
        newx <- candei[newidx,]
        newy <- func(newx,...)
        xi <- rbind(xi,newx)
        yi <- cbind(yi,newy)
        candei <- candei[-newidx,,drop=FALSE]
    }
    estfun <- get(estsol)
    xopt <- estfun(xi,yi,yobs,candest,frac,
                   nstarts=nstarts,gstart=gstart)
    ret <- list(xx=xi,yy=yi,xhat=xopt,maxei=maxinfo)
    return(ret)
}
