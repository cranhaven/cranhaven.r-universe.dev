ESL2D <- function(design,resp,yobs,candidate,frac=.95,nstarts=5,
                  mtype=c("zmean","cmean","lmean"),
                  gstart=0.0001)
{
    if(.Machine$sizeof.pointer != 8)
    {
        cat("the current version only supports 64-bit version of R\n")
        return(NULL)
    }
    mtype <- match.arg(mtype)
    design <- as.matrix(design)
    lbasis <- buildBasis(resp,frac)
    numbas <- lbasis$numbas
    coeff <- lbasis$coeff
    reddsq <- lbasis$redd^2
    cht <- drop(t(lbasis$basis)%*%yobs/lbasis$redd^2)
    nfea <- nrow(candidate)
    criter <- rep(0,nfea)
    for(i in 1:numbas)
    {
        ccoeff <- coeff[,i]
        gpobj <- if(mtype=="zmean") gpsepms(ccoeff,design,NULL,gstart,nstarts) else gpseplmms(ccoeff,design,mtype,NULL,gstart,nstarts)
        pred <- predict(gpobj,candidate)
        delete(gpobj)
        criter <- criter+reddsq[i]*(pred$s2+(pred$mean-cht[i])^2)
    }
    xhat <- candidate[which.min(criter),]
    return(xhat)
}
SL2D <- function(design,resp,yobs,candidate,frac=.95,nstarts=5,
                 mtype=c("zmean","cmean","lmean"),
                 gstart=0.0001)
{
    if(.Machine$sizeof.pointer != 8)
    {
        cat("the current version only supports 64-bit version of R\n")
        return(NULL)
    }
    mtype <- match.arg(mtype)
    design <- as.matrix(design)
    lbasis <- buildBasis(resp,frac)
    numbas <- lbasis$numbas
    coeff <- lbasis$coeff
    reddsq <- lbasis$redd^2
    cht <- drop(t(lbasis$basis)%*%yobs/lbasis$redd^2)
    nfea <- nrow(candidate)
    criter <- rep(0,nfea)
    for(i in 1:numbas)
    {
        ccoeff <- coeff[,i]
        gpobj <- if(mtype=="zmean") gpsepms(ccoeff,design,NULL,gstart,nstarts) else gpseplmms(ccoeff,design,mtype,NULL,gstart,nstarts)
        pred <- predict(gpobj,candidate)
        delete(gpobj)
        criter <- criter+reddsq[i]*(pred$mean-cht[i])^2
    }
    xopt <- candidate[which.min(criter),]
    return(xopt)
}
