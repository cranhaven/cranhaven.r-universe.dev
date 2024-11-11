fitgps_zmean <- function(resp,design,test,nstarts=5,d=NULL,gstart=0.0001,type="zmean")
{
    din <- ncol(design)
    if(!is.matrix(test)) test <- matrix(test,ncol=din)
    d <- darg(d,design)
    g <- garg(gstart,resp)
    parb <- lhs::maximinLHS(nstarts,din+1)
    ranb <- parb[,1:din,drop=FALSE]
    nugb <- parb[,din+1]
    ldmin <- log(d$min)
    ldmax <- log(d$max)
    ranb <- ldmin + ranb*(ldmax-ldmin)
    ranb <- exp(ranb)
    lgmin <- log(g$min)
    lgmax <- log(g$max)
    nugb <- lgmin + nugb*(lgmax-lgmin)
    nugb <- exp(nugb)
    gpis <- llik <- rep(NA,nstarts)
    for(i in 1:nstarts)
    {
        gpis[i] <- newGPsep(design,resp,ranb[i,],nugb[i],TRUE)
        mle <- jmleGPsep(gpis[i],drange=c(d$min,d$max),
                               grange=c(g$min,g$max),
                               dab=d$ab,gab=g$ab)
        llik[i] <- llikGPsep(gpis[i],dab=d$ab,gab=g$ab)
    }
    optidx <- which.max(llik)
    pred <- predGPsep(gpis[optidx],test)
    for(i in 1:nstarts) deleteGPsep(gpis[i])
    return(pred)
}
fitgps_lmean <- function(resp,design,test,nstarts=5,d=NULL,gstart=0.0001,type=c("cmean","lmean"))
{
    din <- ncol(design)
    if(!is.matrix(test)) test <- matrix(test,ncol=din)
    d <- darg(d,design)
    g <- garg(gstart,resp)
    parb <- lhs::maximinLHS(nstarts,din+1)
    ranb <- parb[,1:din,drop=FALSE]
    nugb <- parb[,din+1]
    ldmin <- log(d$min)
    ldmax <- log(d$max)
    ranb <- ldmin + ranb*(ldmax-ldmin)
    ranb <- exp(ranb)
    lgmin <- log(g$min)
    lgmax <- log(g$max)
    nugb <- lgmin + nugb*(lgmax-lgmin)
    nugb <- exp(nugb)
    gpis <- llik <- rep(NA,nstarts)
    for(i in 1:nstarts)
    {
        gpis[i] <- newGPsepLm(design,resp,ranb[i,],nugb[i],TRUE,type)
        mle <- jmleGPsepLm(gpis[i],drange=c(d$min,d$max),
                           grange=c(g$min,g$max),
                           dab=d$ab,gab=g$ab)
        llik[i] <- llikGPsepLm(gpis[i],dab=d$ab,gab=g$ab)
    }
    optidx <- which.max(llik)
    pred <- predGPsepLm(gpis[optidx],test,type)
    for(i in 1:nstarts) deleteGPsepLm(gpis[i])
    return(pred)

}
svdgpsepms <- function(X0,design,resp,frac=.95,nstarts=5,
                       mtype=c("zmean","cmean","lmean"),
                       d=NULL,gstart=0.0001,nthread=1,clutype="PSOCK")
{
    mtype <- match.arg(mtype)
    fitname <- if(mtype=="cmean") "lmean" else mtype
    fitgps <- get(paste("fitgps",fitname,sep="_"))
    lenresp <- length(resp)
    lbasis <- buildBasis(resp,frac)
    basis <- lbasis$basis
    numbas <- lbasis$numbas
    coeff <- lbasis$coeff
    resid <- resp-basis%*%t(coeff)
    varres <- drop(crossprod(as.vector(resid)))
    varres <- varres/(lenresp+2)
    ret <- genericApply(coeff,2,fitgps,design,X0,nstarts,d,gstart,mtype,
                        nthread=nthread,clutype=clutype)
    vmean <- matrix(unlist(sapply(ret,`[`,"mean")),nrow=numbas,byrow=TRUE)
    vsigma2 <- matrix(unlist(sapply(ret,`[`,"s2")),nrow=numbas,byrow=TRUE)
    pmean <- basis%*%vmean
    psd <- sqrt(basis^2%*%vsigma2+varres)
    ret <- list(mean=pmean,sd=psd,coeff=vmean,coeffs2=vsigma2,d2=lbasis$redd^2,
                basis=lbasis$basis,varres=varres)
    return(ret)
}
svdGP <- function(design,resp,X0=design,nstarts=5,gstart=0.0001,
                  frac=.95,centralize=FALSE,nthread=1,clutype="PSOCK")
{
    if(.Machine$sizeof.pointer != 8)
    {
        cat("the current version only supports 64-bit version of R\n")
        return(NULL)
    }
    if(centralize)
    {
        rmean <- apply(resp,1,mean)
        resp <- resp-rmean
    }
    ret <- svdgpsepms(X0,design,resp,frac,nstarts,
                      "zmean",NULL,gstart,nthread,clutype)
    pmean <- ret$mean
    if(centralize) pmean <- pmean+rmean
    res <- list(pmean=pmean,ps2=ret$sd^2)
    return(res)
}
