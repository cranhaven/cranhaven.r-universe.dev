getlik <- function(obj) UseMethod("getlik")
delete <- function(obj) UseMethod("delete")

gpsepms <- function(resp,design, d=NULL,g=0.001,nstarts=5)
{
    din <- ncol(design)
    d <- darg(d,design)
    g <- garg(g,resp)
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
    ret <- list(gpis=gpis,optidx=optidx,optgpi=gpis[optidx])
    class(ret) <- "gpsepms"
    return(ret)
}
predict.gpsepms <- function(obj,test)
{
    optgpi <- obj$gpis[obj$optidx]
    pred <- predGPsep(optgpi,test)
    return(pred)
}
getlik.gpsepms <- function(obj)
{
    optgpi <- obj$gpis[obj$optidx]
    llik <- llikGPsep(optgpi)
    return(llik)
}
delete.gpsepms <- function(obj)
{
    for(i in obj$gpis) deleteGPsep(i)
    return(NULL)
}

gpseplmms <- function(resp,design,type=c("cmean","lmean"),
                      d=NULL,g=0.001,nstarts=5)
{
    type <- match.arg(type)
    din <- ncol(design)
    d <- darg(d,design)
    g <- garg(g,resp)
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
    ret <- list(gpis=gpis,optidx=optidx,optgpi=gpis[optidx],type=type)
    class(ret) <- "gpseplmms"
    return(ret)
}
predict.gpseplmms <- function(obj,test)
{
    optgpi <- obj$gpis[obj$optidx]
    pred <- predGPsepLm(optgpi,test,obj$type)
    return(pred)
}
getlik.gpseplmms <- function(obj)
{
    optgpi <- obj$gpis[obj$optidx]
    llik <- llikGPsepLm(optgpi)
    return(llik)
}
delete.gpseplmms <- function(obj)
{
    for(i in obj$gpis) deleteGPsepLm(i)
    return(NULL)
}
