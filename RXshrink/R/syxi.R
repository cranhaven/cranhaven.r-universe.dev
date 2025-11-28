"syxi" <- function (form, data, i = 1)
{ 
    if (missing(form) || !inherits(form, "formula"))
        stop("First argument to syxi.fit() must be a valid linear regression formula.") 
    yvar <- deparse(form[[2]]) 
    xvar <- deparse(form[[3]]) 
    if (missing(data) || !inherits(data, "data.frame")) 
        stop("Second argument to syxi.fit() must be an existing Data Frame.") 
    dfname <- deparse(substitute(data))
    if (!is.element(yvar, dimnames(data)[[2]])) 
        stop("Response Y-variable in the syxi.fit() formula must be an existing variable.")
    if (!is.element(xvar, dimnames(data)[[2]])) 
        stop("Regressor X-variable in the syxi.fit() formula must be an existing variable.")
    lmobj <- lm(form, data)
    yvec <- as.matrix(lmobj$model[,1])
    xvec <- as.matrix(lmobj$model[,2])
    p <- ncol(xvec)
    if (p > 1) 
        stop("The formula must contain only 1 (non-constant) Regressor x-variable.")
    n <- length(xvec)
    if (n < 10) 
        stop("Number of observations must be at least 10.")
    if (i < 1 || i > 25)
        stop("The i-Index value must be an Integer between 1 and 25.")
    xname = paste0("x",i) 
    yonsx = mgcv::gam(yvec ~ s(xvec))            # gam = Generalized Additive Model...
    gam.sum = summary(yonsx)
    sxfit = yonsx$fitted.values
    dfsxf = data.frame(cbind(yvec,xvec,sxfit))
    names(dfsxf) = c("yvec","xvec","sxfit")
    dfsxf = dfsxf[order(dfsxf$xvec),]
    cmat = cor(dfsxf)
    sxnam = paste0("s",i)
    lmsxf = lm(yvec ~ sxfit)
    RXolist = list(dfname=dfname, y=as.vector(yvec), x=as.vector(xvec), sx=sxfit,
        xname=xname, sxnam=sxnam, dfsxf=dfsxf, yxcor=cmat[1,2], yscor=cmat[1,3],
        xscor=cmat[2,3], lmyxc=lmobj$coefficients, lmbc=lmsxf$coefficients,
        lmsxf=lmsxf$coefficients, adjR2=gam.sum$r.sq, yonsx=yonsx)
    class(RXolist) <- "syxi" 
    RXolist
}

"plot.syxi" = function (x, type = "xy", ...) 
{
    if (type == "xy") {
      plot(x$x, x$y, xlab=x$xname, ylab="y")
      lmyx = lm(x$y ~ x$x)
      abline(lmyx$coefficients[1],lmyx$coefficients[2],lwd=3,lty=2,col=2)
      lines(x$dfsxf$xvec,x$dfsxf$sxfit,lwd=3,lty=1,col=4)
    }
    if (type != "xy") {      # type = "sy"
      plot(x$sx,x$y,xlab=x$sxnam,ylab="y")
      lmsx = lm(x$sx ~ x$y)
      abline(lmsx$coefficients[1],lmsx$coefficients[2],lwd=3,lty=1,col=4)
    }
}
