qqMix <- function(object,xlim=NULL,ylim=NULL,
                  shape=c("disc","lozenge","square","none"),
                  ngon=20,size=1,...) {
K     <- ncol(object$resid)
resid <- as.vector(object$resid)
gamma <- as.vector(object$gamma)
ind   <- order(resid)
resid <- resid[ind]
gamma <- gamma[ind]
N     <- length(resid)
x     <- qnorm((0.5+(1:N))/(N+1))
shape <- match.arg(shape)
if(is.null(xlim)) xlim <- range(x)
if(is.null(ylim)) ylim <- range(resid)
    if(shape=="none") {
    plot(x,resid,xlim=xlim,ylim=ylim,xlab='normal quantiles',
             ylab='empirical quantiles',...)
    } else {
        plot(x,resid,type='n',xlim=xlim,ylim=ylim,
             xlab='normal quantiles',ylab='empirical quantiles')
        uin  <- parUin() # user units per inch
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
    N <- length(x)
    for(i in 1:N) polygon(x[i] + sym$x*gamma[i],resid[i] +
                                 sym$y*gamma[i],err=-1,...)
    }
    invisible()
}
