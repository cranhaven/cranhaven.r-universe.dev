plot.artfima <- function(x, which=c("all","logsdf", "loglogsdf", "res"),  
                         mainQ=TRUE, subQ=TRUE, lag.max=30, ...) {
  stopifnot("artfima" %in% class(x))
  which <- match.arg(which)
  if (which=="res" || which=="all") {
    res <- x$res
    n <- length(res)
    if (lag.max >= n) {
      lag.max <- n-1
    }
    clim <- 1.96/sqrt(n)
    layout(matrix(1:2, nrow=2))
    r <- as.vector(acf(res, lag.max=lag.max, plot=FALSE)$acf)[-1]
    rmax <- max(c(clim, abs(r)))*1.03
    plot(1:lag.max, r, xlab="lag", ylab="residual SACF", ylim=c(-rmax, rmax), 
         pch=16, type="o")
    abline(h=clim, col="red")
    abline(h=-clim, col="red")
    Qk <- n*(n+2)*cumsum(r^2/(n-(1:lag.max)))
    df <- 1:lag.max
    #df <- (1:lag.max)-x$nbeta
    #i0 <- which(df==0)
    #df <- df[-(1:i0)]
    #Qk <- Qk[-(1:i0)]
    pv <- 1-pchisq(Qk, df)
    plot(1:lag.max, pv, xlab="lag", ylab="p-value", ylim=c(0,1.05), pch=16, 
         type="o")
    abline(h=0.05, col="red")
    layout(1)
  } 
  if(which=="logsdf" || which=="loglogsdf" || which=="all") {
    p <- x$arimaOrder[1]
    q <- x$arimaOrder[3]
    sigmaSq <- x$sigmaSq
    if (mainQ) {
      mainTitle <- paste0(x$glp, "(", p, ",", 0, ",", q, ")" )
    } else {
      mainTitle <- ""
    }
    if (subQ) {
      subTitle <- paste0(x$glp, "(", p, ",", 0, ",", q, ")" )
    } else {
      subTitle <- ""
    }
    w <- x$z
    n <- length(w)
    Ip <- Periodogram(w)
    fr <- (1/n)*(1:length(Ip))
    nplot <- max(n, 400) #0.5*nplot = number of ordinates
    y <- sigmaSq*artfimaSDF(n=nplot, obj=x, plot="none")
    yfr <- (1/nplot)*(1:length(y))
    if (which=="logsdf" || which=="all") {
      plot(fr, log(Ip), xlab="frequency", ylab="log power", 
           main=mainTitle, type="p", col=rgb(0,0,1,0.5),
           sub=subTitle)
      lines(fr, log(y), type="l", lwd=2, col="red")
    } 
    if (which=="loglogsdf" || which=="all") {
      plot(log(fr), log(Ip), xlab="log frequency", ylab="log power", 
           main=mainTitle, type="p", col=rgb(0,0,1,0.5),
           sub=subTitle)   
      lines(log(fr), log(y), type="l", lwd=2, col="red")
    }
  }
}
