erpplot <-
function (dta, design=NULL,effect=1, interval=c("none","pointwise","simultaneous"),
                    alpha=0.05, frames = NULL, ylim = NULL, nbs=NULL, ...) {
    interval = match.arg(interval,choices=c("none","pointwise","simultaneous"))
    erpdta = as.matrix(dta)
    n = nrow(erpdta)
    T = ncol(erpdta)
    if (typeof(erpdta) != "double") 
        stop("ERPs should be of type double")
    if (!is.null(frames)) 
        if (length(frames) != ncol(erpdta)) 
            stop(paste("frames should be either null or of length", 
                ncol(erpdta)))
    if (!is.null(frames)) {
        if (any(frames != sort(frames))) 
            stop("frames should be an ascending sequence of integers")
        tframes = frames
    }
    if (is.null(frames)) 
        tframes = 1:T
    if (is.null(design)) {
      if (is.null(ylim)) 
        limy = 1.05 * diag(apply(apply(erpdta, 2, range), 1,range))
      if (!is.null(ylim)) 
        limy = ylim
      matplot(tframes, t(erpdta), type = "l", ylim = limy, ...)
    }
    if (!is.null(design)) {
      erpfit = erptest(erpdta,design,nbs=nbs)
      signal = as.vector(erpfit$signal[effect-1,])
      sdsignal = as.vector(erpfit$sdsignal[effect-1,])
      if (interval=="none") {
        lwr = signal
        upr = signal
        if (is.null(ylim)) 
          limy = 1.05*range(c(lwr,upr))
        if (!is.null(ylim)) 
          limy = ylim
        plot(tframes,signal,type="n",ylim=limy,...)
        lines(tframes,signal,type="l",col="black",lwd=2,lty=1)
        abline(h=0,lty=3,lwd=2)
      }
      if (interval=="pointwise") {
        lwr = signal-qnorm(1-alpha/2)*sdsignal
        upr = signal+qnorm(1-alpha/2)*sdsignal
        if (is.null(ylim)) 
          limy = 1.05*range(c(lwr,upr))
        if (!is.null(ylim)) 
          limy = ylim
        plot(tframes,signal,type="n",ylim=limy,...)
        polygon(c(tframes,rev(tframes)),c(lwr,rev(upr)),col=gray.colors(24)[24],border="grey")
        lines(tframes,signal,type="l",col="black",lwd=2,lty=1)
        abline(h=0,lty=3,lwd=2)
      }
      if (interval=="simultaneous") {
        lwr = signal-qnorm(1-alpha/2)*sdsignal
        upr = signal+qnorm(1-alpha/2)*sdsignal
        lwrs = signal-qnorm(1-alpha/(2*T))*sdsignal
        uprs = signal+qnorm(1-alpha/(2*T))*sdsignal
        if (is.null(ylim)) 
          limy = 1.05*range(c(lwrs,uprs))
        if (!is.null(ylim)) 
          limy = ylim
        plot(tframes,signal,type="n",ylim=limy,...)
        polygon(c(tframes,rev(tframes)),c(lwrs,rev(uprs)),col=gray.colors(24)[24],border="grey")
        polygon(c(tframes,rev(tframes)),c(lwr,rev(upr)),col=gray.colors(24)[12],border="grey")
        lines(tframes,signal,type="l",col="black",lwd=2,lty=1)
        abline(h=0,lty=3,lwd=2)
      }
    }  
}
