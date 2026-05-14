envplot <- function(envlist, shade = TRUE, shade.color = "lightgrey", show.variance = FALSE,
                    # out.point = list(pch = 20, col = "red"), in.point = list(pch = 20, col = "black"),
                    xlim = NULL, ylim = NULL, main = NULL,
                    xlab = "Distance", ylab = "Semivariance"){
  if (!is.list(envlist)) stop(
    "The method 'envsig' must be applied to an object from the output of envelope()")
  if (is.null(envlist$variogram0)) stop(
    "The method 'envsig' must be applied to an object from the output of envelope()")
  check.arg <- function(x){
    (is.null(x) | is.character(x))
  }
  if (!check.arg(main)) stop("Argument 'main' must be a vector of type character")
  if (!check.arg(xlab)) stop("Argument 'xlab' must be a vector of type character")
  if (!check.arg(ylab)) stop("Argument 'ylab' must be a vector of type character")
  if (!is.logical(shade)) stop("Argument 'shade' must be a logical")
  switch(class(envlist$variogram0)[1],
         "gstatVariogram"={
           if (is.null(ylim)){
             ylim <- c(0, max(envlist$upper, envlist$variogram0$gamma)*1.1)
           }
           if (is.null(xlim)){
             xlim <- c(0, max(envlist$variogram0$dist))
           }
           plot(envlist$variogram0$dist, envlist$variogram0$gamma, type="n", xlim = xlim, ylim = ylim,
                xlab = xlab, ylab = ylab, main = main)
           if (shade){
             polygon(c(envlist$variogram0$dist, rev(envlist$variogram0$dist)),
                     c(envlist$lower,rev(envlist$upper)),col=shade.color,border=NA)
           } else {
             lines(envlist$variogram0$dist, envlist$upper, lty=2)
             lines(envlist$variogram0$dist, envlist$lower, lty=2)
           }
           idx <- c(which(envlist$variogram0$gamma < envlist$lower),
                    which(envlist$variogram0$gamma > envlist$upper))
           # Experimental
           # do.call(points,
           #         c(list(x=envlist$variogram0$dist[-idx], y=envlist$variogram0$gamma[-idx]),in.point))
           # do.call(points,
           #         c(list(x=envlist$variogram0$dist[idx], y=envlist$variogram0$gamma[idx]), out.point))
           points(envlist$variogram0$dist[-idx], envlist$variogram0$gamma[-idx],pch=20)
           points(envlist$variogram0$dist[idx], envlist$variogram0$gamma[idx], pch=20, col="red")
           if (show.variance){
             abline(h=var(envlist$dataValues))
             abline(h=var(envlist$dataValues) +
                      qnorm(c(1-envlist$conf.level/2,envlist$conf.level/2))*sd(envlist$dataValues), col="blue")
           }
           outside <- length(idx)
           total <- length(envlist$variogram0$dist)
         },
         "variogram"={
           if (is.null(ylim)){
             ylim <- c(0, max(envlist$upper, envlist$variogram0$v)*1.1)
           }
           if (is.null(xlim)){
             xlim <- c(0, max(envlist$variogram0$u))
           }
           plot(envlist$variogram0$u, envlist$variogram0$v, type = "n",xlim = xlim, ylim = ylim,
                xlab = xlab, ylab = ylab, main = main)
           if (shade){
             polygon(c(envlist$variogram0$u, rev(envlist$variogram0$u)),
                     c(envlist$lower,rev(envlist$upper)),col=shade.color,border=NA)
           } else {
             lines(envlist$variogram0$u, envlist$upper, lty=2)
             lines(envlist$variogram0$u, envlist$lower, lty=2)
           }
           idx <- c(which(envlist$variogram0$v < envlist$lower),
                    which(envlist$variogram0$v > envlist$upper))
           points(envlist$variogram0$u[-idx], envlist$variogram0$v[-idx],pch=20)
           points(envlist$variogram0$u[idx], envlist$variogram0$v[idx], pch=20, col="red")
           if (show.variance){
             abline(h=var(envlist$dataValues))
             abline(h=var(envlist$dataValues) +
                      qnorm(c(1-envlist$conf.level/2,envlist$conf.level/2))*sd(envlist$dataValues), col="blue")
           }
           outside <- length(idx)
           total <- length(envlist$variogram0$u)
         })
  print(paste0("There are ", outside ," out of ", total,
        " variogram estimates outside the ", envlist$conf.level*100, "% envelope."))
}
