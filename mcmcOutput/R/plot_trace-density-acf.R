
# tracePlot, densityPlot and acfPlot functions for class mcmcOutput

# Strategy (designed to avoid repeating code)
# * tracePlot, densityPlot and acfPlot call plotStuff with appropriate type
# * plotStuff checks object, ask, dots, nChains, etc and calls the appropriate *1 function
# * traceplot1, densityPlot1 and acfPlot1 do the individual plots

tracePlot <- function(object, layout=c(3,3), ask=NULL, ...)  {
  
  objName <- deparse(substitute(object))
  
  plotStuff("trace", object=object, objName=objName, layout=layout, ask=ask, ...)
}

tracePlot1 <- function(mat, name, dots) {
  defaultArgs <- list(xlab="Iterations", ylab="", type='l', lty=1)
  useArgs <- modifyList(defaultArgs, dots)
  useArgs$y <- mat
  useArgs$main <- name
  do.call(matplot, useArgs)
  abline(h=mean(mat))
}
# ..........................................................

densityPlot <- function(object,  layout=c(3,3), ask=NULL, ...)  {

  objName <- deparse(substitute(object))
  
  plotStuff("density", object=object, objName=objName, layout=layout, ask=ask, ...)
}

densityPlot1 <- function(mat, name, dots) {
  defaultArgs <- list(ylab="Density", type='l', lty=1, xlab="")
  useArgs <- modifyList(defaultArgs, dots)
  # useArgs$main <- name
  densPlot0(mat, useArgs)
  title(main=name)
}
# ..........................................................

acfPlot <- function(object, lag.max=NULL, layout=c(3,3), ask=NULL, ...)  {

  objName <- deparse(substitute(object))
  
  plotStuff("acf", object=object, objName=objName, layout=layout, ask=ask, lag.max=lag.max, ...)
  
}

acfPlot1 <- function(mat, name, lag.max, dots) {

  defaultArgs <- list(ylab="ACF", xlab="Lag", type='h', lty=1)
  useArgs <- modifyList(defaultArgs, dots)

  acor <- apply(mat, 2, function(x) acf(x, lag.max=lag.max, plot=FALSE)$acf)
  if(any(is.na(acor))) {
    plot(1, 1, type = "n", ann = FALSE, axes = FALSE)
    text(1,1, "No ACF calculated.")
    title(main=name)
  } else {
    lags <- 0:(nrow(acor) - 1)
    if (ncol(mat) > 1) {
      jitt <- seq(-0.2, 0.2, length=ncol(mat))
    } else {
      jitt <- 0
    }
    lags.mat <- outer(lags, jitt, "+")
    useArgs$main <- name
    useArgs$x <- lags.mat
    useArgs$y <- acor
    do.call(matplot, useArgs)
    abline(h=0)
  }
}
