
# Core function to do trace plots, density plots and acf plots.

# Strategy (designed to avoid repeating code)
# * tracePlot, densityPlot and acfPlot call plotTDA with appropriate type
# * plotStuff checks object, ask, dots, nChains, etc and calls the appropriate *1 function
# * trace1, dens1 and acf1 do the individual plots

# This stuff should be in the ... argument:  TODO


plotStuff <- function(type, object, objName, layout, ask, lag.max, ... ) {

  object <- try(mcmcOutput(object), silent=TRUE)
  if(inherits(object, "try-error"))
    stop("Sorry, can't convert your input to a mcmcOutput object.", call.=FALSE)

  # Deal with layout
  nPlots <- ncol(object)
  if(nPlots < prod(layout)) {  # adjust the layout
    if(nPlots <= 3) {
      layout <- c(nPlots, 1)
    } else if(nPlots <= 6) {
      layout <- c(ceiling(nPlots/2), 2)
    }
  }
  nChains <- attr(object, "nChains")

  # Deal with ... argument:
  dots <- list(...)
  if(length(dots) == 1 && inherits(dots[[1]], "list"))
    dots <- dots[[1]]
  if(!is.null(dots$main)) {
    objName <- dots$main
    dots$main <- NULL
  }

  # Do the plots
  old.par <- par(mar = c(4,1,1,1)+0.1, oma=c(1,1,3,1), "mfrow")
    on.exit(par(old.par))
  if(nPlots > prod(layout)) {
    old.ask <- devAskNewPage(dev.interactive(orNone=TRUE))
    on.exit(devAskNewPage(old.ask), add=TRUE)
  }
  par(mfrow=layout)
  for(i in 1:nPlots) {
    mat <- matrix(object[, i], ncol=nChains)
    name <- colnames(object)[i]
    if(any(is.na(mat))) {
      plot(0,0, type = "n", xlim=c(-1,1), main = name, ann = FALSE, axes = FALSE)
      text(0,0, "Chain contains NAs.")
      next
    }
    switch(type,
      trace = tracePlot1(mat, name, dots),
      density = densityPlot1(mat, name, dots),
      acf = acfPlot1(mat, name, lag.max=lag.max, dots) )
    title(objName, outer=TRUE)
  }
}
