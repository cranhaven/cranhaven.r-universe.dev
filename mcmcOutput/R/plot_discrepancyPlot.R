
# Posterior predictive GOF check
# Plot discrepancies for observed and simulated data.

discrepancyPlot <- function(object, observed, simulated, ...) {

  objName <- deparse(substitute(object))

  object <- mcmcOutput(object)  # coerce to class mcmcOutput

  # Check that observed and simulated exist
  nms <- colnames(object)
  if(!(observed %in% nms))
    stop("Can't find the node ", sQuote(observed), " in ", sQuote(objName), call.=FALSE )
  if(!(simulated %in% nms))
    stop("Can't find the node ", sQuote(simulated), " in ", sQuote(objName), call.=FALSE )

  # Get the things to plot
  obs <- object[, observed]
  sim <- object[, simulated]
  lims <- range(obs, sim)
  pval <- mean(sim > obs)

  # Deal with dots
  dots <- list(...)
  if(length(dots) == 1 && inherits(dots[[1]], "list"))
    dots <- dots[[1]]
  defaultArgs <- list(
    xlim=lims, ylim=lims, pch=16, cex=0.8, col=rgb(0, 0, 0, 0.3), bty='l',
    las=1, cex.lab=1.2,
    xlab=paste("Observed:", sQuote(observed)),
    ylab=paste("Simulated:", sQuote(simulated)),
    main="Posterior Predictive GOF check")
  useArgs <- modifyList(defaultArgs, dots)

  # Do the plot
  useArgs$x <- obs
  useArgs$y <- sim
  do.call(graphics::plot, useArgs)
  abline(0, 1, lwd = 2)
  where <- if (pval < 0.5) {
        "topleft"
  } else {
    "bottomright"
  }
  graphics::legend(where, paste("P =", round(pval, 2)),
        bty = "n", cex = 1.5)
  return(pval)
}
