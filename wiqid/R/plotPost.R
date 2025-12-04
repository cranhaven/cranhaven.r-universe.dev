
# Now a wrapper for mcmcOutput::postPlot

plotPost <-
function( ... ) {
  warning("'plotPost' is deprecated, please use 'mcmcOutput::postPlot'", call.=FALSE)
  mcmcOutput::postPlot(...)
}
