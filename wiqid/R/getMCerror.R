
# Calculation of MCMC error

# A wrapper for mcmcOutput::getMCE

getMCerror <- function(object, n.chains, SDpc=FALSE) {
  mcmcOutput::getMCE(x=object, pc=SDpc, bad=NA, sort=FALSE)
}
