
# simplified Gelman convergence diagnostic using Brooks & Gelman's "interval" method.

# See Brooks & Gelman (1998) General methods for monitoring convergence of iterative simulations. J Computational and Graphical Statistics, 7, 434-455. p. 441

# This follows WinBUGS in using the central 80% interval as the measure of width (WinBUGS manual p.27).

simpleRhat <- function(object, n.chains, burnin=0) {
  mcmcOutput::getRhat(x=object, bad=NA, sort=FALSE)
}

