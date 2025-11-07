#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lpcWrapper.R                                                  #
# Contains: lpcWrapper function                                       #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Prepare the \code{lpcBuilder}
##'   Function
##'
##' @description This is an internally called function used toprepare
##'   the \code{\link[lcc]{lpcBuilder}} function.
##'
##' @usage NULL
##'
##' @return returns a vector or list containing the longitudinal
##'   Pearson correlation estimates.
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
lpcWrapper<-function(model, q_f, tk, n.delta){
  G <- getVarCov(model)
  q_r <- dim(G)[1] - 1
  deltas <- getDelta(model = model)
  delta <- deltas$delta
  deltal <- deltas$deltal
  g <- deltas$g
  sig2_epsilon <- model$sigma^2
  rho.pearson <- lpcBuilder(G = G, tk = tk, q_r = q_r,
                     q_f = q_f, g = g, sig2_epsilon = sig2_epsilon,
                     delta = delta, deltal = deltal, model = model)
  return(rho.pearson[[n.delta]])
}
