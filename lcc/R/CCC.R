#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: CCC.R                                                         #
# Contains: CCC function                                              #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Compute the Sampled Concordance
##'   Correlation Values.
##'
##' @description This is an internally called functions used to compute
##'   the sampled concordance correlation values.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom stats var cov cor
##'
##' @keywords internal
CCC<-function(Y1,Y2){
  data=data.frame(Y1,Y2)
  m1<-mean(Y1)
  m2<-mean(Y2)
  S1<-var(Y1)
  S2<-var(Y2)
  S12<-cov(Y1, Y2)
  CCC_lin<-2*S12/(S1+S2+(m1-m2)^2)
  return(CCC_lin)
}
