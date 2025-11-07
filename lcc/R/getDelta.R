#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: getDelta.R                                                    #
# Contains: getDelta function                                         #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################
##' @title Internal Function to Extract Variance Components Estimates.
##'
##' @usage NULL
##'
##' @description This is an internally called function used to extract
##'   variance components estimate of \eqn{\Sigma} matrix based on
##'   specified structure.
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br} and Rafael de Andrade Moral,
##'   \email{rafael_moral@@yahoo.com.br}
##'
##' @keywords internal
getDelta <- function(model) {
  varcomp <- summary(model)
  if(is.null(varcomp$modelStruct$varStruct)) {
    delta <- deltal <- 0
    g1 <- function(x) 1
    g<-g1
  } else {
    var.f <- class(varcomp$modelStruct$varStruct)[1]
    if(var.f == "varIdent") {
      components2 <-
        c(1, exp(as.numeric(varcomp$modelStruct$varStruct))^2)
      delta <- 1
      deltal <- components2[-1]
      g2 <- function(x) x
      g<-g2
    }
    if(var.f == "varExp") {
      if(attr(varcomp$modelStruct$varStruct, "formula")==~time){
        delta=deltal <- varcomp$modelStruct$varStruct
        g <- function(x, tk) exp(2*x*tk)
      } else{if(attr(varcomp$modelStruct$varStruct, "formula")==~time | method){
        components2 <- as.numeric(varcomp$modelStruct$varStruct)
        delta=components2[1]
        deltal <- components2[-1]
        g3 <- function(x, tk) exp(2*x*tk)
        g<-g3
        }
      }
    }
  }
  return(list(delta=delta, deltal=deltal, g=g))
}
