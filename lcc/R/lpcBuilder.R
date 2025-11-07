#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lpcBuilder.R                                                  #
# Contains: lpcBuilder function                                       #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Estimate the Longitudinal Pearson
##'   Correlation.
##'
##' @description This is an internally called function used to estimate
##'   the longitudinal Pearson correlation (LPC).
##'
##' @return returns a vector or list containing the longitudinal
##'   Pearson correlation estimates.
##'
##' @usage NULL
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
lpcBuilder<-function(G, tk, q_r, q_f, g, sig2_epsilon,
                      delta, deltal, model){
  Tk_r <- sapply(0:q_r, function(x) tk^x)
  Tk_f <- sapply(0:q_f, function(x) tk^x)
  tGt <- diag(Tk_r %*% G %*% t(Tk_r))
  varcomp <- summary(model)
  var.f <- class(varcomp$modelStruct$varStruct)[1]
  rho.pearson <- list()
  if(var.f == "varIdent") {
    gd <- g(delta)
    gdl <- g(deltal)
    ldb2 <- length(gdl)
    for(i in 1:ldb2){
      rho.pearson[[i]] <- as.numeric(
        tGt / sqrt((tGt+sig2_epsilon*gd)*(tGt+sig2_epsilon*gdl[i]))
      )
    }
  }
  if(var.f == "varExp") {
    if(attr(varcomp$modelStruct$varStruct, "formula")==~time){
      gd <- g(delta,tk)
      gdl <- g(deltal,tk)
      rho.pearson <-list(tGt / sqrt((tGt +sig2_epsilon*gd)*
                                      (tGt +sig2_epsilon*gdl)), NA)
    } else {if(attr(varcomp$modelStruct$varStruct, "formula")==~time | method){
      gd <- g(delta,tk)
      ldb2 <- length(deltal)
      gdl<-list()
      for(i in 1:ldb2){
        gdl[[i]] <- g(deltal[i],tk)
        rho.pearson[[i]] <-
          as.numeric(unlist(tGt / sqrt((tGt + sig2_epsilon*gd)*
                                         (tGt + sig2_epsilon*gdl[[i]]))))
      }
    }else{print("method not implemented yet")}
    }
  }
  if(var.f == "NULL"){
    rho.pearson<-list(tGt / (tGt + sig2_epsilon),NA)
  }
  return(rho.pearson)
}
