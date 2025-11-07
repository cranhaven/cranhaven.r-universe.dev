#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: laBuilder.R                                                   #
# Contains: laBuilder function                                        #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Estimate the Longitudinal Accuracy.
##'
##' @description This is an internally called function used to estimate
##'   the longitudinal accuracy (LA).
##'
##' @usage NULL
##'
##' @return returns a vector or list containing the longitudinal
##'   accuracy estimates.
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
laBuilder <- function(G, diffbeta, tk, q_r, q_f, g, sig2_epsilon,
                        delta, deltal, model) {
  Tk_r <- sapply(0:q_r, function(x) tk^x)
  Tk_f <- sapply(0:q_f, function(x) tk^x)
  S2 <- try((as.matrix(Tk_f[,c(1:(length(diffbeta)))]) %*% (diffbeta))^2,
            silent = TRUE)
  if (!inherits(S2, "try-error")) {
    tGt <- diag(Tk_r %*% G %*% t(Tk_r))
    varcomp <- summary(model)
    var.f <- class(varcomp$modelStruct$varStruct)[1]
    LA <- list()
    #---------------------------------------------------------------------
    # If the var.class = varIdent
    #---------------------------------------------------------------------
    if(var.f == "varIdent") {
      gd <- g(delta)
      gdl <- g(deltal)
      ldb2 <- length(gdl)
      v <- list()
      u <- list()
      for(i in 1:ldb2){
        v[[i]]<-sqrt((tGt+sig2_epsilon*gd)/(tGt+sig2_epsilon*gdl[i]))
        u[[i]]<-sqrt(S2)/((tGt+sig2_epsilon*gd)*(tGt+sig2_epsilon*gdl[i]))^(1/4)
        LA[[i]] <- as.numeric(2 / (v[[i]]+v[[i]]^(-1)+u[[i]]^2))
      }
    }
    #---------------------------------------------------------------------
    # If the var.class = varExp
    #---------------------------------------------------------------------
    if(var.f == "varExp") {
      if(attr(varcomp$modelStruct$varStruct, "formula")==~time){
        gd <- g(delta,tk)
        gdl <- g(deltal,tk)
        v<-sqrt((tGt+sig2_epsilon*gd)/(tGt+sig2_epsilon*gdl))
        u<-sqrt(S2)/((tGt+sig2_epsilon*gd)*(tGt+sig2_epsilon*gdl))^(1/4)
        LA <- list(2 / (v+v^(-1)+u^2), NA)
      } else {if(attr(varcomp$modelStruct$varStruct, "formula")==~time | method){
        gd <- g(delta,tk)
        ldb2 <- length(deltal)
        gdl<-list()
        v <- list()
        u <- list()
        for(i in 1:ldb2){
          gdl[[i]] <- g(deltal[i],tk)
          v[[i]]<-sqrt((tGt+sig2_epsilon*gd)/(tGt+sig2_epsilon*gdl[[i]]))
          u[[i]]<-sqrt(S2)/((tGt+sig2_epsilon*gd)*(tGt+sig2_epsilon*gdl[[i]]))^(1/4)
          LA[[i]] <-  as.numeric(unlist(2 / (v[[i]]+v[[i]]^(-1)+u[[i]]^2)))
        }
      }else{print("method not implemented yet")}
      }
    }
    #---------------------------------------------------------------------
    # If var.class = NULL
    #---------------------------------------------------------------------
    if(var.f == "NULL"){
      v<-sqrt((tGt+sig2_epsilon)/(tGt+sig2_epsilon))
      u<-sqrt(S2)/((tGt+sig2_epsilon)*(tGt+sig2_epsilon))^(1/4)
      LA <- list(2 / (v+v^(-1)+u^2), NA)
    }
    return(LA)
  }else {
    stop(print("Please, change the name of factor levels associated with
               method argument. Example:  levels M1, M2, M3, ..., Mn,
               with n=0, 1, 2, ..., N."), call. = FALSE)
  }
}
