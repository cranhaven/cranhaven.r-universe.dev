#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lcc_intervals.R                                               #
# Contains: lcc_intervals  function                                   #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Functions to Compute the Non-Parametric Confidence
##'   Intervals for LCC.
##'
##' @description This is an internally called functions used to compute
##'   the non-parametric confidence intervals for LCC.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom stats quantile sd qnorm
##'
##' @keywords internal
lcc_intervals<-function(rho, tk.plot, tk.plot2, ldb, model, ci,
                        percentileMet, LCC_Boot, alpha){
  ZFisher<-function(x){
    1/2*log((1+x)/(1-x))
  }
  if(ldb == 1) {
  LCC_IC <- matrix(0, ncol=length(LCC_Boot), nrow=length(LCC_Boot[[1]]))
  if(percentileMet=="TRUE"){
    for(i in seq_len(length(LCC_Boot))) {
      if(is.null(LCC_Boot[[i]])==FALSE){
        LCC_IC[,i] <- LCC_Boot[[i]]
      }else(message(i,"\n"))
}
    ENV.LCC <- apply(LCC_IC, 1, quantile, probs=c(alpha/2,1-alpha/2))
  }else{
    for(i in seq_len(length(LCC_Boot))) {
      if(is.null(LCC_Boot[[i]])==FALSE){
        LCC_IC[,i] <- ZFisher(LCC_Boot[[i]])
      }else(message(i,"\n"))
    }
    SE<-apply(LCC_IC, 1, sd)
    mean<-apply(LCC_IC, 1, mean)
    ENV.LCC<-matrix(NA, nrow = 2, ncol = length(SE))
    for(i in seq_len(length(SE))){
      ENV.LCC[,i]<-c(mean[i], mean[i])-
        c(qnorm(1-alpha/2)*SE[i],qnorm(alpha/2)*SE[i])
    }
    ENV.LCC<-(exp(2*ENV.LCC)-1)/(exp(2*ENV.LCC)+1)
  }
  CI.LCC<-list("rho"=rho,"ENV.LCC"=ENV.LCC)
  }else{
    LCC_IC<-list()
    ENV.LCC<-list()
    SE_LCC<-list()
    mean_LCC<-list()
    for(i in seq_len(ldb)){
      LCC_IC[[i]] <- matrix(0, ncol=length(LCC_Boot),
                            nrow=length(LCC_Boot[[1]][[i]]))
      if(percentileMet=="TRUE"){
       for(j in seq_len(length(LCC_Boot))) {
        if(is.null(LCC_Boot[[j]])==FALSE){
          LCC_IC[[i]][,j] <- LCC_Boot[[j]][[i]]
        }else(message(i,"\n"))
      }
       ENV.LCC[[i]] <- apply(LCC_IC[[i]], 1, quantile,
                             probs=c(alpha/2,1-alpha/2))
      }else{
        for(j in seq_len(length(LCC_Boot))) {
          if(is.null(LCC_Boot[[j]])==FALSE){
            LCC_IC[[i]][,j] <- ZFisher(LCC_Boot[[j]][[i]])
          }else(message(i,"\n"))
        }
        SE_LCC[[i]]<-apply(LCC_IC[[i]], 1, sd)
        mean_LCC[[i]]<-apply(LCC_IC[[i]], 1, mean)
        ENV.LCC[[i]]<-matrix(NA, nrow = 2, ncol = length(SE_LCC[[i]]))
        for(k in seq_len(length(SE_LCC[[i]]))){
          ENV.LCC[[i]][,k]<-c(mean_LCC[[i]][k], mean_LCC[[i]][k])-
            c(qnorm(1-alpha/2)*SE_LCC[[i]][k],qnorm(alpha/2)*SE_LCC[[i]][k])
        }
        ENV.LCC[[i]]<-(exp(2*ENV.LCC[[i]])-1)/(exp(2*ENV.LCC[[i]])+1)
        }
      }
    CI.LCC<-list("rho"=rho,"ENV.LCC"=ENV.LCC)
  }
 return(CI.LCC)
}
