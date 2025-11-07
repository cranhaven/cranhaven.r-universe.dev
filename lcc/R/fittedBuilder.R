#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: fittedBuilder.R                                               #
# Contains: fittedBuilder function                                    #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Build Fitted Values for
##'   \code{lcc} Objects
##'
##' @description This is an internally called function used to build
##'   fitted values.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
fittedBuilder <- function(object, type){
  ret <- list()
  .form <- switch(type,
                  "lcc" = 1,
                  "lpc" = 2,
                  "la"  = 3)
  .name <- switch(type,
                  "lcc" = "LCC",
                  "lpc" = "LPC",
                  "la"  = "LA")
  if (inherits(object$Summary.lcc$comp, "character")) {
    if (inherits(object$Summary.lcc$fitted, "data.frame")) {
      ret <- data.frame(Methods = object$Summary.lcc$comp,
                        Time = object$Summary.lcc$fitted[,"Time"],
                        LCC = object$Summary.lcc$fitted[, .name])
      attr(ret, "row.names")
    }else{
      ret <- data.frame(Methods = object$Summary.lcc$comp,
                        Time = object$Summary.lcc$fitted[[.form]][,"Time"],
                        LCC = object$Summary.lcc$fitted[[.form]][, .name])
      attr(ret, "row.names")
    }
  }else{
    if (inherits(object$Summary.lcc$fitted, "data.frame")) {
      fit <- list()
      rn <- list()
      met <- list()
      for(i in 1:length(object$Summary.lcc$comp)){
        fit[[i]] <- data.frame(LCC = object$Summary.lcc$fitted[[i]][, .name])
        rn[[i]] <- data.frame(Time = object$Summary.lcc$fitted[[i]][,"Time"])
        met[[i]] <- data.frame(Methods = rep(object$Summary.lcc$comp[[i]], nrow(fit[[i]])))
      }
      ret <- data.frame(do.call(rbind.data.frame, met), do.call(rbind.data.frame, rn),
                        do.call(rbind.data.frame, fit))
      attr(ret, "row.names")
    }else{
      fit <- list()
      rn <- list()
      met <- list()
      if(is.null(object$Summary.lcc$fitted$LCC)){
        for(i in 1:length(object$Summary.lcc$comp)){
          fit[[i]] <- data.frame(LCC = object$Summary.lcc$fitted[[i]][, .name])
          rn[[i]] <- data.frame(Time = object$Summary.lcc$fitted[[i]][,"Time"])
          met[[i]] <- data.frame(Methods = rep(object$Summary.lcc$comp[[i]], nrow(fit[[i]])))
        }
      }else{
        for(i in 1:length(object$Summary.lcc$comp)){
          fit[[i]] <- data.frame(LCC = object$Summary.lcc$fitted[[.form]][[i]][, .name])
          rn[[i]] <- data.frame(Time = object$Summary.lcc$fitted[[.form]][[i]][,"Time"])
          met[[i]] <- data.frame(Methods = rep(object$Summary.lcc$comp[[i]], nrow(fit[[i]])))
        }
      }
      ret <- data.frame(do.call(rbind.data.frame, met), do.call(rbind.data.frame, rn),
                        do.call(rbind.data.frame, fit))
      attr(ret, "row.names")
    }
  }
  colnames(ret)[colnames(ret)=="LCC"] <- paste0("fitted.",.name)
  return(ret)
}
