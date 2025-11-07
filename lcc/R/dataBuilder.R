#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: dataBuilder.R                                                 #
# Contains: dataBuilder function                                      #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################
##' @title Internal Function to Prepare the Dataset for \code{lcc}
##' Objects
##'
##' @description This is an internally called function used to prepare
##'   the dataset for \code{lcc} objects
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
dataBuilder <- function(dataset, resp, subject, method, time, gs = NULL){
  Data <- data.frame(dataset)
  Data <- try(rename.vars(Data, from = c(resp, subject, method, time),
                      to = c("resp", "subject", "method", "time"),
                      info = FALSE), TRUE)
  if (is.null(gs) == FALSE) {
      gold <- which.max(levels(Data$method) == gs)
      others <- seq(1, length(levels(Data$method)))[-gold]
      Data$method <- factor(Data$method,
                            levels = levels(Data$method)[c(gold,
                                                           others)])
  }
  return(Data)
}
