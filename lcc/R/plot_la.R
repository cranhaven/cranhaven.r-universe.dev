#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: plot_la.R                                                     #
# Contains: plot_la function                                          #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 03/06/2020                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Prepare the \code{plotBuilder_la}
##'   Function.
##'
##' @description This is an internally called functions used to prepare
##'   the \code{\link[lcc]{plotBuilder_la}} function.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
##'
plot_la <- function(Cb,ENV.Cb, tk.plot, tk.plot2,ldb, model,
                    ci, arg, ...) {
  CCC<-CCC_lin(dataset=model$data, resp="resp", subject="subject",
               method="method", time="time")
  Pearson<-Pearson(dataset=model$data, resp="resp", subject="subject",
                   method="method", time="time")
  if(ci==FALSE){
    plotBuilder_la(CCC = CCC, Pearson = Pearson, tk.plot = tk.plot,
                 tk.plot2 = tk.plot2, ldb = ldb, Cb=Cb,
                 model = model, ci=FALSE, arg = arg, ...)

  }else{
    plotBuilder_la(CCC = CCC, Pearson = Pearson, ENV.Cb = ENV.Cb,
                   tk.plot = tk.plot, tk.plot2 = tk.plot2, ldb = ldb,
                   Cb=Cb, model = model, ci=TRUE, arg = arg, ...)
  }
}
