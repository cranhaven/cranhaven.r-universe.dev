#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: plot_lpc.R                                                    #
# Contains: Pearson, plot_lpc                                         #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Estimate the Sampled Pearson
##'   Correlation.
##'
##' @description This is an internally called functions used to estimate
##'   the sampled Pearson correlation.
##'
##' @usage NULL
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##' @return No return value, called for side effects
##' @importFrom stats cor
##'
##' @keywords internal
Pearson<-function(dataset, resp, subject, method, time){
  #  resp<-NULL
  #  method<-NULL
  #  subject<-NULL
  #  Data<-dataBuilder(dataset = dataset, resp=resp, subject=subject,
  # method=method, time=time)
  Data <- dataset
  Data<-subset(Data,select = c(resp, method, time, subject))
  Data_s<-split(Data, Data$method)

  P.Lin<-function(Y1,Y2,time){
    data=data.frame(Y1,Y2,time)
    Cor<-as.data.frame(
      as.matrix(
        by(data[,1:2], data$time, function(x) {cor(x$Y1, x$Y2)})
      )
    )
    return(Cor)
  }
  Pearson.Lin<-list()
  for(i in 2:length(levels(Data$method))){
    Pearson.Lin[[i-1]]<-P.Lin(Y1=Data_s[[1]]$resp,Y2=Data_s[[i]]$resp,
                              time=Data$time)
  }
  return(Pearson.Lin)
}

##' @title Internal function to prepare the
##'   \code{\link[lcc]{plotBuilder_lpc}} function.
##'
##' @description This is an internally called functions used to prepare
##'   the \code{\link[lcc]{plotBuilder_lpc}} function.
##'
##' @usage NULL
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
plot_lpc <- function(LPC,ENV.LPC, tk.plot, tk.plot2,ldb, model,
                     ci, arg, ...) {
  Pearson<-Pearson(dataset=model$data, resp="resp", subject="subject",
                   method="method", time="time")
  if(ci==FALSE){
    plotBuilder_lpc(LPC = LPC, tk.plot = tk.plot,
                     tk.plot2 = tk.plot2, ldb = ldb, Pearson=Pearson,
                    model = model, ci=FALSE, arg = arg, ...)

  }else{
    plotBuilder_lpc(LPC = LPC, ENV.LPC = ENV.LPC, tk.plot = tk.plot,
                     tk.plot2 = tk.plot2, ldb = ldb, Pearson = Pearson,
                    model = model, ci=TRUE, arg = arg, ...)
  }
}
