#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: plot_lcc.R                                                    #
# Contains: CCC_lin, plot_lcc                                         #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Estimate the Sampled Concordance
##'   Correlation Coefficient.
##'
##' @description This is an internally called function used to estimate
##'   the sampled concordance correlation coefficient.
##'
##' @usage NULL
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##' @return No return value, called for side effects
##' @importFrom stats cor cov
##'
##' @keywords internal
CCC_lin<-function(dataset, resp, subject, method, time){
  Data <- dataset
  Data<-subset(Data, select = c(resp, method, time, subject))
  Data_s<-split(Data, Data$method)

  Lin<-function(Y1,Y2,time){
    data=data.frame(Y1,Y2,time)
    m1<-with(data,tapply(Y1, time, mean))
    m2<-with(data,tapply(Y2, time, mean))
    S1<-with(data,tapply(Y1, time, var))
    S2<-with(data,tapply(Y2, time, var))
    S12<-as.data.frame(
      as.matrix(
        by(data[,1:2], data$time, function(x) {cov(x$Y1, x$Y2)})
      )
    )
    Cor<-as.data.frame(
      as.matrix(
        by(data[,1:2], data$time, function(x) {cor(x$Y1, x$Y2)})
      )
    )
    data.lin<-data.frame("time"=unique(time),"M1"=as.numeric(m1),
                         "M2"=as.numeric(m2),"S1"=as.numeric(S1),
                         "S2"=as.numeric(S2),"S12"=as.numeric(S12[,1]),
                         "Cor"=as.numeric(Cor[,1]))
    CCC<-as.data.frame(
      as.matrix(
        by(data.lin[,2:6],data.lin$time,
           function(x){2*x$S12/(x$S1+x$S2+(x$M1-x$M2)^2)})
      )
    )
    return(CCC)
  }
  CCC.Lin<-list()
  for(i in 2:length(levels(Data$method))){
    CCC.Lin[[i-1]]<-Lin(Y1=Data_s[[1]]$resp,Y2=Data_s[[i]]$resp,
                        time=Data$time)
  }
  return(CCC.Lin)
}

##' @title Internal function to prepare the \code{plotBuilder_lcc}
##'   function.
##'
##' @description This is an internally called function used to prepare
##'   the \code{\link[lcc]{plotBuilder_lcc}} function.
##'
##' @usage NULL
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
plot_lcc <- function(rho,ENV.LCC, tk.plot, tk.plot2,ldb, model,
                     ci, arg, ...) {
  CCC<-CCC_lin(dataset=model$data, resp="resp", subject="subject",
               method="method", time="time")
  if(ci==FALSE){
    plotBuilder_lcc(rho = rho, tk.plot = tk.plot,
                 tk.plot2 = tk.plot2, ldb = ldb, CCC=CCC,
                 model = model, ci=FALSE, arg = arg, ...)

  }else{
    plotBuilder_lcc(rho = rho, ENV.LCC = ENV.LCC, tk.plot = tk.plot,
                 tk.plot2 = tk.plot2, ldb = ldb, CCC=CCC,
                 model = model, ci=TRUE, arg = arg, ...)
  }
}
