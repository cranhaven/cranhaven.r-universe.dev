#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lccSummary.R                                                  #
# Contains: lccSummary function                                       #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Summarize Fitted and Sampled Values for
##'   \code{lcc} Objects
##'
##' @description This is an internally called function used to summarize
##'   fitted and sampled values, and the concordance correlation
##'   coefficient between them for \code{lcc} objects.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom stats predict
##'
##' @keywords internal
lccSummary<-function(model, q_f, diffbeta, tk,
                      tk.plot, tk.plot2, rho, ENV.LCC,
                      rho.pearson, ENV.LPC, Cb, ENV.Cb,
                      ldb, ci, components){
  if(components==FALSE){
    if(ci==FALSE){
      CCC<-CCC_lin(dataset=model$data, resp="resp", subject="subject",
                   method="method", time="time")
    if(ldb==1){
        comp <- paste0(levels(model$data$method)[2], " vs. ",
                       levels(model$data$method)[1])
        LCC.data<-data.frame("Time"=tk.plot,"LCC"=rho)
        CCC.data<-data.frame("Time" = tk.plot2, "CCC" = CCC)
        colnames(CCC.data) <- c("Time", "CCC")
        GF<-CCC(predict(model), model$data$resp)
        plot.data<-list("fitted"=LCC.data,"sampled"=CCC.data,
                        "gof" = GF, "comp"=comp)
    }else{
      LCC.data<-list()
      comp <- list()
      for(i in 1:ldb) {
        comp[[i]] <- paste0(levels(model$data$method)[i+1],
                            " vs. ", levels(model$data$method)[1])
        LCC.data[[i]]<-data.frame("Time"=tk.plot,"LCC"=rho[[i]])
        CCC.data<-data.frame("Time" = tk.plot2, "CCC" = CCC)
        colnames(CCC.data) <- c("Time", "CCC")
      }
      GF<-CCC(predict(model), model$data$resp)
      plot.data<-list("fitted"=LCC.data,"sampled"=CCC.data, "gof" = GF,
                      "comp"=comp)
    }
  }else{
    if(ldb==1){
      comp = paste0(levels(model$data$method)[2],
                    " vs. ", levels(model$data$method)[1])
      CCC<-CCC_lin(dataset=model$data, resp="resp",
                   subject="subject", method="method", time="time")
      LCC.data<-data.frame("Time"=tk.plot,"LCC"=rho,
                           "Lower"=ENV.LCC[1,], "Upper"=ENV.LCC[2,])
      CCC.data<-data.frame("Time" = tk.plot2, "CCC" = CCC)
      colnames(CCC.data) <- c("Time", "CCC")
      GF<-CCC(predict(model), model$data$resp)
       plot.data<-list("fitted"=LCC.data,"sampled"=CCC.data, "gof" = GF,
                       "comp" = comp)
    }else{
      CCC<-CCC_lin(dataset=model$data, resp="resp",
                   subject="subject", method="method", time="time")
      LCC.data<-list()
      comp <- list()
      for(i in 1:ldb) {
        comp[[i]] <- paste0(levels(model$data$method)[i+1],
                            " vs. ", levels(model$data$method)[1])
        LCC.data[[i]]<-data.frame("Time" = tk.plot,"LCC"=rho[[i]],
                                  "Lower" = ENV.LCC[[i]][1,],
                                  "Upper" = ENV.LCC[[i]][2,])
        CCC.data<-data.frame("Time" = tk.plot2, "CCC" = CCC)
        colnames(CCC.data) <- c("Time", "CCC")
      }
      GF<-CCC(predict(model), model$data$resp)
      plot.data<-list("fitted"=LCC.data,"sampled"=CCC.data,
                      "gof" = GF,"comp" = comp)
      }
    }
  }else{
    if(ci==FALSE){
      CCC<-CCC_lin(dataset=model$data, resp="resp",
                   subject="subject", method="method", time="time")
      Pearson<-Pearson(dataset=model$data, resp="resp",
                       subject="subject", method="method", time="time")
       if(ldb==1){
        comp <- paste0(levels(model$data$method)[2], " vs. ",
                       levels(model$data$method)[1])
        LA <- CCC[[1]]/Pearson[[1]]
        LCC.data<-data.frame("Time"=tk.plot,"LCC"=rho,
                             "LPC"=rho.pearson, "LA"=Cb)
        CCC.data<-data.frame("Time" = tk.plot2, "CCC" = CCC,
                             "Pearson" = Pearson, "Cb" = LA)
        colnames(CCC.data) <- c("Time", "CCC", "Pearson", "Cb")
        GF<-CCC(predict(model), model$data$resp)
        plot.data<-list("fitted"=LCC.data,"sampled"=CCC.data,
                        "gof" = GF, "comp"=comp)
      }else{
        LCC.data <- list()
        CCC.data <- list()
        LA <- list()
        comp <- list()
        for(i in 1:ldb) {
          comp[[i]] <- paste0(levels(model$data$method)[i+1],
                              " vs. ", levels(model$data$method)[1])
          LA[[i]] <- CCC[[i]]/Pearson[[i]]
          LCC.data[[i]] <- data.frame("Time"=tk.plot,"LCC"=rho[[i]],
                                      "LPC"=rho.pearson[[i]],
                                      "LA"=Cb[[i]])
          CCC.data[[i]]<-data.frame("Time" = tk.plot2, "CCC" = CCC[[i]],
                                    "Pearson" = Pearson[[i]], "Cb" = LA[[i]])
          colnames(CCC.data[[i]]) <- c("Time", "CCC", "Pearson", "Cb")
        }
        GF<-CCC(predict(model), model$data$resp)
        plot.data<-list("fitted"=LCC.data,"sampled"=CCC.data,
                        "gof" = GF, "comp" = comp)
      }
    }else{
      if(ldb==1){
        CCC<-CCC_lin(dataset=model$data, resp="resp", subject="subject",
                     method="method", time="time")
        Pearson<-Pearson(dataset=model$data, resp="resp",
                         subject="subject", method="method", time="time")
        LA<-CCC[[1]]/Pearson[[1]]
        comp <- paste0(levels(model$data$method)[2],
                       " vs. ", levels(model$data$method)[1])
        LCC.data<-data.frame("Time"=tk.plot,"LCC"=rho,
                             "Lower"=ENV.LCC[1,], "Upper"=ENV.LCC[2,])
        LPC.data<-data.frame("Time"=tk.plot,"LPC"=rho.pearson,
                             "Lower"=ENV.LPC[1,], "Upper"=ENV.LPC[2,])
        LA.data<-data.frame("Time"=tk.plot,"LA"=Cb, "Lower"=ENV.Cb[1,],
                            "Upper"=ENV.Cb[2,])
        CCC.data<-data.frame("Time" = tk.plot2, "CCC" = CCC,
                             "Pearson" = Pearson, "Cb" = LA)
        colnames(CCC.data) <- c("Time", "CCC", "Pearson", "Cb")
        fit<-list("LCC" = LCC.data, "LPC" = LPC.data, "LA" = LA.data)
        GF<-CCC(predict(model), model$data$resp)
        plot.data<-list("fitted"=fit,"sampled" = CCC.data,
                        "gof" = GF, "comp" = comp)
      }else{
        CCC<-CCC_lin(dataset=model$data, resp="resp", subject="subject",
                     method="method", time="time")
        Pearson<-Pearson(dataset=model$data, resp="resp",
                         subject="subject", method="method",
                         time="time")
        LA<-list()
        CCC.data <- list()
        LCC.data <- list()
        LPC.data <- list()
        LA.data <- list()
        comp <- list()
        for(i in 1:ldb) {
          comp[[i]] <- paste0(levels(model$data$method)[i+1], " vs. ",
                              levels(model$data$method)[1])
          LA[[i]]<-CCC[[i]]/Pearson[[i]]
          LCC.data[[i]]<-data.frame("Time"=tk.plot,"LCC"=rho[[i]],
                                    "Lower"=ENV.LCC[[i]][1,],
                                    "Upper"=ENV.LCC[[i]][2,])
          LPC.data[[i]]<-data.frame("Time"=tk.plot,
                                    "LPC"=rho.pearson[[i]],
                                    "Lower"=ENV.LPC[[i]][1,],
                                    "Upper"=ENV.LPC[[i]][2,])
          LA.data[[i]]<-data.frame("Time"=tk.plot,"LA"=Cb[[i]],
                                   "Lower"=ENV.Cb[[i]][1,],
                                   "Upper"=ENV.Cb[[i]][2,])
          CCC.data[[i]]<-data.frame("Time" = tk.plot2, "CCC" = CCC[[i]],
                                    "Pearson" = Pearson[[i]], "LA" = LA[[i]])
          colnames(CCC.data[[i]]) <- c("Time", "CCC", "Pearson", "Cb")
        }
        fit<-list("LCC" = LCC.data, "LPC" = LPC.data, "LA" = LA.data)
        GF<-CCC(predict(model), model$data$resp)
        plot.data<-list("fitted"=fit, "sampled" = CCC.data,
                        "gof" = GF, "comp" = comp)
      }
    }
  }
  return(invisible(plot.data))
}
