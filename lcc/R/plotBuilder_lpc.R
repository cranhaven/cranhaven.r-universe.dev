#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: plotBuilder_lpc.R                                             #
# Contains: plotBuilder_lpc function                                  #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 03/06/2020                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Produces a Longitudinal Perason
##'   Correlation Plot.
##'
##' @description This is an internally called function used to produces
##'   a longitudinal Perason correlation plot from fitted ans sampled
##'   values with or not non-parametric confidence intervals.
##'
##' @details returns a inital plot for the longitudinal Pearson
##'   correlation.
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @usage NULL
##' 
##' @return No return value, called for side effects
##'
##' @keywords internal
plotBuilder_lpc<-function(LPC, ENV.LPC, tk.plot, Pearson,
                          tk.plot2, ldb, model, ci, arg, ...){
  Time<-tk.plot
  if(ci==FALSE){
    if(ldb == 1) {
      data_plot<-data.frame("LPC"=LPC,
                            "Time"=tk.plot)
      data_plot2<-data.frame("Pearson"=Pearson[[1]]$V1,
                             "Time"=tk.plot2)
      LPC<-data_plot$LPC
      Time<-data_plot$Time
      Plot<-ggplot(data_plot, aes(y=LPC, x=Time))+
        geom_line(data=data_plot, colour=arg$colour, size=arg$size)+
        geom_point(data=data_plot2, aes(y=Pearson, x=Time),
                   shape=arg$shape)+
        ggtitle(paste(levels(model$data$method)[2], "vs.",
                      levels(model$data$method)[1]))+
        labs(x = paste0(arg$xlab))+
        labs(y = paste0(arg$ylab))+
        theme(plot.title = element_text(hjust = 0.5))
      print(Plot)
    } else{
      data_plot<-list(NA)
      data_plot2<-list(NA)
      for(i in 1:ldb){
        data_plot[[i]] <- data.frame("LPC"=LPC[,i],
                                     "Time"=tk.plot)
        data_plot[[i]]$Level <-
          paste(levels(model$data$method)[i+1], "vs.",
                levels(model$data$method)[1])
        data_plot2[[i]] <- data.frame("Pearson" = Pearson[[i]]$V1,
                                      "Time" = tk.plot2)
        data_plot2[[i]]$Level <-
          paste(levels(model$data$method)[i+1], "vs.",
                levels(model$data$method)[1])
      }
      data_plot_ <- do.call(rbind.data.frame, data_plot)
      data_plot2_ <- do.call(rbind.data.frame, data_plot2)
      Plot <- ggplot(data_plot_, aes(y=LPC, x=Time))+
        geom_line(data=data_plot_, colour=arg$colour, size=arg$size)+
        geom_point(data=data_plot2_, aes(y=Pearson, x=Time), shape=arg$shape)+
        facet_wrap(~ Level, ...) +
        labs(x = paste0(arg$xlab)) +
        labs(y = paste0(arg$ylab)) +
        theme(plot.title = element_text(hjust = 0.5))
      print(Plot)
    }
  }else{
    if(ldb == 1) {
      data_plot<-data.frame("LPC"=LPC,
                            "Time"=tk.plot,
                            "lower_LPC"=t(ENV.LPC)[,1],
                            "upper_LPC"=t(ENV.LPC)[,2]
                            )
      data_plot2<-data.frame("Pearson"=Pearson[[1]]$V1,
                             "Time"=tk.plot2)
      LPC<-data_plot$LPC
      Time<-data_plot$Time
      lower_LPC<-data_plot$lower_LPC
      upper_LPC<-data_plot$upper_LPC
      Plot<-ggplot(data_plot, aes(y=LPC, x=Time))+
    geom_line(data=data_plot, colour=arg$colour, size=arg$size)+
      geom_point(data=data_plot2, aes(y=Pearson, x=Time),
                 shape=arg$shape)+
      geom_ribbon(data=data_plot,aes(ymin=lower_LPC,ymax=upper_LPC),
                  fill="grey70", alpha=0.3,show.legend = TRUE)+
      ggtitle(paste(levels(model$data$method)[2], "vs.",
                  levels(model$data$method)[1]))+
      labs(x = paste0(arg$xlab))+
      labs(y = paste0(arg$ylab))+
      theme(plot.title = element_text(hjust = 0.5))
      print(Plot)
    } else{
      data_plot<-list(NA)
      data_plot2<-list(NA)
      for(i in 1:ldb){
        data_plot[[i]] <- data.frame("LPC" = LPC[,i],
                               "Time" = tk.plot,
                               "lower_LPC" = t(ENV.LPC[[i]])[,1],
                               "upper_LPC" = t(ENV.LPC[[i]])[,2])
        data_plot[[i]]$Level <-
          paste(levels(model$data$method)[i+1], "vs.",
                levels(model$data$method)[1])
        data_plot2[[i]] <- data.frame("Pearson" = Pearson[[i]]$V1,
                                      "Time" = tk.plot2)
        data_plot2[[i]]$Level <-
          paste(levels(model$data$method)[i+1], "vs.",
                levels(model$data$method)[1])
      }
      data_plot_ <- do.call(rbind.data.frame, data_plot)
      data_plot2_ <- do.call(rbind.data.frame, data_plot2)
      Plot <- ggplot(data_plot_, aes(y=LPC, x=Time))+
        geom_line(data=data_plot_, colour=arg$colour, size=arg$size)+
        geom_point(data=data_plot2_, aes(y=Pearson, x=Time), shape=arg$shape)+
        geom_ribbon(data=data_plot_, aes(ymin = lower_LPC,
                                         ymax = upper_LPC),
                    fill="grey70", alpha = 0.3, show.legend = TRUE) +
        facet_wrap(~Level, ...) +
        labs(x = paste0(arg$xlab)) +
        labs(y = paste0(arg$ylab)) +
        theme(plot.title = element_text(hjust = 0.5))
      print(Plot)
    }
  }
}
