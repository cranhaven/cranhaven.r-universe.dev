#' Plot the hazard model fitted and forecasted parameters
#'
#' This function allows to define the behavior of the triangle payments.
#' 
#' @param x \code{clmpluspredictions}, Model effects (fitted and extrapolated) to be plotted.
#' @param cy.type \code{character}, whether to show fitted period effect with or without extrapolatio Default is "fe", standing for fitted and extrapolated. Alternative is to specify "f" for fitted effect.
#' @param ... Arguments to be passed to plot.
#' @examples
#' data(sifa.mtpl)
#' sifa.mtpl.rtt <- AggregateDataPP(cumulative.payments.triangle=sifa.mtpl)
#' clm.fit<-clmplus(sifa.mtpl.rtt, 'a')
#' clm <- predict(clm.fit)
#' plot(clm)
#' 
#' @return No return value, plots coefficients of the hazard models.
#' 
#' @references 
#' Pittarello, G., Hiabu, M., & Villegas, A. M. (2023). Replicating and extending chain-ladder via an age-period-cohort structure on the claim development in a run-off triangle. arXiv preprint arXiv:2301.03858.
#' 
#' @export
plot.clmpluspredictions <- function(x, 
                              cy.type ="fe", 
                              ...){
  
  plot_list=NULL
  
  if(!is.null(x$apc_output$hazard.model)){
    
    a.tp <- grepl('a',x$apc_output$hazard.model)
    c.tp <- grepl('c',x$apc_output$hazard.model)
    p.tp <- grepl('p',x$apc_output$hazard.model)
    lc.tp <- grepl('lc',x$apc_output$hazard.model)
    
    if(lc.tp){
      
      a.df <- data.frame(x=0:(length(x$apc_output$model.fit$ax)-1),
                         y=x$apc_output$model.fit$ax,
                         y2=x$apc_output$model.fit$bx)
      
      #age effect
      p1 <- ggplot2::ggplot(a.df,
                            ggplot2::aes(x=x,y=y))+
        ggplot2::geom_line()+
        ggplot2::theme_classic()+
        ggplot2::xlab('Development period')+
        ggplot2::ylab(expression(a[j])) + 
        ggplot2::ggtitle("Fitted effect")
      
      #age deviation from calendar
      p2 <- ggplot2::ggplot(a.df,
                            ggplot2::aes(x=x,y=y2))+
        ggplot2::geom_line()+
        ggplot2::theme_classic()+
        ggplot2::xlab('Development period')+
        ggplot2::ylab(expression(b[j])) + 
        ggplot2::ggtitle("Fitted effect")
      
      #calendar effect
      kt=as.vector(x$apc_output$model.fit$kt)
      kt.f=as.vector(x$apc_output$alphaij$kt.f$mean)
      kt.tot=c(kt,kt.f)
      
      kt.u80=as.vector(x$apc_output$alphaij$kt.f$upper[,1])
      kt.u80=c(rep(NA,length(kt)),kt.u80)
      kt.u95=as.vector(x$apc_output$alphaij$kt.f$upper[,2])
      kt.u95=c(rep(NA,length(kt)),kt.u95)
      kt.l80=as.vector(x$apc_output$alphaij$kt.f$lower[,1])
      kt.l80=c(rep(NA,length(kt)),kt.l80)
      kt.l95=as.vector(x$apc_output$alphaij$kt.f$lower[,2])
      kt.l95=c(rep(NA,length(kt)),kt.l95)
      
      ckj.df= data.frame(x=0:(length(kt.tot)-1),y=kt.tot,kt.u80,kt.u95,kt.l80,kt.l95)
      
      
      p3 <- ggplot2::ggplot(data=ckj.df[1:length(kt),],
                            ggplot2::aes(x=x,
                                         y=y))+
        ggplot2::geom_line()+
        ggplot2::theme_classic()+
        ggplot2::xlab('Calendar period')+
        ggplot2::ylab(expression(c[k+j]))+
        ggplot2::ggtitle('Fitted effect')
      
      p3.f <- ggplot2::ggplot(data=ckj.df[-1,],ggplot2::aes(x=x,y=y))+
        ggplot2::geom_line()+
        ggplot2::geom_ribbon(ggplot2::aes(ymin = kt.l80, ymax = kt.u80), alpha = 0.2)+
        ggplot2::geom_ribbon(ggplot2::aes(ymin = kt.l95, ymax = kt.u95), alpha = 0.1)+
        ggplot2::theme_classic()+
        ggplot2::xlab('Calendar period')+
        ggplot2::ylab(expression(c[k+j]))+
        ggplot2::ggtitle('Fitted and extrapolated effect')
      
      
      
      plot_list[[1]]=p1
      plot_list[[2]]=p2
      if(cy.type=='f'){
        plot_list[[3]]=p3}else{
          
          plot_list[[3]]=p3.f
          
        }
      
      
      c.tp=FALSE
    }
    
    
    if(a.tp){
      
      a.df <- data.frame(x=(x$apc_output$model.fit$ages-1),
                         y=x$apc_output$model.fit$ax)
      p1 <- ggplot2::ggplot(a.df,
                            ggplot2::aes(x=x,y=y))+
        ggplot2::geom_line()+
        ggplot2::theme_classic()+
        ggplot2::xlab('Development period')+
        ggplot2::ylab(expression(a[j])) + 
        ggplot2::ggtitle("Fitted effect")
      
      plot_list[[as.numeric(a.tp)]]=p1
      
    }
    
    
    if(c.tp){
      
      gc.fitted <- x$apc_output$model.fit$gc
      gc.fitted <- gc.fitted[!is.na(gc.fitted)]
      gc.tot <- c(gc.fitted,x$apc_output$alphaij$gc.f$mean[1])
      
      gc.l80 <- as.vector(x$apc_output$alphaij$gc.f$lower[1,1])
      gc.l80 <- c(rep(0,length(gc.fitted)),gc.l80)
      gc.l95 <- as.vector(x$apc_output$alphaij$gc.f$lower[1,2])
      gc.l95 <- c(rep(0,length(gc.fitted)),gc.l95)
      
      gc.u80 <- as.vector(x$apc_output$alphaij$gc.f$upper[1,1])
      gc.u80 <- c(rep(0,length(gc.fitted)),gc.u80)
      gc.u95 <- as.vector(x$apc_output$alphaij$gc.f$upper[1,2])
      gc.u95 <- c(rep(0,length(gc.fitted)),gc.u95)
      
      
      
      c.df <- data.frame(x=0:(length(gc.tot)-1),
                         y=as.vector(gc.tot),
                         gc.u80=gc.u80,
                         gc.l80=gc.l80,
                         gc.u95=gc.u95,
                         gc.l95=gc.l95)
      
      p2 <- ggplot2::ggplot(data=c.df[1:(dim(c.df)[1]-1),],
                            ggplot2::aes(x=x,
                                         y=y))+
        ggplot2::geom_point(ggplot2::aes(x=c.df[dim(c.df)[1],'x'],y=c.df[dim(c.df)[1],'y']),colour="red")+
        ggplot2::geom_line()+
        ggplot2::theme_classic()+
        ggplot2::xlab('Accident period')+
        ggplot2::ylab(expression(g[k]))+
        ggplot2::ggtitle('Fitted and extrapolated effect')
      
      
      plot_list[[as.numeric(a.tp+c.tp)]]=p2
      
      
      
    }
    
    
    if(p.tp){
      
      kt=as.vector(x$apc_output$model.fit$kt[1,])
      kt.f=as.vector(x$apc_output$alphaij$kt.f$mean)
      kt.tot=c(kt,kt.f)
      
      kt.u80=as.vector(x$apc_output$alphaij$kt.f$upper[,1])
      kt.u80=c(rep(NA,length(kt)),kt.u80)
      kt.u95=as.vector(x$apc_output$alphaij$kt.f$upper[,2])
      kt.u95=c(rep(NA,length(kt)),kt.u95)
      kt.l80=as.vector(x$apc_output$alphaij$kt.f$lower[,1])
      kt.l80=c(rep(NA,length(kt)),kt.l80)
      kt.l95=as.vector(x$apc_output$alphaij$kt.f$lower[,2])
      kt.l95=c(rep(NA,length(kt)),kt.l95)
      
      ckj.df= data.frame(x=0:(length(kt.tot)-1),y=kt.tot,kt.u80,kt.u95,kt.l80,kt.l95)
      
      
      p3 <- ggplot2::ggplot(data=ckj.df[1:length(kt),],
                            ggplot2::aes(x=x,
                                         y=y))+
        ggplot2::geom_line()+
        ggplot2::theme_classic()+
        ggplot2::xlab('Calendar period')+
        ggplot2::ylab(expression(c[k+j]))+
        ggplot2::ggtitle('Fitted effect')
      
      p3.f <- ggplot2::ggplot(data=ckj.df[-1,],ggplot2::aes(x=x,y=y))+
        ggplot2::geom_line()+
        ggplot2::geom_ribbon(ggplot2::aes(ymin = kt.l80, ymax = kt.u80), alpha = 0.2)+
        ggplot2::geom_ribbon(ggplot2::aes(ymin = kt.l95, ymax = kt.u95), alpha = 0.1)+
        ggplot2::theme_classic()+
        ggplot2::xlab('Calendar period')+
        ggplot2::ylab(expression(c[k+j]))+
        ggplot2::ggtitle('Fitted and extrapolated effect')
      
      
      if(cy.type=='f'){
        plot_list[[as.numeric(a.tp+c.tp+p.tp)]]=p3}else{
          
          plot_list[[as.numeric(a.tp+c.tp+p.tp)]]=p3.f
          
        }
      
    }
    
    
    
    
    
    
  }else{
    
    warning('For non standard models clmplus only shows StMoMo default')
    plot(x$apc_output$model.fit)
  }
  
  do.call(getExportedValue("gridExtra","grid.arrange"), c(plot_list, ncol = 1)) 
  
}



globalVariables(c("dy", "cy","y", "y2","ay","aes","dev","origin","value","incrementals","cumulatives"))





