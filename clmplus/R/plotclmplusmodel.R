#' Plot the hazard model residuals 
#'
#' This function allows to plot the hazard model residuals on the triangle payments.
#' 
#' @param x \code{clmplusmodel} object, model fit to plot.
#' @param heat.lim limits in the residuals plot.
#' @param ... Extra arguments to be passed to the plot function.
#' 
#' @examples
#' data(sifa.mtpl)
#' sifa.mtpl.rtt <- AggregateDataPP(cumulative.payments.triangle=sifa.mtpl)
#' clm.fit<-clmplus(sifa.mtpl.rtt, 'a')
#' plot(clm.fit)
#' 
#' @return No return value, plots the hazard model residuals in triangular form.
#' 
#' @references 
#' 
#' Pittarello, Gabriele, Munir Hiabu, and Andrés M. Villegas. "Replicating and extending chain ladder 
#' via an age-period-cohort structure on the claim development in a run-off triangle." arXiv preprint arXiv:2301.03858 (2023).
#' 
#' @export
plot.clmplusmodel <- function(x,
                              heat.lim=c(-2.5,2.5),
                              ...){
  
  if((x$apc_input$hazard.model %in% names(pkg.env$models))|(x$apc_input$hazard.model=="user.defined")){
    
    res.m = stats::residuals(x$model.fit)
    res.tr=pkg.env$c2t(res.m$residuals)
    colnames(res.tr) <- rownames(res.tr) <- c(0:(dim(res.tr)[2]-1))
    longdf.no.0 = ChainLadder::as.LongTriangle(res.tr)
    
  }
  
  # if(x$apc_input$hazard.model == 'lc'){
  #   
  #   data.O= x$model.fit$data.T$occurrance
  #   data.E= x$model.fit$data.T$exposure
  #   ind <- is.na(data.E)
  #   W = matrix(1,nrow=dim(data.O)[1],ncol=dim(data.E)[2])
  #   W[ind]=0
  #   
  #   ax.mx = matrix(rep(x$model.fit$ax,dim(data.O)[2]),
  #                  byrow = F,
  #                  ncol=dim(data.O)[2])
  #   
  #   bx.mx = matrix(rep(x$model.fit$bx,
  #                      dim(data.O)[2]),
  #                  byrow = F,
  #                  ncol=dim(data.O)[2])
  #   
  #   kt.mx = matrix(rep(x$model.fit$kt,
  #                      dim(data.O)[1]),
  #                  byrow = T,
  #                  nrow=dim(data.O)[1])
  #   
  #   
  #   mu.mx = exp(ax.mx+bx.mx*kt.mx)
  #   
  #   data.O.h = data.E*mu.mx
  #   
  #   res <- array(NA, dim(W))
  #   
  #   res[!ind] <- 2 * W[!ind] * (data.O[!ind] * log(data.O[!ind] / data.O.h[!ind]) - (data.O[!ind] - data.O.h[!ind]))
  #   signRes <- sign(data.O - data.O.h)
  #   
  #   phi <- sum(res[!ind]) / ((dim(data.O)[1]*(dim(data.O)[1]+1))/2 - 3*dim(data.O)[1])
  #   res.m <- signRes * sqrt(abs(res) / phi) 
  #   
  #   res.tr=pkg.env$c2t(res.m)
  #   colnames(res.tr) <- rownames(res.tr) <- c(0:(dim(res.tr)[2]-1))
  #   longdf.no.0 = ChainLadder::as.LongTriangle(res.tr)
  # 
  # }
  # 
  # 
  p_hm <- ggplot2::ggplot(data=longdf.no.0, ggplot2::aes(x=as.integer(dev)-1, y=as.integer(origin)-1)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = value))+ggplot2::scale_y_reverse()+
    ggplot2::scale_fill_gradient2(name="model residuals", 
                         low="royalblue", 
                         mid="white", 
                         high="#a71429", 
                         midpoint=0, 
                         space="Lab", 
                         na.value="grey50", 
                         limits=heat.lim,
                         guide="colourbar")+
    ggplot2::labs(x="Development year", y="Accident year")+
    ggplot2::ggtitle(x$modelfamily)+
    ggplot2::theme(axis.title.x = ggplot2::element_text(size=8), axis.text.x  = ggplot2::element_text(size=7))+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size=8), axis.text.y  = ggplot2::element_text(size=7))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey", colour = "grey", size = 2, linetype = "solid"),
          panel.grid = ggplot2::element_line(colour="grey")) + 
    NULL
  
    p_hm
  
}








