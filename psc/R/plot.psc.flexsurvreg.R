#' Function for Plotting PSC objects
#' @param pscOb an object of class 'psc'
#' @param addFit should a curve for the model fit be added?
#' @param ... not used
#' @return a survival plot corresponding to the psc fit
#' @details making use of 'ggsurvplot' in the survminer package, this function
#' plots the expected survival funtion for the 'control' treatment estimated
#' from the CFM along with the Kaplan Meier estimates of the observed events
#' @import ggplot2
#' @import ggpubr
#' @importFrom survival Surv survfit
#' @importFrom survminer ggsurvplot
#' @importFrom ggpubr ggarrange
plot.psc.flexsurvreg <- function(pscOb, addFit=T,...){

  # Binding local varaibles
  S <- trt <-s_fpm <- s_data <- lo <- hi <- NULL

  med <- coef(pscOb)
  med <- as.numeric(as.character(data.frame(med)$mean))

  ## defining treatment (for multiple treatment comparisons)
  #mtc.cond <- "trt"%in%colnames(pscOb$DC$X)
  #trt <- rep(1,nrow(x$DC_clean$cov))
  #if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])


  ### Getting model survival estimate
  s_fpm <- cfmSumm.flexsurvreg(pscOb,bootCI=TRUE);
  s_data <- data.frame("time"=s_fpm$time,"S"=s_fpm$S,"lo"=s_fpm$lo,"hi"=s_fpm$hi)

  ## plot
  out <- pscOb$DC$Y

  #out$trt <- trt
  sfit <- survfit(Surv(time,cen)~1,data=out)
  sfit_plot <- survminer::ggsurvplot(sfit,data=out,legend="none",risk.table=TRUE)

  ### Adding model confidence intervals
  p1 <- sfit_plot$plot + geom_line(data=s_data, aes(time,S),col=3,linewidth=1.5)

  if("lo"%in%names(s_data)){
    p1 <- p1 + geom_line(data=s_data, aes(time,lo),linetype=2,col=3,linewidth=1.5)
    p1 <- p1 + geom_line(data=s_data, aes(time,hi),linetype=2,col=3,linewidth=1.5)
  }

  if(addFit){
    for(i in 1:length(med)) {
      S2 <- s_data$S^(exp(med[i]))
      tmp_data <- data.frame("time"=s_data$time,"S2"=S2)
      p1 <- p1 + geom_line(data=tmp_data, aes(time,S2),linetype=1,col=3+i,linewidth=1.5)
    }
  }

  p2 <- sfit_plot$table
  p2$labels$y <- ""
  ggpubr::ggarrange(p1,p2,ncol=1,heights=c(3,1))

}





