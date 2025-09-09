#' Function for Plotting PSC objects
#' @param x an object of class 'psc'
#' @param ... not used
#' @return a survival plot corresponding to the psc fit
#' @details making use of 'ggsurvplot' in the survminer package, this function
#' plots the expected survival funtion for the 'control' treatment estimated
#' from the CFM along with the Kaplan Meier estimates of the observed events
#' @import ggplot2 survminer
plot.psc.flexsurvreg <- function(x, ...){

  # Binding local varaibles
  S <- trt <- NULL

  med <- coef(x)
  med <- med[-nrow(med),1]

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])


  ### Getting model survival estimate
  s_fpm <- surv_fpm(x$DC_clean)
  s_data <- data.frame("time"=s_fpm$time,"S"=s_fpm$S)

  # plot
  out <- x$DC_clean$outcome
  out$trt <- trt
  sfit <- survfit(Surv(time,cen)~trt,data=out)
  sfit_plot <- ggsurvplot(sfit,data=out,legend="none")$plot
  sfit_plot + geom_line(data=s_data, aes(time,S),col=6,lwd=1.5)

}

