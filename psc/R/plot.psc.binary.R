#' Function for Plotting PSC objects
#'
#' A function which illsutrates the predicted response under the counter factual
#' model and the observed response under the experimental treatment(s).
#'
#' @param pscOb an object of class 'psc'
#' @param ... not used
#' @return a survival plot corresponding to the psc fit
#' @details This function plots the expected response of the control treatment
#'    along with the observe response rates of the experimental arms
#' @import ggplot2
plot.psc.binary <- function(pscOb,...){

  # binding variables
  pr_data <- Outcome <- lp <-  NULL

  fam <- pscOb$family;fam
  out <- pscOb$DC$Y;out

  ## defining treatment (for multiple treatment comparisons)
  #mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  #trt <- rep(1,nrow(x$DC_clean$cov))
  #if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])
  #new.mn <- tapply(out,trt,mean)
  #length(new.mn)

  pr_cont <- cfmSumm.glm(pscOb)
  pr_data <- data.frame("lp"=pr_cont$lp)
  new.mn <- mean(out)

  if(!is.null(pscOb$DC$trt)){
    new.mn <- tapply(out,pscOb$DC$trt,mean,na.rm=T)
  }

  ## Plot
  ggplot(aes(lp),data=pr_data)+
    geom_density(linewidth=1.3)+
    xlim(c(0,1)) +
    xlab("Pr(Response)")+
    ylab("Density") +
    geom_vline(aes(xintercept=mean(lp)),col=3,linetype="dashed",linewidth=1.2)+
    geom_vline(xintercept=new.mn,col=c(4:(length(new.mn)+3)),linewidth=1.2)

}

