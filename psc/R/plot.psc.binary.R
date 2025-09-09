#' Function for Plotting PSC objects
#'
#' A function which illsutrates the predicted response under the counter factual
#' model and the observed response under the experimental treatment(s).
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return a survival plot corresponding to the psc fit
#' @details This function plots the expected response of the control treatment
#'    along with the observe response rates of the experimental arms
#' @import ggplot2
#' @examples
#' bin.mod <- psc::bin.mod
#' data <- psc::data
#' bin.psc <- pscfit(bin.mod,data,nsim=3000)
#' plot(bin.psc)
#' @export
plot.psc.binary <- function(x,...){

  # binding variables
  pr_data <- Outcome <-  NULL

  fam <- x$DC_clean$model_extract$family;fam
  out <- as.numeric(unlist(x$DC_clean$out));out

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])

  new.mn <- tapply(out,trt,mean)
  length(new.mn)

  pr_cont <- linPred(x$DC_clean,resp=T)
  pr_data <- data.frame(pr_cont)
  pr_data$Outcome <-"CFM"

  ggplot(aes(pr_cont,colour=Outcome),data=pr_data)+
    geom_density(linewidth=1.3)+
    xlim(c(0,1)) +
    xlab("Pr(Response)")+
    ylab("Density") +
    geom_vline(aes(xintercept=mean(pr_cont),colour=Outcome),linetype="dashed",linewidth=1.2)+
    geom_vline(xintercept=new.mn,col=c(3:(length(new.mn)+2)),linewidth=1.2)

}
