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
plot.psc.cont <- function(x, ...){

  # Binding local varaibles
  y <- xn <- yn <- Outcome  <- linPred <-  NULL

  fam <- x$DC_clean$model_extract$family;fam
  out <- as.numeric(unlist(x$DC_clean$out));out

  ## defining treatment (for multiple treatment comparisons)
  mtc.cond <- "trt"%in%colnames(x$DC_clean$cov)
  trt <- rep(1,nrow(x$DC_clean$cov))
  if(mtc.cond) trt <- factor(x$DC_clean$cov[,which(colnames(x$DC_clean$cov)=="trt")])

  new.mn <- tapply(out,trt,mean)

  pr_cont <- linPred(x$DC_clean,resp=T)
  pr_data <- data.frame(pr_cont)
  pr_data$Outcome <-"CFM"

  pr_data
  den <- density(pr_data$pr_cont)
  den.out <- density(out)
  den <- data.frame("x"=den$x,"y"=den$y,"xn"=den.out$x,"yn"=den.out$y)

  ggplot(aes(x,y),data=data.frame(den))+
    geom_line(linewidth=1.5,col=2)+
    geom_line(aes(xn,yn),color="darkorchid",linewidth=1.5)+
    xlab("Response")+
    ylab("Frequency") +

    geom_vline(aes(xintercept=mean(pr_cont),colour=Outcome),data=pr_data,linetype="dashed",linewidth=1.2) +
    geom_vline(xintercept=new.mn,col=c(3:(length(new.mn)+2)),linewidth=1.2)

}

