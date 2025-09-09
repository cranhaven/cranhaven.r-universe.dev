#' Function for Plotting PSC objects
#' #' A function which illsutrates the predicted response under the counter factual
#' model and the observed response under the experimental treatment(s).
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return a survival plot corresponding to the psc fit
#' @details This function plots the expected response of the control treatment
#'    along with the observe response rates of the experimental arms
#' @import ggplot2
#' @examples
#' count.mod <- psc::count.mod
#' data <- psc::data
#' count.psc <- pscfit(count.mod,data,nsim=3000)
#' plot(count.psc)
#' @export
plot.psc.count <- function(x, ...){

  # Binding local varaibles
  pr_data <- den <- Outcome <- y <- NULL

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

  den <- density(pr_data$pr_cont)
  den <- data.frame("x"=den$x,"y"=den$y)
  infl <- max(table(out))/max(den$y)
  den$y <- den$y*infl

  ggplot(aes(out),data=data.frame(out))+
    geom_bar(col="gray",fill="gray")+
    geom_line(aes(x,y),data=den,linewidth=1.5,col="darkorchid")+
    xlab("Response/Pr(Response)")+
    ylab("Frequency") +
    geom_vline(aes(xintercept=mean(pr_cont),colour=Outcome),data=pr_data,linetype="dashed",linewidth=1.2) +
    geom_vline(xintercept=new.mn,col=c(3:(length(new.mn)+2)),linewidth=1.2)

}
