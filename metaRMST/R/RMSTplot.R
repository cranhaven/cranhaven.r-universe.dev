#' Plot RMST curves in each trial and combined effects
#'
#'  
#'
#' @param RMSTobject object created by RMSTcurves 
#' @param estimates option to include meta-analysis estimates and CIs 
#' @param MA_legend option to include a legend for meta-analysis symbols
#' @param trial_legend option to include a legend for trial colors
#' @param type specify plot type (defaults to line plot) 
#' @param col option to specify vector of colors for each study
#' @param lwd option to specify line width
#' @param ylim option to specify limits for y axis
#' @param yby option to specify intervals for y axis
#' @param xlim option to specify limits for x axis
#' @param xby option to specify intervals for x axis
#' @param main option to add title
#' @param xlab option to specify x axis label
#' @param ylab option to specify y axis label
#' @description Plot the \code{RMSTcurve} object
#' @return a plot of RMSTD over time with option to add combined effect estimates and pointwise 95% confidence intervals
#' @import mvmeta meta survival survRM2
#' @importFrom graphics abline arrows axis legend points lines plot
#' @importFrom stats qnorm
#' @export

RMSTplot <- function(RMSTobject, type="l", col=c("red","blue","green","orange","purple", "yellow", "brown", "gray"),
                     lwd=2, ylim=c(-0.75,2.75), yby=0.25, xlim=c(0,36), xby=12, 
                     main="", xlab="Time (unit)", ylab="Difference in RMST (unit)", trial_legend=TRUE, MA_legend=TRUE, estimates=TRUE){
  
  J <- ncol(RMSTobject[[1]])-1 
  
  plot(RMSTobject[[1]][,1], RMSTobject[[1]][,2], type="l", col=col[1], lwd=lwd, 
       ylim=ylim, xlim=xlim,
       main=main, xlab=xlab, ylab=ylab, xaxt="n", yaxt="n")
  axis(2, at=seq(min(ylim), max(ylim), by=yby)) 
  axis(1, at=seq(min(xlim), max(xlim), by=xby))
  
  for(j in 2:J){  
    lines(RMSTobject[[1]][,1], RMSTobject[[1]][,j+1], col=col[j], lwd=2)
    # add RP lines
    lines(RMSTobject[[2]][,1], RMSTobject[[2]][,j+1], col=col[j], lty=2)
  }
  abline(h=0)
  
  # trial legend
  if (trial_legend){
    legend("topleft", legend=paste0("trial", 1:J), 
           col=col[1:J], 
           horiz=F, lty=rep(1, J), pch=rep(NA, J), 
           lwd=rep(2,J), seg.len=1, bty="n")
  }
  
  # MA method legend
  if(MA_legend){
    legend("topright", inset=c(-0.2,0), legend=c("Multivariate, analytic covariance", "Multivariate, bootstrap covariance", "Univariate, flexible parametric model estimates", "Univariate, available data" ), 
           col=c("black", "black", "black", "black"), 
           horiz=F, lty=c(1,1,1,1), pch=c(19,17,15,18), 
           lwd=c(1,1,1,1), seg.len=1, bty="n")
  }
  
  if(estimates){
    # add meta-analysis results
    for(i in 1:nrow(RMSTobject[[3]])){
      mypch <- ifelse(RMSTobject[[3]][i,1]=="Random Effect MVMA", 19, 
                      ifelse(RMSTobject[[3]][i,1]=="Random Effect MVMA boot", 17,
                             ifelse(RMSTobject[[3]][i,1]=="univariate", 18,  15)))
      
      stagger <- ifelse(RMSTobject[[3]][i,1]=="Random Effect MVMA", -0.9, 
                        ifelse(RMSTobject[[3]][i,1]=="Random Effect MVMA boot", -0.3,
                               ifelse(RMSTobject[[3]][i,1]=="univariate", 0.9,  0.3)))
      
      #mvma
      points(RMSTobject[[3]][i,2]+stagger, RMSTobject[[3]][i,4], pch=mypch, col="black")
      arrows(RMSTobject[[3]][i,2]+stagger, RMSTobject[[3]][i,6], RMSTobject[[3]][i,2]+stagger, RMSTobject[[3]][i,7], angle=90, code=3, length=0.05, col="black")
      
    }
    
  }
  
}
