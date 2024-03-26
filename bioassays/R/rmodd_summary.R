#'@name rmodd_summary
#'@aliases rmodd_summary
#'@title Summarise a Numerical Vector with Control on Outlier Removal
#'
#'@description Summarise a numerical vector with control on how the outliers has to be treated.
#'
#' @usage rmodd_summary(x, rm = "FALSE", strict = "FALSE", cutoff = 80, n = 3)
#'
#' @param x numerical vector
#'
#' @param rm if rm = "TRUE" outliers are omitted. If rm = "FALSE" all elements in the vector are considered for summarising
#'
#' @param strict if strict = "FALSE" outliers are omitted based on IQR rule. If strict = "TRUE" more aggressive outlier omitting method is used to bring CV below a cutoff value
#'
#'@param cutoff cv cutoff value for the aggressive outlier removal
#'
#'@param n minimum number of samples needed
#'
#'@details In IQR rule (ie when strict = "FALSE") those values above 'Q3 + 1.5 IQR' and those below 'Q1 - 1.5 IQR' is considered as outlier. For the aggressive outlier removal (ie when strict = "TRUE") those values above 90th percentile and  below 10th percentile are removed consecutively till the cv fall below the 'cutoff' or only the minimum number of samples is leftover (whichever happens first halt the loop).
#'
#'@return A  numeric vector of length 5 with the elements representing
#'\item{mean }{the average of samples}
#'\item{median }{the median of samples}
#'\item{n }{number of samples}
#'\item{sd }{standard deviation of samples}
#'\item{cv }{percentage cv of samples}
#'
#'@author A.A Palakkan
#'
#'@examples
#'## data set x
#'x <- c(1.01,0.98,0.6,0.54,0.6,0.6,0.4,3)
#'
#'## summarising without removing outliers
#'rmodd_summary(x, rm = "FALSE", strict= "FALSE", cutoff=80, n=3)
#'
#'## summarising after removing outliers (IQR methord)
#'rmodd_summary(x, rm = "TRUE", strict= "FALSE", cutoff=20, n=5)
#'
#'## summarising after removing outliers (Stringent to reduce cv)
#'rmodd_summary(x, rm = "TRUE", strict= "TRUE", cutoff=20, n=5)
#'
#'@keywords arith
#'
#'@importFrom stats median sd quantile IQR
#'
#'@export
#'
#'

rmodd_summary <- function (x, rm = "FALSE", strict= "FALSE", cutoff=80,n=3 ) {

## intial setup ###
z1<-c(mean(x),stats::median(x),length(x),stats::sd(x),stats::sd(x)/mean(x)*100)

## rm == FALSE ###
if(rm=="FALSE"){names(z1)<-c("mean","median","n","sd","cv")
return(z1)}

## rm == TRUE & strict ==FALSE, cv < cutoff ###
if(rm=="TRUE"){
  if(z1[5]< cutoff & strict == "FALSE") {names(z1)<-c("mean","median","n","sd","cv")
  return(z1)}

## rm == TRUE & strict ==FALSE, cv > cutoff ###
  if(z1[5]> cutoff & strict == "FALSE") {
    Q <- stats::quantile(x,probs=c(0.25,0.75),na.rm=FALSE)
    iqr <- stats::IQR(x)
    up <-  Q[2]+1.5*iqr # Upper Range
    low<- Q[1]-1.5*iqr # Lower Range
    x<- subset(x, (x > low & x < up))
    z1<-c(mean(x),stats::median(x),length(x),stats::sd(x),stats::sd(x)/mean(x)*100)
    names(z1)<-c("mean","median","n","sd","cv")
    return(z1)
    }

## rm == TRUE & strict ==TRUE, cv > cutoff ###
  zn<-z1
  cycle<-0
  while (abs(z1[5])> cutoff & strict == "TRUE") {
    cycle<-cycle+1
    up_P<-1-0.1*cycle
    low_P<-0.1*cycle
    temp1<-x[which(!(x> stats::quantile(x,probs=up_P)))]
    z2<-c(mean(temp1),stats::median(temp1),length(temp1),stats::sd(temp1)/mean(temp1)*100)
    d2<-abs(zn[2]-z2[2])

    temp2<-x[which(!(x < stats::quantile(x,probs=low_P)))]
    z3<-c(mean(temp2),stats::median(temp2),length(temp2),stats::sd(temp2)/mean(temp2)*100)
    d3<-abs(zn[2]-z3[2])

    if (d2<d3) {out<-which(!(x < stats::quantile(x,probs=low_P) | x >= stats::quantile(x,probs=up_P)))}
    if(d2>d3){out<-which(!(x <= stats::quantile(x,probs=low_P) | x > stats::quantile(x,probs=up_P)))}
    if (d2==d3){out<-which(!(x < stats::quantile(x,probs=low_P) | x > stats::quantile(x,probs=up_P)))}

    x<-x[out]
    z1new<-c(mean(x),stats::median(x),length(x),stats::sd(x),stats::sd(x)/mean(x)*100)
    if(z1new[3]<=n){break}
    z1<-z1new
    names(z1)<-c("mean","median","n","sd","cv")
  }
  return(z1)
}

}




