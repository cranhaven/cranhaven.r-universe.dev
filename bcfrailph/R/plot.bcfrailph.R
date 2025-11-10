#'
#' @name plot.bcfrailph
#' @title Plot bcfrailph
#' @description Generics to print the S3 class bcfrailph.
#' @details Calls \code{plot.bcfrailph()}.
#'
#' @param x A class \code{bcfrailph} object.
#' @param lty Line type line type 1 is a solid line (the default).
#' @param col Colors to be used for points.
#' @param type The type of plot produced. type="l" Plot lines (the default) and type="p" Plot individual points. 
#' @param xlim range of variable on the x axis.
#' @param ylim range of variable on the y axis.
#' @param xlab Axis label for the x axis.
#' @param conf.int whether confidence interval is included in the plot the deafault is FALSE.
#' @param main main is a string for figure title, placed at the top of the plot in a large font.
#' @param ... ignored
#' @return An plot of  \code{plot.bcfrailph} object.
#' @export
#  (deprecated) @S3method plot bcfrailph
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @note The plot of cumulative baseline hazard function.
#' @seealso \code{\link{bcfrailph}}
#'
#' @examples
#' set.seed(24)
#' simdata<-simbcfrailph(psize=100, cenr= c(0),beta=c(-1),frailty=c("gamma"),
#' frailpar=c(0.4,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(0.9), scale = c(2)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data ## the generated data set.
#'
#' #fit
#' bcfit=bcfrailph(Surv(time, censor) ~ X1+frailty(PID),data=dataa)
#' plot(bcfit)
#'

plot.bcfrailph<- function(x,lty=1,col=1,type = "l",xlim=NULL,
ylim=NULL,xlab=NULL,main=NULL,conf.int=FALSE,...){
if(!inherits(x, "bcfrailph")){
stop("Argument must be the result of bcfrailph")}
t0=x$bhaz[,1]
H0=cumsum(x$bhaz[,2])
if(conf.int){
se=sqrt(cumsum((x$bhaz[,3])^2))
lci=H0-1.96*se;uci=H0+1.96*se
lw=pmin(0,min(lci))
if(length(xlim)==0){xlim=c(min(t0),max(t0))}
if(length(ylim)==0){ylim=c(lw,max(uci))}
if(length(xlab)==0){xlab = "time"}
plot(t0,H0,lty=lty,col=col,type =type ,xlim=xlim,
ylim=ylim,xlab=xlab,ylab="Cumulative baseline hazard",main=main)
lines(x = t0, y = uci, type = "s", lty = 1, col = "blue")
lines(x = t0, y = lci, type = "s", lty = 1, col = "blue")}else{
if(length(xlim)==0){xlim=c(min(t0),max(t0))}
if(length(ylim)==0){ylim=c(min(H0),max(H0))}
if(length(xlab)==0){xlab = "time"}
plot(t0,H0,lty=lty,col=col,type =type ,xlim=xlim,
ylim=ylim,xlab=xlab,ylab="Cumulative baseline hazard",main=main)}
}
