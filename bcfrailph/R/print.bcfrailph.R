#'
#' @name print.bcfrailph
#' @title Print bcfrailph
#' @description Generics to print the S3 class bcfrailph.
#' @details Calls \code{print.bcfrailph()}.
#'
#' @param x A class \code{bcfrailph} object.
#' @param ... ignored
#' @return An object of \code{print.bcfrailph}, with some more human-readable results from \code{bcfrailph} object.
#' @export
#  (deprecated) @S3method print bcfrailph
#' @importFrom stats printCoefmat
#' @importFrom stats pnorm
#'
#' @note The summary function is currently identical to the print function.
#' @seealso \code{\link{bcfrailph}}
#'
#' @examples
#' set.seed(4)
#' simdata<-simbcfrailph(psize=300, cenr= c(0.3),beta=c(2),frailty=c("gamma"),
#' frailpar=c(0.5,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(5), scale = c(0.1)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data
#'
#' fitbcfrailph=bcfrailph(Surv(time,censor)~ X1+frailty(PID) ,data=dataa,frail_distrn=c("gamma"))
#' fitbcfrailph
#'
print.bcfrailph<- function(x, ...)
{
if(!inherits(x, "bcfrailph")){
stop("Argument must be the result of bcfrailph")}
cat("Call:\n")
print(x$call)
cat("\nn= ",length(x$censor),"and number of events=",sum(x$censor)," \n")
cat("\nRegression Coefficients:\n")
zval <- x$coefficients/x$stderr[1:length(x$coefficients)]
se=x$stderr
RTAB <- cbind(Estimate =x$coefficients,StdErr =se[1:length(x$coefficients)],
z.value =  zval,p.value = 2*(1-pnorm(abs(zval), mean=0,sd=1)))
printCoefmat(RTAB, P.values=TRUE, has.Pvalue=TRUE)
if(x$frailty==c("gamma")){
cat("\nFrailty Distribution:Bivariate correlated gamma\n")
cat("Frailty variance =",x$frailparest[1],"(",se[length(se)-1],")\n")
cat("Correlation Estimate =",x$frailparest[2],"(",se[length(se)],")\n")
cat("Log likelihood with frailty =",x$Iloglilk,"\n") }
if(x$frailty==c("lognormal")){
cat("\nFrailty Distribution:Bivariate correlated log-normal\n")
cat("Variance of random effect =",x$frailparest[1],"(",se[length(se)-1],")\n")
cat("Correlation Estimate of random effects =",x$frailparest[2],"(",se[length(se)],")\n")
cat("Approximated Log likelihood with frailty =",x$Iloglilk,"\n")}
cat("Log likelihood without frailty=",x$loglilk0,"\n")
}
