#' @name summary.bcfraildv
#' @title Print bcfraildv
#' @description Generics to print the S3 class bcfraildv.
#' @details Calls \code{print.bcfraildv()}.
#'
#' @param object A class \code{bcfraildv} object.
#' @param ... ignored
#'
#' @return An object of \code{print.bcfraildv}, with some more human-readable results from \code{bcfraildv} object.
#' @export
#  (deprecated) @S3method summary bcfraildv
#' @importFrom stats printCoefmat
#' @importFrom stats pnorm
#'
#' @note The summary function is currently identical to the print function.
#' @seealso \code{\link{bcfraildv}}
#'
#' @examples
#' set.seed(4)
#' simdata<-simbcfraildv(psize=300, cenr= c(0.3),beta=c(2),frailty=c("gamma"),
#' frailpar=c(0.5,0.5,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(5), scale = c(0.1)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data
#'
#' fitbcfrail=bcfraildv(Surv(time,censor)~ X1+frailty(PID) ,data=dataa)
#' fitbcfrail
#' summary(fitbcfrail)
#'
summary.bcfraildv<- function(object, ...)
{
if(!inherits(object, "bcfraildv")){
stop("Argument must be the result of bcfraildv")}
cat("Call:\n")
print(object$call)
cat("\nn= ",length(object$censor),"and number of events=",sum(object$censor)," \n")
cat("\nRegression Coefficients:\n")
zval <- object$coefficients/object$stderr[1:length(object$coefficients)]
se=object$stderr
RTAB <- cbind(Estimate =object$coefficients,StdErr =se[1:length(object$coefficients)],
z.value =  zval,p.value = 2*(1-pnorm(abs(zval), mean=0,sd=1)))
printCoefmat(RTAB, P.values=TRUE, has.Pvalue=TRUE)
cat("\nFrailty Distribution:Bivariate Correlated ",object$frailty,"\n")
if(object$frailty==c("gamma")){
  cat("Frailty variance 1 =",object$frailparest[1],"(",se[length(se)-2],")\n")
  cat("Frailty variance 2 =",object$frailparest[2],"(",se[length(se)-1],")\n")
  cat("Correlation Estimate =",object$frailparest[3],"(",se[length(se)],")\n")}
if(object$frailty==c("lognormal")){
  cat("Variance of random effect 1 =",object$frailparest[1],"\n")
  cat("Variance of random effect 2 =",object$frailparest[2],"\n")
  cat("Correlation Estimate of random effects =",object$frailparest[3],"\n")}
cat("Log likelihood =",object$Iloglilk,"\n")
}
