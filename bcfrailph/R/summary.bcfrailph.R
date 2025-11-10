#' @name summary.bcfrailph
#' @title Print bcfrailph
#' @description Generics to print the S3 class bcfrailph.
#' @details Calls \code{print.bcfrailph()}.
#'
#' @param object A class \code{bcfrailph} object.
#' @param ... ignored
#'
#' @return An object of \code{summary.bcfrailph}, with some more human-readable results from \code{bcfrailph} object.
#' @export
#  (deprecated) @S3method summary bcfrailph
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
#' summary(fitbcfrailph)
#'
summary.bcfrailph<- function(object, ...)
{
if(!inherits(object, "bcfrailph")){
stop("Argument must be the result of bcfrailph")}
cat("Call:\n")
print(object$call)
cat("\nn= ",length(object$censor),"and number of events=",sum(object$censor)," \n")
cat("\nRegression Coefficients:\n")
zval <- object$coefficients/object$stderr[1:length(object$coefficients)]
se=object$stderr
RTAB <- cbind(Estimate =object$coefficients,StdErr =se[1:length(object$coefficients)],
se2= c(sqrt(diag(object$vcov2))),z.value =  zval,p.value = 2*(1-pnorm(abs(zval), mean=0,sd=1)))
printCoefmat(RTAB, P.values=TRUE, has.Pvalue=TRUE)
cat("\nFrailty Distribution:Bivariate Correlated ",object$frailty,"\n")
if(object$frailty==c("gamma")){
if(length(object$weights)==0){
cat("Frailty variance =",object$frailparest[1],"(",se[length(se)-1],")\n")
cat("Correlation Estimate =",object$frailparest[2],"(",se[length(se)],")\n")}
if(length(object$weights)>0){
cat("Frailty variance =",object$frailparest[1],"\n")
cat("Correlation Estimate =",object$frailparest[2],"\n")}}
if(object$frailty==c("lognormal")){
cat("Variance of random effect =",object$frailparest[1],"\n")
cat("Correlation Estimate of random effects =",object$frailparest[2],"\n")}
cat("Log likelihood with frailty =",object$Iloglilk,"\n")
cat("Log likelihood without frailty=",object$loglilk0,"\n")
}

