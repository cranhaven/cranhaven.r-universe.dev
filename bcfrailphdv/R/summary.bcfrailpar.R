#' @name summary.bcfrailpar
#' @title Print bcfrailpar
#' @description Generics to print the S3 class bcfrailpar.
#' @details Calls \code{print.bcfrailpar()}.
#'
#' @param object A class \code{bcfrailpar} object.
#' @param ... ignored
#'
#' @return An object of \code{summary.bcfrailpar}, with some more human-readable results from \code{bcfrailpar} object.
#' @export
#  (deprecated) @S3method summary bcfrailpar
#' @importFrom stats printCoefmat
#' @importFrom stats pnorm
#'
#' @note The summary function is currently identical to the print function.
#' @seealso \code{\link{bcfrailpar}}
#'
#' @examples
#' set.seed(40)
#' simdata<-simbcfraildv(psize=500, cenr= c(0.3),beta=c(-1),frailty=c("gamma"),
#' frailpar=c(0.4,0.4,0.5),bhaz=c("gompertz"),
#' bhazpar=list(shape =c(0.09), scale = c(0.2)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data
#'
#' fitbcfrail=bcfrailpar(Surv(time,censor)~ X1+frailty(PID) ,
#' data=dataa,haz="gompertz")
#' fitbcfrail
#' summary(fitbcfrail)
#'
summary.bcfrailpar<- function(object, ...)
{
if(!inherits(object, "bcfrailpar")){
stop("Argument must be the result of bcfrailpar")}
cat("Call:\n")
print(object$call)
cat("\nn= ",length(object$censor),"and number of events=",sum(object$censor)," \n")
if(length(object$coefficients)>0){
np=(length(object$basehazpar)+length(object$frailparest))
cat("\nRegression Coefficients:\n")
zval <- object$coefficients/object$stderr[(np+1):(np+length(object$coefficients))]
se=object$stderr
RTAB <- cbind(Estimate =object$coefficients,StdErr =se[(np+1):(np+length(object$coefficients))],
z.value =  zval,p.value = 2*(1-pnorm(abs(zval), mean=0,sd=1)))
printCoefmat(RTAB, P.values=TRUE, has.Pvalue=TRUE)}
se=object$stderr
cat("\nFrailty Distribution:Bivariate Correlated ",object$frailty," with ",object$haz," baseline\n")
if(object$comonvar){
cat("Frailty variance =",object$frailparest[1],"(",se[1],")\n")
cat("Correlation Estimate =",object$frailparest[2],"(",se[2],")\n")
if(object$frailty=="pv"){
cat("alpha parameter =",object$alpha[1],"(",object$sealpha,")\n")}
if(object$haz==c("weibull")){
cat("Scale parameter =",object$basehazpar[1],"(",se[3],")\n")
cat("Shape parameter =",object$basehazpar[2],"(",se[4],")\n")}
if(object$haz==c("gompertz")){
cat("Scale parameter =",object$basehazpar[1],"(",se[3],")\n")
cat("Shape parameter =",object$basehazpar[2],"(",se[4],")\n")}
if(object$haz==c("exponential")){
cat("Scale parameter =",object$basehazpar,"(",se[3],")\n")}}else{
cat("Frailty variance 1=",object$frailparest[1],"(",se[1],")\n")
cat("Frailty variance 2 =",object$frailparest[2],"(",se[2],")\n")
cat("Correlation Estimate =",object$frailparest[3],"(",se[3],")\n")
if(object$haz==c("weibull")){
cat("Scale parameter =",object$basehazpar[1],"(",se[4],")\n")
cat("Shape parameter =",object$basehazpar[2],"(",se[5],")\n")}
if(object$haz==c("gompertz")){
cat("Scale parameter =",object$basehazpar[1],"(",se[4],")\n")
cat("Shape parameter =",object$basehazpar[2],"(",se[5],")\n")}
if(object$haz==c("exponential")){
cat("Scale parameter =",object$basehazpar,"(",se[4],")\n")}}
cat("Log likelihood =",object$loglik,"\n")
cat("AIC =",object$AIC,"\n")
cat("BIC =",object$BIC,"\n")
}

