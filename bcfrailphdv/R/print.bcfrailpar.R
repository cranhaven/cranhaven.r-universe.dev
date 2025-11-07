#'
#' @name print.bcfrailpar
#' @title Print bcfrailpar
#' @description Generics to print the S3 class bcfrailpar.
#' @details Calls \code{print.bcfrailpar()}.
#'
#' @param x A class \code{bcfrailpar} object.
#' @param ... ignored
#' @return An object of \code{print.bcfrailpar}, with some more human-readable results from \code{bcfrailpar} object.
#' @export
#  (deprecated) @S3method print bcfrailpar
#' @importFrom stats printCoefmat
#' @importFrom stats pnorm
#'
#' @note The summary function is currently identical to the print function.
#' @seealso \code{\link{bcfrailpar}}
#'
#' @examples
#' set.seed(4)
#' simdata<-simbcfraildv(psize=500, cenr= c(0),beta=c(-1),frailty=c("gamma"),
#' frailpar=c(0.4,0.4,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(0.9), scale = c(2)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data ## the simulated data set
#'
#' #fit
#' parbcfit=bcfrailpar(Surv(time, censor) ~ X1+frailty(PID),data=dataa)
#' parbcfit
#'
print.bcfrailpar<- function(x, ...)
{
if(!inherits(x, "bcfrailpar")){
stop("Argument must be the result of bcfrailpar")}
cat("Call:\n")
print(x$call)
cat("\nn= ",length(x$censor),"and number of events=",sum(x$censor)," \n")
if(length(x$coefficients)>0){
np=(length(x$basehazpar)+length(x$frailparest))
cat("\nRegression Coefficients:\n")
zval <- x$coefficients/x$stderr[(np+1):(np+length(x$coefficients))]
se=x$stderr
RTAB <- cbind(Estimate =x$coefficients,StdErr =se[(np+1):(np+length(x$coefficients))],
z.value =  zval,p.value = 2*(1-pnorm(abs(zval), mean=0,sd=1)))
printCoefmat(RTAB, P.values=TRUE, has.Pvalue=TRUE)}
se=x$stderr
cat("\nFrailty Distribution:Bivariate Correlated ",x$frailty," with ",x$haz," baseline\n")
if(x$comonvar){
cat("Frailty variance =",x$frailparest[1],"(",se[1],")\n")
cat("Correlation Estimate =",x$frailparest[2],"(",se[2],")\n")
if(x$frailty=="pv"){
cat("alpha parameter =",x$alpha[1],"(",x$sealpha,")\n")}
if(x$haz==c("weibull")){
cat("Scale parameter =",x$basehazpar[1],"(",se[3],")\n")
cat("Shape parameter =",x$basehazpar[2],"(",se[4],")\n")}
if(x$haz==c("gompertz")){
cat("Scale parameter =",x$basehazpar[1],"(",se[3],")\n")
cat("Shape parameter =",x$basehazpar[2],"(",se[4],")\n")}
if(x$haz==c("exponential")){
cat("Scale parameter =",x$basehazpar,"(",se[3],")\n")}}else{
cat("Frailty variance 1=",x$frailparest[1],"(",se[1],")\n")
cat("Frailty variance 2 =",x$frailparest[2],"(",se[2],")\n")
cat("Correlation Estimate =",x$frailparest[3],"(",se[3],")\n")
if(x$haz==c("weibull")){
cat("Scale parameter =",x$basehazpar[1],"(",se[4],")\n")
cat("Shape parameter =",x$basehazpar[2],"(",se[5],")\n")}
if(x$haz==c("gompertz")){
cat("Scale parameter =",x$basehazpar[1],"(",se[4],")\n")
cat("Shape parameter =",x$basehazpar[2],"(",se[5],")\n")}
if(x$haz==c("exponential")){
cat("Scale parameter =",x$basehazpar,"(",se[4],")\n")}}
cat("Log likelihood =",x$loglik,"\n")
cat("AIC =",x$AIC,"\n")
cat("BIC =",x$BIC,"\n")
}


