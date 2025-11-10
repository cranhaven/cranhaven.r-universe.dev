#'
#' @name summary.shrgamsp
#' @title Print shrgamsp
#' @description Generics to print the S3 class shrgamsp.
#' @details Calls \code{print.shrgamsp()}.
#'
#' @param object A class \code{shrgamsp} object.
#' @param ... ignored
#' @return An object of \code{summary.shrgamsp}, with some more human-readable results from \code{shrgamsp} object.
#' @export
#  (deprecated) @S3method summary shrgamsp
#' @importFrom stats printCoefmat
#' @importFrom stats pnorm
#'
#' @note The summary function is currently identical to the print function.
#' @seealso \code{\link{bcfrailph}}
#'
summary.shrgamsp<- function(object, ...)
{
if(!inherits(object, "shrgamsp")){
stop("Argument must be the result of shrgamsp")}
cat("Call:\n")
print(object$call)
cat("\nn= ",length(object$censor),"and number of events=",sum(object$censor)," \n")
cat("\nRegression Coefficients:\n")
zval <- object$coefficients/object$stderr[1:length(object$coefficients)]
se=object$stderr
RTAB <- cbind(Estimate =object$coefficients,StdErr =se[1:length(object$coefficients)],
z.value =  zval,p.value = 2*(1-pnorm(abs(zval), mean=0,sd=1)))
printCoefmat(RTAB, P.values=TRUE, has.Pvalue=TRUE)
cat("\nFrailty Distribution:shared gamma model\n")
if(length(object$fittype)>0){
cat("Frailty variance =",object$frailparest,"(",se[length(se)],")\n")}
if(length(object$fittype)==0){
cat("Frailty variance =",object$frailparest,"\n")}
cat("Log likelihood with frailty =",object$Iloglilk,"\n")
cat("Log likelihood without frailty=",object$loglik0,"\n")
}
