#'
#' @name print.shrgamsp
#' @title Print shrgamsp
#' @description Generics to print the S3 class shrgamsp.
#' @details Calls \code{print.shrgamsp()}.
#'
#' @param x A class \code{shrgamsp} object.
#' @param ... ignored
#' @return An object of \code{print.shrgamsp}, with some more human-readable results from \code{shrgamsp} object.
#' @export
#  (deprecated) @S3method print shrgamsp
#' @importFrom stats printCoefmat
#' @importFrom stats pnorm
#'
#' @note The summary function is currently identical to the print function.
#' @seealso \code{\link{bcfrailph}}
#'
print.shrgamsp<- function(x, ...)
{
if(!inherits(x, "shrgamsp")){
stop("Argument must be the result of shrgamsp")}
cat("Call:\n")
print(x$call)
cat("\nn= ",length(x$censor),"and number of events=",sum(x$censor)," \n")
cat("\nRegression Coefficients:\n")
zval <- x$coefficients/x$stderr[1:length(x$coefficients)]
se=x$stderr
RTAB <- cbind(Estimate =x$coefficients,StdErr =se[1:length(x$coefficients)],
z.value =  zval,p.value = 2*(1-pnorm(abs(zval), mean=0,sd=1)))
printCoefmat(RTAB, P.values=TRUE, has.Pvalue=TRUE)
cat("\nFrailty Distribution:shared gamma model\n")
if(length(x$fittype)>0){
cat("Frailty variance =",x$frailparest,"(",se[length(se)],")\n")}
if(length(x$fittype)==0){
cat("Frailty variance =",x$frailparest,"\n")}
cat("Log likelihood with frailty =",x$Iloglilk,"\n")
cat("Log likelihood without frailty=",x$loglik0,"\n")
}
