print.summary.orcutt <-
function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  cat("\n")
  printCoefmat(x$coefficients, P.values=TRUE, has.Pvalue=TRUE, signif.stars = getOption("show.signif.stars"),
               signif.legend = TRUE)
  
  cat("\nResidual standard error:", x$stat[1],"on", x$stat[2] ,"degrees of freedom")
  cat("\nMultiple R-squared: ", x$stat[3], ",  Adjusted R-squared: ",  x$stat[4])
  cat("\nF-statistic:", x$stat[5] , "on", x$stat[6] ,"and", x$stat[2] ,"DF,  p-value: <", 
      format(x$stat[7], scientific=TRUE, digits = 4))
  
  #Durbin-Watson
  cat("\n\nDurbin-Watson statistic \n(original):   ", format(round(x$DW.t[1],5), nsmall=5),", p-value:", format(x$DW.t[2], scientific=TRUE, digits = 4))
  cat("\n(transformed):", format(round(x$DW.t[3],5), nsmall=5),", p-value:", format(x$DW.t[4], scientific=TRUE, digits = 4))
  
}
