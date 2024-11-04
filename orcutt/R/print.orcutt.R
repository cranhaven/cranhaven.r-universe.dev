print.orcutt <-
function (x, ...){
  # title
  cat("Cochrane-orcutt estimation for first order autocorrelation \n \n")
  
  
  # formula
  cat("Call:\n")
  print(x$call)
  
  # number of interaction
  cat("\n number of interaction:" , x$number.interaction)
  
  # rho
  cat("\n rho" , round(x$rho,6))
  
  
  #Durbin-Watson
  cat("\n\nDurbin-Watson statistic \n(original):   ", format(round(x$DW[1],5), nsmall=5),", p-value:", format(x$DW[2], scientific=TRUE, digits = 4))
  cat("\n(transformed):", format(round(x$DW[3],5), nsmall=5),", p-value:", format(x$DW[4], scientific=TRUE, digits = 4))
  
  
  # coefficients
  cat("\n \n coefficients: \n" )
  print(round(x$coefficients,6))
  
}
