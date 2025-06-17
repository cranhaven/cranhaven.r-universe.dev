print.PolyTrend <-
function(x, ...) {
  
  cat("\nPolyTrend input data:\n")
  cat(sprintf("\nY: %s\n", paste(x$Y,collapse=" ")))
  cat(sprintf("alpha: %.2f\n", x$alpha))
  
  cat("\nPolyTrend classification:\n")
  
  strTrendType <-c("concealed", "no trend", "linear", "quadratic", "cubic")
  cat(sprintf("\nTrend type: %i (%s)\n", x$TrendType, strTrendType[x$TrendType+2] ))
  
  cat(sprintf("Slope: %.4f\n", x$Slope))
  
  strDirection <- "positive"
  if(x$Direction < 0) strDirection <- "negative"
  cat(sprintf("Direction: %i (%s)\n", x$Direction, strDirection))
  
  strSignificance <- "statistically significant"
  if(x$Significance < 0) strSignificance <- "statistically insignificant"
  cat(sprintf("Significance: %i (%s)\n", x$Significance, strSignificance))
  
  strPolynomialDegree <-c("no trend", "linear", "quadratic", "cubic")
  cat(sprintf("Polynomial degree: %i (%s)\n", x$PolynomialDegree, strPolynomialDegree[x$PolynomialDegree+1]))
  
  invisible(x)
}
