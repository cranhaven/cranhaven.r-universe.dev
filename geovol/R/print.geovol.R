print.geovol <- function (x, n.extreme = 20, ...)
{
  
  cat("\n")
  cat("\n")
  cat("Date:", x$date, "\n")
   cat("Model: GEOVOL \n")
  cat("Method: maximization-maximization (algorithm stopped after", x$iter, "iterations) \n")
  cat("No. of observations:", x$n, "\n")
  cat("No. of time series:", x$m, "\n")
  cat("\n")  
  cat("Estimated GEOVOL factor (", sep = "",n.extreme," most extreme values): \n")
  cat("\n")  
  x_ordered <- x$x[order(x$x, decreasing = TRUE),]
  x.extreme <- as.matrix(x_ordered[1:n.extreme])
  rownames(x.extreme) <- paste((x$dates[order(x$x, decreasing = TRUE)])[1:n.extreme])
  colnames(x.extreme) <- "GEOVOL"
  print(round(x.extreme, 4))
  cat("\n")
  cat("Estimated GEOVOL loadings: \n")
  cat("\n")  
  print(round((x$s), 4))
  cat("\n")
  
}
