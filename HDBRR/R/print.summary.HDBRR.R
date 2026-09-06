
#' @rdname print
#' @export

print.summary.HDBRR<- function(x, ...){
  if(!inherits(x, "summary.HDBRR")) stop("This function only works for objects of class 'HDBRR'\n");
  model <- x$call
  summary <- x$summary
  lambda <- x$lambda
  edf <- x$edf
  cat("\nCall:\n", paste(deparse(model), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat("\nCoefficients:\n")
  print(summary)
  cat("-----\n")
  cat("Signif. codes: 10 '***' 6 '**' 2 '*' 0 ' ' \n\n")
  cat("\nRidge parameter:", lambda)
  cat("\nEffective degrees of freedom:", edf)
}
