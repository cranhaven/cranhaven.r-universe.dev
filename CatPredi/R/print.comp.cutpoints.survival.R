print.comp.cutpoints.survival <-
function(x, digits = 4, ...) { 
  cat("\nCall:\n")
	print(x$call)    
	cat("\n\n*************************************************\n")
  cat("Compare optimal number of cut points")
	cat("\n*************************************************\n\n")
	cat(paste("Bias corrected concordance difference:", round(x$CI.cor.diff, 4), sep="  "), fill=TRUE)
	cat(paste("95% Bootstrap Confidence Interval:","(",round(x$icb.CI.diff[1], 4),",", round(x$icb.CI.diff[2], 4), ")" ,sep=" "), fill=TRUE)
  invisible(x)
}
