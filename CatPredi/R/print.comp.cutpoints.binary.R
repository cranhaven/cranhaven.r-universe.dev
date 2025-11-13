print.comp.cutpoints.binary <-
function(x, digits = 4, ...) {
	  
	cat("\n\n*************************************************\n")
  cat("Compare optimal number of cut points")
	cat("\n*************************************************\n\n")
	cat(paste("Bias corrected AUC difference:", round(x$AUC.cor.diff, 4), sep="  "), fill=TRUE)
	cat(paste("95% Bootstrap Confidence Interval:","(",round(x$icb.auc.diff[1], 4),",", round(x$icb.auc.diff[2], 4), ")" ,sep=" "), fill=TRUE)
  invisible(x)
}
