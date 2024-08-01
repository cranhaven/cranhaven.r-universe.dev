plot.AROC <-
function(x, ...) {

	main.roc <- switch(class(x)[2], "pooledROC.BB" = "Pooled ROC curve - Bayesian bootstrap", "pooledROC.emp" = "Pooled ROC curve - Empirical", "AROC.kernel" = "AROC Kernel-based", "AROC.bnp" = "AROC Bayesian nonparametric", "AROC.bsp" = "AROC Bayesian semiparametric", "AROC.sp" = "AROC semiparametric")

	main.auc <- ifelse(any(class(x) %in% c("pooledROC.BB", "pooledROC.emp")), "AUC", "AAUC")

	plot(x$p, x$ROC[,1], xlab = "FPF", ylab = "TPF", xlim = c(0,1), ylim = c(0,1), main = main.roc, type = "l", cex.lab = 1.3, cex.axis = 1.3,...)
	if(ncol(x$ROC) == 3) {
		lines(x$p, x$ROC[,2], lty = 2)
		lines(x$p, x$ROC[,3], lty = 2)
	}
	abline(0,1, col = "grey")
	if(length(x$AUC) == 3) {
		legend.text <- paste0(main.auc, ": ", paste(round(x$AUC[1], 3), " (", round(x$AUC[2], 3),"",", ", round(x$AUC[3], 3),")", sep = ""))
	} else {
		legend.text <- paste0(main.auc, ": ", round(x$AUC[1], 3))
	}
	legend(0.4, 0.2, legend.text, bty = "n", cex = 1.3)

}
