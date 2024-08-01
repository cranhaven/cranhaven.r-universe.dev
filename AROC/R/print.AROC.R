print.AROC <- 
function(x, ...) {
	method <- switch(class(x)[2], "pooledROC.BB" = "Pooled ROC curve - Bayesian bootstrap", "pooledROC.emp" = "Pooled ROC curve - Empirical", "AROC.kernel" = "AROC Kernel-based", "AROC.bnp" = "AROC Bayesian nonparametric", "AROC.bsp" = "AROC Bayesian semiparametric", "AROC.sp" = "AROC semiparametric")

	cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
	cat(paste0("\nApproach: ", method))
	cat("\n----------------------------------------------\n")

	auc_aauc <- ifelse(any(class(x) %in% c("pooledROC.BB", "pooledROC.emp")), "Area under the pooled ROC curve", "Area under the covariate-adjusted ROC curve")
	if(length(x$AUC) == 3) {
		legend.text <- paste0(auc_aauc, ": ", paste(round(x$AUC[1], 3), " (", round(x$AUC[2], 3),"",", ", round(x$AUC[3], 3),")", sep = ""))
	} else {
		legend.text <- paste0(auc_aauc, ": ", round(x$AUC[1], 3))
	}
	cat(paste0(legend.text,"\n"))

	if(!is.null(x$pAUC)) {
		p_auc_aauc <- ifelse(any(class(x) %in% c("pooledROC.BB", "pooledROC.emp")), "Partial area under the pooled ROC curve", "Partial area under the covariate-adjusted ROC curve")
		p_auc_aauc <- paste0(p_auc_aauc, " (FPF = ", attr(x$pAUC, "value"), ")")

		if(length(x$AUC) == 3) {
		legend.text <- paste0(p_auc_aauc, ": ", paste(round(x$pAUC[1], 3), " (", round(x$pAUC[2], 3),"",", ", round(x$pAUC[3], 3),")", sep = ""))
		} else {
			legend.text <- paste0(p_auc_aauc, ": ", round(x$pAUC[1], 3))
		}
		cat(paste0(legend.text,"\n"))
	}
	waic <- any(class(x) %in% c("AROC.bnp", "AROC.bsp")) & !is.null(x$WAIC)
	lpml <- any(class(x) %in% c("AROC.bnp", "AROC.bsp")) & !is.null(x$lpml)

	if(waic | lpml) {

		cat("\n\nModel selection criteria - Healthy population")
		cat("\n----------------------------------------------\n")
		if(waic) {
			cat(paste("Widely applicable information criterion (WAIC): ", round(x$WAIC, 3),"\n"))		
		}

		if(lpml) {
			cat(paste("Pseudo marginal likelihood (LPML): ", round(x$lpml$lpml),"\n"))		
		}
	}
	invisible(x)   
}
