plot.catpredi.survival <-
function(x, ...){
	# Fit the model      
	formula <- update(x$formula, as.formula(paste("~ . + pspline(", x$cat.var, ")", sep = "")))
	formula <- as.formula(Reduce(paste, deparse(formula)))
	fit.survival <- coxph(formula, data = x$data)
	pos <- attr(terms.formula(formula, specials = c("pspline")),"specials")$pspline - 1
	termplot(fit.survival, terms = pos, se = TRUE, ylabs = paste0("f(",x$cat.var,")"))
	abline(v = x$results$cutpoints, lty = 2)
}
