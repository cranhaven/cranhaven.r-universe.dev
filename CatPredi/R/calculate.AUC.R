calculate.AUC <-
function(points, formula, cat.var, data.f, range, min.p.cat = 1) {
	var.names <- all.vars(formula)
	x <- data.f[,cat.var]
	y <- data.f[,var.names[1]]
	if (all(sapply(points, function(x, range){ res <- if(x >= range[1] & x <= range[2]){TRUE} else {FALSE}}, range = range))) {
		cutoffs <- sort(unique(c(max(x), min(x), points)))
		x.cut <- cut(x, cutoffs, include.lowest=TRUE,right=TRUE)
		if(length(levels(x.cut)) > 1 & all(table(x.cut)>min.p.cat)) {
			if(length(var.names) == 1) {
				ratio <- prop.table(table(x.cut[y==1]))/prop.table(table(x.cut[y==0]))
				ratio.s <- ratio[match(x.cut, names(ratio))]
				auc <- compute.empirical.AUC(ratio.s[y==1], ratio.s[y==0])
			} else {
				 data.f[,"x.cut_"] <- x.cut
				 formula.n <- update(formula, as.formula("~ . + x.cut_"))				  
				 fit <- gam(formula.n, family = binomial, data = data.f)
				 auc <- compute.empirical.AUC(fit$fitted[y==1], fit$fitted[y==0])  
			}
		} else {
			auc <- NA
		}
	} else {
		auc <- NA
	}
	auc
}
