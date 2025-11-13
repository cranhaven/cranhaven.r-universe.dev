auc.opt.corrected <-
function(formula, cat.var, data, c.points, AUC, B=50 ,  b.method = c("ncoutcome","coutcome")) {
	b.method <- match.arg(b.method)
  auc.boot <- auc.original <- vector(length=B)
	var.names <- all.vars(formula)
	X <- data[,cat.var]
	Y <- data[,var.names[1]]
	data.f <- data
	for (i in 1:B) {
		data.b <- bootstrap.sample(data, group = var.names[1], method = b.method)
		Y.b <- data.b[,var.names[1]]
		X.b <- data.b[,cat.var]		
		sel.point = sort(unique(c(min(X, X.b, na.rm=TRUE), max(X, X.b , na.rm=TRUE), c.points)))
		x.cut = cut(X, sel.point, include.lowest = TRUE, right = TRUE)
		x.cut.boot <- cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
		if(length(levels(x.cut.boot)) == length(levels(x.cut)) & all(table(x.cut.boot)>1)){
			if(length(var.names) == 1) {
				# Bootstrap
				ratio <- prop.table(table(x.cut.boot[Y.b==1]))/prop.table(table(x.cut.boot[Y.b==0]))
				ratio.s <- ratio[match(x.cut.boot, names(ratio))]
				auc.boot[i] <- compute.empirical.AUC(ratio.s[Y.b==1], ratio.s[Y.b==0])
				# Original Sample
				ratio.s <- ratio[match(x.cut, names(ratio))]
				auc.original[i] <- compute.empirical.AUC(ratio.s[Y==1], ratio.s[Y==0])
			} else {
				# Bootstrap
				data.b[,"x.cut_"] <- x.cut.boot
				formula.n <- update(formula, as.formula("~ . + x.cut_"))
				fit.b <- gam(formula.n, family = binomial, data = data.b)
				auc.boot[i] <- compute.empirical.AUC(fit.b$fitted[Y.b==1], fit.b$fitted[Y.b==0])
				# Original Sample
				data.f[,"x.cut_"] <- x.cut
				fit.o <- predict(fit.b, newdata =data.f, type="response")
				auc.original[i] <- compute.empirical.AUC(fit.o[Y==1], fit.o[Y==0])
		  	}
		} else {
			auc.boot[i] <- NA
			auc.original[i] <- NA
		}
	}
	auc.corrected <- AUC - mean(abs(auc.original - auc.boot), na.rm=TRUE)
	auc.corrected
}
