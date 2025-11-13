select.cutpoint.auc <-
function(formula, cat.var, data, range, points, l.s.points = 100, min.p.cat = 1) {
	search.points = seq(min(range),max(range),l = l.s.points)
	auc.matrix = matrix(ncol = 2,nrow = length(search.points))
	colnames(auc.matrix)<- c("points","auc")
	var.names <- all.vars(formula)
	X <- data[,cat.var]
	Y <- data[,var.names[1]]
	for (i in 1:length(search.points)){
		cutoffs = sort(unique(c(min(X), max(X), points, search.points[i])))
		x.cut = cut(X, cutoffs, include.lowest = TRUE, right = TRUE)
		if(length(levels(x.cut)) > 1 & all(table(x.cut)>= min.p.cat)) {
			if(length(var.names) == 1) {
				ratio <- prop.table(table(x.cut[Y==1]))/prop.table(table(x.cut[Y==0]))
				ratio.s <- ratio[match(x.cut, names(ratio))]
				auc.matrix[i,2] <- compute.empirical.AUC(ratio.s[Y==1], ratio.s[Y==0])
			} else {
				data[,"x.cut_"] <- x.cut
				formula.n <- update(formula, as.formula("~ . + x.cut_"))
				fit <- gam(formula.n, family = binomial, data = data)
				auc.matrix[i,2] <- compute.empirical.AUC(fit$fitted[Y==1], fit$fitted[Y==0])  
			}
			auc.matrix[i,1] <- search.points[i] 
		 } else {
			auc.matrix[i,2] <- NA
		 }
	}
	auc.matrix
}
