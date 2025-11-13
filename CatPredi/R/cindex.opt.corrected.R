cindex.opt.corrected <-
function(formula, cat.var, data , c.points, cindex , B ,  b.method = c("ncoutcome","coutcome")){
  b.method <- match.arg(b.method)
	cind.boot <- cind.original  <- vector(length=B)
	var.names <- c(all.vars(formula), cat.var)
	X <- data[,cat.var]
	data.o <- data
	for (i in 1:B) {
		data.b <- bootstrap.sample(data, group = var.names[2],  method = b.method)
		sel.point = sort(unique(c(min(X, data.b[,cat.var]), max(X, data.b[,cat.var]), c.points)))
		x.cut.boot <- cut(data.b[,cat.var], sel.point, include.lowest = TRUE, right = TRUE)
		x.cut <-  cut(X, sel.point, include.lowest = TRUE, right = TRUE)
		if(length(levels(x.cut.boot)) == length(levels(x.cut)) & all(table(x.cut.boot)>1)) {		
			data.b[,"x.cut_"] <- x.cut.boot
			data.o[,"x.cut_"] <- x.cut
			# Bootstrap
			formula.n <- update(formula, as.formula("~ . + x.cut_"))
			f.boot <- cph(formula.n, data=data.b)
			cind.boot[i] <- cindex.categorization(f.boot$linear.predictors, Surv(data.b[,var.names[1]],data.b[,var.names[2]]))
			# Original Sample 
			p <- predict(f.boot, newdata = data.o, type = "lp")
			cind.original[i] <- cindex.categorization(p, Surv(data.o[,var.names[1]],data.o[,var.names[2]]))
		} else {
			cind.boot[i] <- NA
			cind.original[i] <- NA
		}		
	}
	cind.corrected <- cindex - mean(abs(cind.original - cind.boot), na.rm=TRUE)
	cind.corrected
}
