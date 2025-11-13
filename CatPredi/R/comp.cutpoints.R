comp.cutpoints <-
function(obj1, obj2, V = 100) {
	if(is.null(obj2$results$AUC.cor)==TRUE || is.null(obj1$results$AUC.cor)==TRUE) {
		stop("argument correct.AUC=TRUE is needed in catpredi")
	}
	if(obj1$formula != obj2$formula) {
		stop("The categorized variables are not comparable")
	}
	if(obj1$control$B != obj2$control$B) {
		warning("The bootstrap resamples used for the optimism correction of the AUC is different in both objects")
	}	
	AUC.cor.diff <- obj2$results$AUC.cor - obj1$results$AUC.cor
	formula <- obj1$formula

	point1 <- obj1$results$cutpoints
	point2 <- obj2$results$cutpoints
	B1 <-   obj1$control$B
  B2 <-   obj2$control$B
  b.method <- obj1$control$b.method	
	data <-   obj1$data
	cat.var <- obj1$cat.var  
	var.names <- all.vars(formula)
	X <- data[,cat.var]
	Y <- data[,var.names[1]]
	
	auc.b.1 <- auc.b.2 <- auc.b.diff <- vector(length = V)
  
	for (i in 1:V) {
		data.b <- bootstrap.sample(data, group = var.names[1], method = b.method)
		Y.b <- data.b[,var.names[1]]
		X.b <- data.b[,cat.var]	
		# k 
		sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point1)))
		x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
		data.b[,"x.cut1"] <- x.cut
		formula.n <- update(formula, as.formula("~ . + x.cut1"))
			fit.1 <- gam(formula.n, family = binomial, data = data.b)             
		auc.fit1 <- compute.empirical.AUC(fit.1$fitted[Y.b==1], fit.1$fitted[Y.b==0])
		auc.b.1[i] <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.b, c.points = point1, AUC =auc.fit1 , B=B1, b.method = b.method)
	
		# k = k+1
		sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point2)))
		x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
		data.b[,"x.cut2"] <- x.cut
		formula.n <- update(formula, as.formula("~ . + x.cut2"))
		fit.2 <- gam(formula.n, family = binomial, data = data.b)
		auc.fit2 <- compute.empirical.AUC(fit.2$fitted[Y.b==1], fit.2$fitted[Y.b==0])
		auc.b.2[i] <- auc.opt.corrected(formula = formula, cat.var = cat.var, data = data.b, c.points = point2, AUC =auc.fit2 , B=B2, b.method = b.method)
		auc.b.diff[i] <- auc.b.2[i] - auc.b.1[i]
	}
	auc.diff <- quantile(auc.b.diff, p = c(0.025, 0.975), na.rm=TRUE)
	res <- list( AUC.cor.diff = AUC.cor.diff , icb.auc.diff = auc.diff)
	class(res) <- "comp.cutpoints"
  res
}
