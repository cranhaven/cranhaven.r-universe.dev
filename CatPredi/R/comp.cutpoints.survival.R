comp.cutpoints.survival <-
function(obj1,obj2, V=100) {
	if(obj1$correct.index==FALSE || obj2$correct.index==FALSE ) {
			stop("argument correct.index=TRUE is needed in catpredi.survival")
	}
	if(obj1$formula !=obj2$formula) {
		stop("The categorized variables are not comparable")
	}
	if(obj1$conc.index != obj2$conc.index) {
		stop("The concordance index selected in both objects must be the same")
	}	
	if(obj1$control$B != obj2$control$B) {
		warning("The bootstrap resamples used for the optimism correction is different in both objects")
	}	
	formula <- obj1$formula
	point1 <- obj1$results$cutpoints
	point2 <- obj2$results$cutpoints
	B1 <-   obj1$control$B
  B2 <-   obj2$control$B
  b.method <- obj1$control$b.method	
	data <-   obj1$data
	cat.var <- obj1$cat.var  
	var.names <- c(all.vars(formula), cat.var)
	X <- data[,cat.var]
	ci.b.1 <- ci.b.2 <- ci.b.diff <- vector(length = V)
  
	if(obj1$conc.index=="cindex") {
		ci.cor.diff <- obj2$results$Cindex.cor - obj1$results$Cindex.cor
		for (i in 1:V) {
			data.b <- bootstrap.sample(data, group = var.names[2], method = b.method)
			X.b <- data.b[,cat.var]
			# k 
			sel.point = sort(unique(c(min(X,X.b), max(X,X.b), point1)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut1"] <- x.cut
			formula.n <- update(formula, as.formula("~ . + x.cut1"))
	
			fit.1 <- cph(formula.n, data=data.b)
			cind.fit1 <- cindex.categorization(fit.1$linear.predictors, Surv(data.b[,var.names[1]],data.b[,var.names[2]]))
			ci.b.1[i] <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point1 , cindex = cind.fit1 , B=B1 , b.method = b.method)
			
			# k = k+1
			sel.point = sort(unique(c(min(X,X.b), max(X,X.b), point2)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut2"] <- x.cut
			formula.n <- update(formula, as.formula("~ . + x.cut2"))
		
			fit.2 <- cph(formula.n, data=data.b)
			cind.fit2 <- cindex.categorization(fit.2$linear.predictors, Surv(data.b[,var.names[1]],data.b[,var.names[2]]))
			ci.b.2[i] <- cindex.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point2 , cindex = cind.fit2 , B=B2 , b.method = b.method)

			ci.b.diff[i] <- ci.b.2[i] - ci.b.1[i]
		}	
	} else {
		ci.cor.diff <- obj2$results$CPE.cor - obj1$results$CPE.cor
		for (i in 1:V) {
			data.b <- bootstrap.sample(data,var.names[2], b.method)
			X.b <- data.b[,cat.var]
			# k 
			sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point1)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut1"] <- x.cut
			formula.n <- update(formula, as.formula("~ . + x.cut1"))
			fit.1 <- cph(formula.n, data=data.b)
			cpe.fit1 <- phcpe2(coef = fit.1$coefficients, coef.var = fit.1$var, design = model.matrix(fit.1, data = data.b))$CPE #phcpe(fit.1,CPE.SE=FALSE, out.ties=FALSE)$CPE
			ci.b.1[i] <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point1 , cpe = cpe.fit1 , B = B1 , b.method = b.method)  
				
			# k = k+1
			sel.point = sort(unique(c(min(X,X.b , na.rm=TRUE), max(X,X.b , na.rm=TRUE), point2)))
			x.cut = cut(X.b, sel.point, include.lowest = TRUE, right = TRUE)
			data.b[,"x.cut2"] <- x.cut
			formula.n <- update(formula, as.formula("~ . + x.cut2"))
			
			fit.2 <- cph(formula.n, data=data.b)
			# cpe.fit2 <- coxcpe(fit.2, data.b)
			cpe.fit2 <- phcpe2(coef = fit.2$coefficients, coef.var = fit.2$var, design = model.matrix(fit.2, data = data.b))$CPE #phcpe(fit.2,CPE.SE=FALSE, out.ties=FALSE)$CPE
			ci.b.2[i] <- cpe.opt.corrected(formula = formula, cat.var =  cat.var , data = data.b , c.points = point2 , cpe = cpe.fit2 , B = B2, b.method = b.method)  
	
			ci.b.diff[i] <- ci.b.2[i] - ci.b.1[i]
		} 
	}
	ci.diff <- quantile(ci.b.diff, p = c(0.025, 0.975), na.rm=TRUE)
	res <- list( call = match.call() , CI.cor.diff = ci.cor.diff , icb.CI.diff = ci.diff)
	class(res) <- "comp.cutpoints.survival"
	res
}
