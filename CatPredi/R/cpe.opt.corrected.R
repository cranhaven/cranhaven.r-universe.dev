cpe.opt.corrected <-
function(formula, cat.var, data, c.points, cpe , B , b.method = c("ncoutcome","coutcome")) {
	b.method <- match.arg(b.method)
  cpe.boot <- cpe.original  <- vector(length=B)	
	var.names <- c(all.vars(formula), cat.var)
	X <- data[,cat.var]
	data.o <- data
	
	for (i in 1:B) {
		data.b <- bootstrap.sample(data, group = var.names[2],  method = b.method)
		sel.point = sort(unique(c(min(X, data.b[,cat.var]), max(X, data.b[,cat.var]), c.points)))
		x.cut.boot <- cut(data.b[,cat.var], sel.point, include.lowest = TRUE, right = TRUE)
		x.cut <-  cut(X, sel.point, include.lowest = TRUE, right = TRUE)
		if(length(levels(x.cut.boot)) == length(levels(x.cut)) & all(table(x.cut.boot)>1)){
			data.b[,"x.cut_"] <- x.cut.boot
			data.o[,"x.cut_"] <- x.cut
			# Bootstrap
			formula.n <- update(formula, as.formula("~ . + x.cut_"))
			f.boot <- try(cph(formula.n, data=data.b))
			#if(class(f.boot) == "try-error") {
			if("try-error" %in% class(f.boot)){
				cpe.boot[i] <- NA
				cpe.original[i] <- NA
			} else {
				# cpe.boot[i] <- cpe(f.boot, data.b)
			  cpe.boot[i] <- phcpe2(coef = f.boot$coefficients, coef.var = f.boot$var, design = model.matrix(f.boot, data = data.b))$CPE  #phcpe(f.boot,CPE.SE=FALSE, out.ties=FALSE)$CPE
				
				# Original Sample 
				# cpe.original[i] <- cpe(f.boot, data.o)
			  cpe.original[i] <- phcpe2(coef = f.boot$coefficients, coef.var = f.boot$var, design = model.matrix(f.boot, data = data.o))$CPE
			}
		} else {
			cpe.boot[i] <- NA
			cpe.original[i] <- NA
		}     
	}
	cpe.corrected <- cpe - mean(abs(cpe.original - cpe.boot), na.rm=TRUE)
	cpe.corrected
}
