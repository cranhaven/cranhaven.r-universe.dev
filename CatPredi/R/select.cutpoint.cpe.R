select.cutpoint.cpe <-
function(formula, cat.var, data , range, point, l.s.points = 100, min.p.cat) {
	search.points = seq(min(range),max(range),l=l.s.points)
	cpe.matrix = matrix(ncol=2,nrow=length(search.points))
	colnames(cpe.matrix)<- c("points","cpe")
	var.names <- c(all.vars(formula), cat.var)
	X <- data[,cat.var]	
	for (i in 1:length(search.points)){		 
		cutoffs = sort(unique(c(min(X), max(X),point, search.points[i])))
		x.cut = cut(X,cutoffs, include.lowest = TRUE,right = TRUE)
		if(length(levels(x.cut)) > 1 & all(table(x.cut)>min.p.cat)) {
			data[,"x.cut_"] <- x.cut
			formula.n <- update(formula, as.formula("~ . + x.cut_"))			
			fit <- try(cph(formula.n, data = data))
			#if(class(fit) == "try-error"){
			if("try-error" %in% class(fit)){
				cpe.matrix[i,1] <- search.points[i]
				cpe.matrix[i,2] <- NA
			} else {
				cpe.matrix[i,1] <- search.points[i]
				cpe.matrix[i,2] <- phcpe2(coef = fit$coefficients, coef.var = fit$var, design = model.matrix(fit, data = data))$CPE
			}     		
		} else {
			cpe.matrix[i,1] <- search.points[i]
			cpe.matrix[i,2] <- NA
	 	}
	}
	cpe.matrix 
}
