select.cutpoint.cind <-
function(formula, cat.var, data , range, point, l.s.points = 100, min.p.cat) {
	search.points=seq(min(range),max(range),l=l.s.points)
	cind.matrix=matrix(ncol=2,nrow=length(search.points))
	colnames(cind.matrix)<- c("points","c-index")
	var.names <- c(all.vars(formula), cat.var)
	X <- data[,cat.var]
	
	for (i in 1:length(search.points)){
		cutoffs=sort(unique(c(min(X), max(X),point, search.points[i])))
		x.cut=cut(X,cutoffs, include.lowest=TRUE,right=TRUE)
		if(length(levels(x.cut)) > 1 & all(table(x.cut)>min.p.cat)) {
			data[,"x.cut_"] <- x.cut
			formula.n <- update(formula, as.formula("~ . + x.cut_"))
			fit <- try(cph(formula.n, data = data))
			#if(class(fit) == "try-error"){
			if("try-error" %in% class(fit)) {
				cind.matrix[i,1] <- search.points[i]
				cind.matrix[i,2] <- NA
			} else {       
				cind.matrix[i,1] <- search.points[i]
				cind.matrix[i,2] <- cindex.categorization(fit$linear.predictors, Surv(data[,var.names[1]],data[,var.names[2]]))        
			}
		} else {
			cind.matrix[i,1] <- search.points[i]
			cind.matrix[i,2] <- NA
	 	}
	}
	cind.matrix
}
