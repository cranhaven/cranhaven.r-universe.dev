compute.empirical.AUC <-
function(X,Y) {	
	comp <- outer(Y, X, "-")
	comp <- replace(comp, is.na(comp), 0)
	auc <-(sum(comp<0) + 0.5*sum(comp==0))/length(as.vector(comp))
	
	auc
}
