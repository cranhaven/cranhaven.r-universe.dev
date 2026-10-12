getVarExp <-
function(ORs,K) {
	# get variance explained assuming additivity
	k <- (sum(sapply(1:length(ORs[,4]),function(i) .getVarExp_loci(K,ORs[i,3],ORs[i,4],ORs[i,5]))))
	return(k/(1+k))
	}

