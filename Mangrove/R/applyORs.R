applyORs <-
function(riskpred,K){
	#calculates disease probabilities from odds ratios and a prevalence
	id <- names(riskpred)
	class(riskpred) <- "numeric"
	names(riskpred) <- id
	preodd <- K/(1-K)
	postodd <- preodd*riskpred
	return(postodd/(1 + postodd))
	}

