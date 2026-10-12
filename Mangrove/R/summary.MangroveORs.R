summary.MangroveORs <-
function(object,K=NULL,...){
	x <- object
	cat("A Mangrove Odds Ratios object:\nNumber of variants:")
	cat(length(x[,1]))
	cat("\nMean absolute het OR: ")
	cat(round(mean(sapply(x$ORhet,function(i) max(i,1/i))),3))
	cat("\nMean absolute hom OR: ")
	cat(round(mean(sapply(x$ORhom,function(i) max(i,1/i))),3))
	RAF <- x$Freq
	RAF[x$ORhet < 1] <- 1 - RAF[x$ORhet < 1]
	cat("\nMean risk allele frequency: ")
	cat(round(mean(RAF),3))
	if (is.null(K)){
		cat('\nFor a common (10%) disease, these variants explain ')
		cat(round(getVarExp(x,0.1)*100,2))
		cat('% of variance\nFor a rare (0.5%) disease, these variants explain ')
		cat(round(getVarExp(x,0.005)*100,2))
		cat('% of variance\n')	
	} else {
		cat('\nGiven a prevalence of ')
		cat(K*100)
		cat('% these variants explain ')
		cat(round(getVarExp(x,K)*100,2))
		cat('% of variance\n')	
	}
}

