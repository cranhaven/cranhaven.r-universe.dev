summary.MangroveBetas <- function(object,...){
	x <- object
	cat("A Mangrove Betas object:\nNumber of variants:")
	cat(length(x[,1]))
	cat("\nMean absolute beta: ")
	cat(round(mean(abs(x$beta)),3))
	RAF <- x$Freq
	RAF[x$Freq < 0] <- 1 - RAF[x$beta < 0]
	cat("\nMean risk allele frequency: ")
	cat(round(mean(RAF),3))
	cat('\nThese variants explain ')
	cat(round(sum(2*x$Freq*(1 - x$Freq)*x$beta^2)*100,2))
	cat('% of variance\n')	
}
