summary.MangroveSample <-
function(object,...){
	x <- object
	cat("A Mangrove family prevalence simulation.\n")
	cat("The pedigree has ")
	cat(x$Ncases)
	cat(" cases out of ")
	cat(x$N)
	cat(" individuals.\n")
	cat("The prevalence is ")
	cat(x$K)
	cat(".\nExpected number of cases given common variants (95% CI): ")
	cat(mean(x$sample))
	cat(" (")
	cat(as.numeric(quantile(x$sample,probs=0.025)))
	cat(" - ")
	cat(as.numeric(quantile(x$sample,probs=0.975)))
	cat(")\n")
	cat("Probability of seeing at least ")
	cat(x$Ncases)
	cat(" cases given common variants: ")
	cat(mean(x$sample >= x$Ncases))
	cat('\n')
	}

