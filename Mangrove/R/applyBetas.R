applyBetas <- function(contpred,mu,sigma){
	id <- names(contpred)
	class(contpred) <- "numeric"
    names(contpred) <- id
	return(mu + contpred*sigma)
}
