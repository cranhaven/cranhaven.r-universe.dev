calcBetas <- function(ped,betas){
	# calculate odds ratios for each individual based on genotype data, given an odds ratio file

	if (any(sapply(betas$rsID,function(x) length(grep(x,dimnames(ped)[[2]]))) == 0)){
		stop("Not all SNPs in the odds ratio file are present in the ped file.")
		}

	# calculate count matrix
	out1 <- t((t(ped[,5 + 2*(1:((length(ped[1,])-6)/2))]) == (betas$RiskAllele)) + (t(ped[,6 + 2*(1:((length(ped[1,])-6)/2))]) == (betas$RiskAllele)))
	
	# find missing genotypes
	missing <- (ped[,5 + 2*(1:((length(ped[1,])-6)/2))] == "N") + (ped[,5 + 2*(1:((length(ped[1,])-6)/2))] == "0") + (ped[,6 + 2*(1:((length(ped[1,])-6)/2))] == "N") + (ped[,6 + 2*(1:((length(ped[1,])-6)/2))] == "0")
	out1[missing] <- NA

	# perform prediction
	prediction <- as.numeric(((betas$beta %*% t(out1)) - sum(betas$betaBar)))
	
	# name the risk vector, and set missing indiviudals to missing
	names(prediction) <- dimnames(ped)[[1]]
	totMissing <- apply(ped[,7:length(ped[1,])],1,function(x) all(x == "0" | x == "N"))
	prediction[totMissing] <- NA
	class(prediction) <- "MangroveContPreds"
	return(prediction)
}
