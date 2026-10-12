calcORs <-
function(ped,ORs){
	# calculate odds ratios for each individual based on genotype data, given an odds ratio file

	if (any(sapply(ORs[,1],function(x) length(grep(x,dimnames(ped)[[2]]))) == 0)){
		stop("Not all SNPs in the odds ratio file are present in the ped file.")
		}

	# a vector to hold the missing values
	risk <- rep(1,length(ped[,1]))
	
	# set all individuals with all data missing to NA

	for (i in 1:length(ORs[,1])){
		SNPs <- ped[paste(ORs[i,1],c(".1",".2"),sep="")]
		missingDat <- apply(SNPs,1,function(x) any(x == "0" | x == "N"))
		counts <- apply(SNPs == ORs[i,2],1,sum)
		ORmap <- c(1,ORs[i,3],ORs[i,4])/ORs[i,6]
		thisORs <- (ORmap[counts+1])
		#outtemp[i,] <<- c(ORs[i,1],thisORs["CD1"])
		thisORs[missingDat] <- 1
		risk <- risk*thisORs
	}
	
	# name the risk vector, and set missing indiviudals to missing
	names(risk) <- dimnames(ped)[[1]]
	totMissing <- apply(ped[,7:length(ped[1,])],1,function(x) all(x == "0" | x == "N"))
	risk[totMissing] <- NA
	class(risk) <- "MangroveRiskPreds"
	return(risk)
}

