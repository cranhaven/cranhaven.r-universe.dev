#DataChecks <- function (PhenotypesVector, Genotypes, Pathways, Covariates, LogFile) {
#
#	#Checking for NAs in phenotype vector
#	if (TRUE %in% sapply(PhenotypesVector, is.na)) {
#		stop(Sys.time(), " -- there are NAs in the phenotype vector. Please remove and align the remaining files (eg genotype matrix).");
#	}
#	LogFile <- rbind(LogFile, paste(format(Sys.time()), " -- PhenotypesVector passed NA check.", sep=""))
#
#	#Checking for missingness in genotype matrix
#	if (apply(Genotypes, c(1,2), is.na)) {
#		stop(Sys.time(), " -- there are NAs in the genotype matrix. There must be zero missingness in the genotype matrix. Please correct and rerun.");
#	}
#	LogFile <- rbind(LogFile, paste(format(Sys.time()), " -- Genotypes passed NA check.", sep=""))
#
#	return(LogFile)
#
#}

#' @importFrom stats sd lm residuals
PreprocessData <- function (PhenotypesVector, Genotypes, Pathways, Covariates, CenterStandardize, RegressPhenotypes, LogFile) {

	PreprocessData.Output <- list();
	PhenotypeMatrix <- c();

	#Setup file for indices of each pathway
	Pathways.Full <- lapply(strsplit(as.character(Pathways[,2]), ","), as.numeric)

	#Center and standardize genotype matrix (n x p) if flagged
	if (CenterStandardize == TRUE) {
		Genotypes.Mean <- apply(Genotypes, 2, mean); 
		Genotypes.SD <- apply(Genotypes, 2, sd); 
		Genotypes <- t((t(Genotypes)-Genotypes.Mean)/Genotypes.SD);
#		LogFile <- rbind(LogFile, paste(format(Sys.time()), " -- Centering and standardizing genotype matrix.", sep=""))
	}

	#Regress out additive effects from phenotype if flagged
	if (RegressPhenotypes == TRUE) { 
		for (i in 1:nrow(Pathways)) { 
			Genotypes.Pathway <- Genotypes[,as.numeric(unlist(Pathways.Full[i]))];
			PhenotypeMatrix <- cbind(PhenotypeMatrix, residuals(lm(as.matrix(PhenotypesVector) ~ as.matrix(Genotypes.Pathway) - 1)))	
		}
	} else {
		for (i in 1:nrow(Pathways)) { 
			PhenotypeMatrix <- cbind(PhenotypeMatrix, PhenotypesVector);
		}
	}

	return(list(PhenotypeMatrix=PhenotypeMatrix, Genotypes=Genotypes, Pathways.Full=Pathways.Full, LogFile=LogFile))

}
