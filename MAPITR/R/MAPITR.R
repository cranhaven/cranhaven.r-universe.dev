#' @title MArginal ePIstasis Test for Regions
#' (\code{MAPITR})
#'
#' @description Run \code{MAPITR} for a group of pathways on a single
#' phenotype and a set of genome-wide SNPs
#'
#' @param Phenotype A vector containing phenotypic values for all
#' individuals being analyzed. No default value.
#'
#' @param Genotypes A n x p matrix containing the genotypes (0/1/2) for
#' all p SNPs across all n individuals. No default value.
#'
#' @param Pathways A r x 2 matrix containing the pathway names and then
#' a comma-separated list of the \code{Genotypes} column indices
#' representing each SNP in the associated pathway. Note, this second 
#' column of comma-separated indices are the numeric positions for each 
#' SNP in \code{Genotypes} and not the SNP IDs or column names. No
#' default value.
#'
#' @param Covariates A n x q matrix containing any q additional covariates
#' that should be included in the M-projection matrix of the model. 
#' See Turchin et al. 2020 for details. Note that these are covariates
#' which are applied to both sides of the model, ie the phenotype as well
#' as the genotypes. A y-intercept term is automatically included and does
#' not need to be part of this n x q matrix. No default value.
#'
#' @param CenterStandardize A logical \code{TRUE}/\code{FALSE} flag that
#' indicates whether the genotype matrix \code{Genotypes} should be
#' centered and standardized before analysis. This is a recommended step.
#' Indicate \code{FALSE} if this is a preprocessing step that has
#' already been done prior to running \code{MAPITR}. The default value 
#' is TRUE.  
#'
#' @param OpenMP A logical \code{TRUE}/\code{FALSE} flag that indicates
#' whether OpenMP versions of \code{MAPITR} should be implemented.
#' OpenMP versions of the underlying Rcpp code will run more quickly, 
#' but requires the user to be operating an R version that has 
#' been installed with OpenMP access. The default value is FALSE.
#'
#' @param cores A numeric value providing the expected number of cores
#' if the OpenMP version of the code is being used. 
#' \code{parallel::detectCores()} is used by default to assign this 
#' variable when no value is given. A value generally should only be 
#' given when needing finer control of the code or for testing purposes. 
#' The default value is NULL. 
#'
#' @param ... Additional optional arguments.
#'
#' @return A list containing two entries. First, a dataframe (\code{Results})
#' containing in the first column the list of pathways that were analyzed, 
#' in the second column the associated \code{MAPITR} p-values for each 
#' pathway, in the third column the associated \code{MAPITR} variance 
#' component estimates for each pathway, and in the fourth column the 
#' associated \code{MAPITR} PVEs for each pathway. Second, a matrix
#' (\code{Eigenvalues}) containing the n associated \code{MAPITR} 
#' eigenvalues for each pathway per column. 
#' 
#' @examples
#' data(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, 
#' MAPITR_TestData_Pathways)
#' MAPITROutput <- MAPITR(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, 
#' MAPITR_TestData_Pathways, OpenMP=FALSE)
#' MAPITROutput$Results
#'
#' @export
#' 
MAPITR <- function (Genotypes, Phenotype, Pathways, Covariates = NULL, CenterStandardize = TRUE, OpenMP = FALSE, cores = NULL, ...) {
	return(MAPITRmain(Genotypes, Phenotype, Pathways, Covariates, CenterStandardize, OpenMP, cores, ...))
}

#' @importFrom parallel detectCores
MAPITRmain <- function (Genotypes, Phenotype, Pathways, Covariates, CenterStandardize, OpenMP, cores, GRM_Grand = NULL, GRM_Pathway = NULL, RegressPhenotypes = TRUE, PrintProgress = FALSE) {

        MAPITRprocessing <- list()
	MAPITRoutput <- list()
	MAPITRoutput$LogFile <- c()
	MAPITRoutput$pValues <- NULL
	MAPITRoutput$PVE <- NULL
	if (is.null(cores)) { 
		cores = parallel::detectCores()
	}

        MAPITRoutput$LogFile <- rbind(MAPITRoutput$LogFile, paste(format(Sys.time()), " -- beginning MAPITR.", sep=""))
        if (PrintProgress == TRUE) {
                write(paste(format(Sys.time()), " -- beginning MAPITR.", sep=""), stderr())
        }

	#Data Checks
	#MAPITRoutput$LogFile <- DataChecks(Phenotype, Genotypes, Pathways, Covariates, MAPITRoutput$LogFile)

	#Preprocessing Data
	MAPITRoutput.temp1 <- PreprocessData(Phenotype, Genotypes, Pathways, Covariates, CenterStandardize, RegressPhenotypes, MAPITRoutput$LogFile)
	PhenotypeMatrix <- MAPITRoutput.temp1$PhenotypeMatrix
	Genotypes <- MAPITRoutput.temp1$Genotypes
	Pathways.Full <- MAPITRoutput.temp1$Pathways.Full
	MAPITRoutput$LogFile <- MAPITRoutput.temp1$LogFile
	rm(MAPITRoutput.temp1)	

	#Running appropriate version of MAPITR
	if (OpenMP == FALSE) { 
		if (is.null(Covariates)) {
			MAPITRoutput.temp4 <- RunMAPITR.Base.noOpenMP(PhenotypeMatrix, Genotypes, Pathways.Full, MAPITRoutput$LogFile) 
			MAPITRoutput$Est <- MAPITRoutput.temp4$Est; MAPITRoutput$PVE <- MAPITRoutput.temp4$PVE; MAPITRoutput$Eigenvalues <- MAPITRoutput.temp4$Eigenvalues;
			MAPITRoutput$pValues <- GetMAPITRpValues(MAPITRoutput.temp4$Est, MAPITRoutput.temp4$Eigenvalues)
			rm(MAPITRoutput.temp4)
		} else if (!is.null(Covariates)) { 
			MAPITRoutput.temp5 <- RunMAPITR.wCovs.noOpenMP(PhenotypeMatrix, Genotypes, Pathways.Full, Covariates, MAPITRoutput$LogFile) 
			MAPITRoutput$Est <- MAPITRoutput.temp5$Est; MAPITRoutput$PVE <- MAPITRoutput.temp5$PVE; MAPITRoutput$Eigenvalues <- MAPITRoutput.temp5$Eigenvalues;
			MAPITRoutput$pValues <- GetMAPITRpValues(MAPITRoutput.temp5$Est, MAPITRoutput.temp5$Eigenvalues)
			rm(MAPITRoutput.temp5)
		} else {
			stop(Sys.time(), " -- 'Covariates' is neither null or not null. This should not happen. Contact current maintainer of code.")
		}
	} else if (OpenMP == TRUE) { 
		if (is.null(Covariates)) {
			MAPITRoutput.temp2 <- RunMAPITR.Base(PhenotypeMatrix, Genotypes, Pathways.Full, cores, MAPITRoutput$LogFile) 
			MAPITRoutput$Est <- MAPITRoutput.temp2$Est; MAPITRoutput$PVE <- MAPITRoutput.temp2$PVE; MAPITRoutput$Eigenvalues <- MAPITRoutput.temp2$Eigenvalues;
			MAPITRoutput$pValues <- GetMAPITRpValues(MAPITRoutput.temp2$Est, MAPITRoutput.temp2$Eigenvalues)
			rm(MAPITRoutput.temp2)
		} else if (!is.null(Covariates)) { 
			MAPITRoutput.temp3 <- RunMAPITR.wCovs(PhenotypeMatrix, Genotypes, Pathways.Full, Covariates, cores, MAPITRoutput$LogFile) 
			MAPITRoutput$Est <- MAPITRoutput.temp3$Est; MAPITRoutput$PVE <- MAPITRoutput.temp3$PVE; MAPITRoutput$Eigenvalues <- MAPITRoutput.temp3$Eigenvalues;
			MAPITRoutput$pValues <- GetMAPITRpValues(MAPITRoutput.temp3$Est, MAPITRoutput.temp3$Eigenvalues)
			rm(MAPITRoutput.temp3)
		} else {
			stop(Sys.time(), " -- 'Covariates' is neither null or not null. This should not happen. Contact current maintainer of code.")
		}
	}
	else {
		stop(Sys.time(), " -- 'OpenMP' is neither TRUE or FALSE. Please rerun with one of these two options.")
	}
		
	#Postprocessing of results (if needed)
	Pathway.Names <- as.character(Pathways[,1])

	MAPITRoutput.Final <- list();
	MAPITRoutput.Final.Sub1 <- cbind(Pathway.Names, MAPITRoutput$pValues, MAPITRoutput$Est, MAPITRoutput$PVE); colnames(MAPITRoutput.Final.Sub1) <- c("Pathways", "pValues", "Est", "PVE"); 
	MAPITRoutput.Final.Sub1 <- as.data.frame(MAPITRoutput.Final.Sub1); MAPITRoutput.Final.Sub1[,1] <- as.character(MAPITRoutput.Final.Sub1[,1]); MAPITRoutput.Final.Sub1[,2] <- as.numeric(as.character(MAPITRoutput.Final.Sub1[,2])); MAPITRoutput.Final.Sub1[,3] <- as.numeric(as.character(MAPITRoutput.Final.Sub1[,3])); MAPITRoutput.Final.Sub1[,4] <- as.numeric(as.character(MAPITRoutput.Final.Sub1[,4]));
	MAPITRoutput.Final[["Results"]] <- MAPITRoutput.Final.Sub1; MAPITRoutput.Final[["Eigenvalues"]] <- MAPITRoutput$Eigenvalues; 

	return(MAPITRoutput.Final) 

}
