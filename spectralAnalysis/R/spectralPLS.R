#' Compute PLS model
#' 
#' @param objectList list of SPC files
#' @param UPLC_DF dataframe with UPLC data, which should contain the following columns: experiment, time, and 1 column per compound
#' @param ncomp number of PLS components, defaults to  10 
#' 
#' @return PLS model, as obtained from \code{\link[pls]{plsr}} 
#' @importFrom dplyr filter select .data
#' @importFrom pls plsr
#' 
#' @author Nicolas Sauwen
#' @export
spectralPLSCalibration <- function(objectList, UPLC_DF, ncomp = 10){
	
	uniqueExperiments <- unique(UPLC_DF$experiment)
	uniqueExperimentIDsList <- strsplit(as.character(uniqueExperiments), split = "-")
	uniqueExperimentIDs <- sapply(uniqueExperimentIDsList, function(x) x[length(x)])
	
	# Check if UPLC time goes beyond IR time:
	maxIRTime <- min(sapply(objectList, function(x) max(getTimePoints(x))))
	UPLC_DF <- filter(UPLC_DF, time < maxIRTime)
	
	uniqueCompounds <- setdiff(colnames(UPLC_DF), c("experiment", "time", "min", "sample", "time_dh"))
	Y <- matrix(0, nrow = nrow(UPLC_DF), ncol = length(uniqueCompounds))
	Y <- as.matrix(dplyr::select(UPLC_DF, uniqueCompounds))
	NA_colSums <- apply(Y, 2, function(x) sum(is.na(x)))
	discardColInds <- which(NA_colSums > nrow(Y)/2)
	if(length(discardColInds) > 0) Y <- Y[,-discardColInds]
	Y[is.na(Y)] <- 0
	
	nWavelengths <- length(getSpectralAxis(objectList[[1]]))
	X <- matrix(0, nrow = nrow(UPLC_DF), ncol = nWavelengths)
	expNamesSPC <- sapply(objectList, getExperimentName)
	idx <- 0
	
	for(i in 1:length(uniqueExperimentIDs)){
		matchInd <- grep(uniqueExperimentIDs[i], expNamesSPC)
		if(length(matchInd) == 0){
			matchInd <- grep(substr(uniqueExperimentIDs[i], 2, nchar(uniqueExperimentIDs[i])), expNamesSPC)
		} 
		if(length(matchInd) == 0) return("Mismatch in experiment name(s) between IR and UPLC datasets. PLS model could not be built.")
		times_UPLC <- filter(UPLC_DF, .data$experiment == uniqueExperiments[i])$time
		times_spectral <- objectList[[matchInd]]@timePoints
		spectra <- getSpectra(objectList[[matchInd]])
		for(j in 1:length(times_UPLC)){
			X[idx+j, ] <- spectra[which.min(abs(times_spectral - times_UPLC[j])), ]
		}
		idx <- idx + j
	}
#	plsModel <- pls::plsr(Y ~ X, ncomp = ncomp, validation = "LOO", scale = FALSE)
	plsModel <- plsr(Y ~ X, ncomp = ncomp, validation = "LOO", scale = TRUE)
	
	return(plsModel)
}



#' Perform PLS prediction
#' 
#' @param spectralObject \code{\link{SpectraInTime-class}}
#' @param plsModel PLS model as obtained from \code{\link{spectralPLSCalibration}}
#' @param nComp Number of components 
#' 
#' @return \code{\link{SpectraInTimeComp-class}} which includes PLS model + prediction
#' 
#' @author Nicolas Sauwen
#' @export
spectralPlsPrediction <- function(spectralObject, plsModel, nComp){
	
	X <- getSpectra(spectralObject)
	predictionMat <- predict(plsModel, X, ncomp = nComp)[,,1]
	
	plsSlot           <-  list( plsModel = plsModel , nComp = nComp , prediction =  predictionMat )
	spectralObjectPLS <- SpectraInTimeComp( spectralObject , dimensionReduction = list( PLS = plsSlot )  )
	
	return(spectralObjectPLS)
}

