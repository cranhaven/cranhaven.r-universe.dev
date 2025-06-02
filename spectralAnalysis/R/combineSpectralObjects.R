#' Function to combine \code{\link{SpectraInTime-class}} objects containing 1 spectrum each
#' 
#' @param objectList List of \code{\link{SpectraInTime-class}} objects to be combined
#' @param timeRange Numeric value, equal to the maximum time of the measured spectra. 
#' @param checkNames Boolean - if \code{TRUE}, the experiment name of the spectral objects will be compared to see if these spectral objects belong to the same experiment
#' @return \code{\link{SpectraInTime-class}}
#' 
#' @author Nicolas Sauwen
#' @export
combineSpectralObjects <- function(objectList, timeRange, checkNames = TRUE){
	
	# First, some tests to see if spectral objects are combinable
	nWavelengths <- unique(sapply(objectList, function(x) length(getSpectralAxis(x))))
	if(length(nWavelengths) > 1) return("Spectral objects don't have the same wavelengths. Combining not possible")
	expNameFirst <- getExperimentName(objectList[[1]])
	expNameLast <- getExperimentName(objectList[[length(objectList)]])
	nChar <- min(nchar(expNameFirst), nchar(expNameLast))
	if(checkNames){
		matchingPartInd <- which(unlist(strsplit(substr(expNameFirst, 1, nChar), split = "")) != unlist(strsplit(substr(expNameLast, 1, nChar), split = "")))[1] - 1
		if(matchingPartInd < 7) return("Spectral objects don't belong to same experiment. Combining not possible")
	} else matchingPartInd <- nchar(expNameFirst)
	expName <- substr(expNameFirst, 1, matchingPartInd)
	
	timePoints <- seq(0, timeRange, length.out = length(objectList))*3600
	spectra <- t(sapply(objectList, function(x) getSpectra(x)))
	
	combinedObject <- objectList[[1]]
	setExperimentName(combinedObject) <- expName
	combinedObject@timePoints <- timePoints
	combinedObject@timePointsAlt <- timePoints
	combinedObject@spectra <- spectra
	
	return(combinedObject)
}
