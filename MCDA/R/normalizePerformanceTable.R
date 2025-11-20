#' Function to normalize (or rescale) the columns (or criteria) of a
#' performance table.
#' 
#' Standardizes the range of the criteria according to a few methods :
#' percentage of max, scale between 0 and 1, scale to 0 mean and 1 standard
#' deviation, scale to euclidian unit length.
#' 
#' 
#' @param performanceTable A matrix containing the performance table to be
#' plotted. The columns are labelled according to the criteria IDs, and the
#' rows according to the alternatives IDs.
#' @param normalizationTypes Vector indicating the type of normalization that
#' should be applied to each of the criteria. Possible values :
#' "percentageOfMax", "rescaling" (minimum becomes 0, maximum becomes 1),
#' "standardization" (rescale to a mean of 0 and a standard deviation of 1),
#' "scaleToUnitLength" (scale the criteria values such that the column has
#' euclidian length 1). Any other value (like "none") will result in no data
#' transformation. The elements are named according to the IDs of the criteria.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the data should be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @keywords methods
#' @examples
#' 
#' library(MCDA)
#' 
#' performanceTable <- matrix(runif(5*9), ncol=5)
#' 
#' row.names(performanceTable) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9")
#' 
#' colnames(performanceTable) <- c("g1","g2","g3","g4", "g5")
#' 
#' normalizationTypes <- c("percentageOfMax","rescaling",
#'                         "standardization","scaleToUnitLength", "none")
#' 
#' names(normalizationTypes) <- c("g1","g2","g3","g4","g5")
#' 
#' normalizedPerformanceTable <- normalizePerformanceTable(performanceTable,
#'                                                         normalizationTypes)
#' 
#' 
#' @export normalizePerformanceTable
normalizePerformanceTable <- function(performanceTable, normalizationTypes=NULL, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## http://en.wikipedia.org/wiki/Feature_scaling
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performance table, should be a matrix or a data frame")
  
  if (!(is.null(normalizationTypes) || is.vector(normalizationTypes)))
    stop("normalizationTypes should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteria IDs should be in a vector")
  
  ## filter the performance table and the criteria according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)) performanceTable <- performanceTable[alternativesIDs,]
  
  if (!is.null(criteriaIDs)) performanceTable <- performanceTable[,criteriaIDs]
  
  if (!is.null(criteriaIDs) && !is.null(normalizationTypes)) normalizationTypes <- normalizationTypes[criteriaIDs]
  
  for (i in 1:dim(performanceTable)[2]){
    if (normalizationTypes[i] == "percentageOfMax"){
      performanceTable[,i] <- percentageOfMax(performanceTable[,i])
    }
    else if (normalizationTypes[i] == "rescaling"){
      performanceTable[,i] <- rescaling(performanceTable[,i])
    }
    else if (normalizationTypes[i] == "standardization"){
      performanceTable[,i] <- standardization(performanceTable[,i])
    }
    else if (normalizationTypes[i] == "scaleToUnitLength"){
      performanceTable[,i] <- scaleToUnitLength(performanceTable[,i])
    }
  }

  return(performanceTable)
  
}

percentageOfMax <- function(data){
  max <- max(data)
  data <- data/max
  return(data)
}

rescaling <- function(data){
  max <- max(data)
  min <- min(data)
  data <- (data-min)/(max-min)
  return(data)
}

standardization <- function(data){
  mean <- mean(data)
  sd <- sd(data)
  data <- (data - mean)/sd
  return(data)
}

scaleToUnitLength <- function(data){
  norm <- sqrt(sum(data^2))
  data <- data/norm
  return(data)
}
