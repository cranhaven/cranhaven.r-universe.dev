#' VIKOR method
#' 
#' VIKOR is a multi-criteria decision analysis method originally developed by 
#' Serafim Opricovic in his 1979 Ph.D. Thesis, and later published in 1998.
#' 
#' 
#' @param performanceTable Information matrix with nAlt rows and nCrit columns. 
#' Values correspond to the level the corresponding criteria takes for the 
#' corresponding alternative. All values should be numeric. Rows and columns
#' should be named as the alternatives and criteria, respectively.
#' @param criteriaWeights Numeric vector with nCrit elements. Should be named.
#' @param criteriaMinMax Character vector with nCrit elements. It should 
#' contain values "min" if the corresponding criteria is to be minimised (less 
#' is better), or "max" if the corresponding criteria is to be maximised (more 
#' is better).
#' @param v Numeric scalar. Parameter defining the importance given to the group
#' utility, with respect to the minimun regret of the opponent alternative. 
#' Should be between 0 and 1. Default is 0.5.
#' @param positiveIdealSolutions Numeric vector of ideal criteria values. If 
#' omitted, then they are defined as the best values observed among the 
#' existing alternatives.
#' @param negativeIdealSolutions Numeric vector of worst possible criteria 
#' values. If omitted, then they are defined as the worst values observed among 
#' the existing alternatives.
#' @param alternativesIDs Character vector. Name of the alternatives to consider
#' in the evaluation. If omitted, all alternatives in performanceTable are used.
#' @param criteriaIDs Character vector. Name of the criteria to consider 
#' in the evaluation. If omitted, all criteria in performanceTable are used.
#' 
#' @return The function returns a vector containing the VIKOR score for each
#' alternative.
#' @references Opricovic, S. (1998). Multicriteria optimization of civil 
#' engineering systems. Faculty of civil engineering, Belgrade, 2(1), 5-21.
#' @examples
#' alts <- c("Corsa","Clio","Fiesta")
#' crit <- c("price","economy", "aesthetics","bootCapacity")
#' performanceTable <- matrix(c(5490, 51.4, 8.5, 285,
#'                              6500, 70.6, 7.0, 288,
#'                              6489, 54.3, 7.5, 290), 
#'                              nrow=3, ncol=4, byrow=TRUE, 
#'                              dimnames=list(alts, crit))
#' criteriaWeights <- setNames(c(0.35,0.25,0.25,0.15), crit)
#' criteriaMinMax  <- setNames(c("min", "max", "max", "max"), crit)
#' positiveIdealSolutions <- setNames(c(4500, 80, 9, 300), crit)
#' negativeIdealSolutions <- setNames(c(7000, 52, 7, 150), crit)
#' 
#' # Overall
#' VIKOR(performanceTable, criteriaWeights, criteriaMinMax)
#' # Assuming different ideal and worst solutions
#' VIKOR(performanceTable, criteriaWeights, criteriaMinMax,
#'       v=0.5, positiveIdealSolutions, negativeIdealSolutions)
#' # Using a subset of alternatives and criteria
#' VIKOR(performanceTable, criteriaWeights, criteriaMinMax,
#'       v=0.5, positiveIdealSolutions, negativeIdealSolutions,
#'       alternativesIDs = c("Clio","Fiesta"),
#'       criteriaIDs = c("price","economy","aesthetics"))
#' @export VIKOR
VIKOR <- function(performanceTable, criteriaWeights, criteriaMinMax, v=0.5, 
                  positiveIdealSolutions = NULL, negativeIdealSolutions = NULL, 
                  alternativesIDs = NULL, criteriaIDs = NULL){
  ### Validate input
  if(!(is.null(alternativesIDs) || is.vector(alternativesIDs))) stop("alternatives IDs should be in a vector")
  if(!(is.null(criteriaIDs) || is.vector(criteriaIDs))) stop("criteria IDs should be in a vector")
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("performanceTable must be a matrix or a data frame")
  if (!(length(criteriaWeights) == ncol(performanceTable))) 
    stop("the number of criteriaWeights must equal the number of columns in the performanceTable")
  if (missing(criteriaMinMax)) 
    stop("the input criteriaMinMax is required.")
  if (!is.null(alternativesIDs)) 
    performanceTable <- performanceTable[alternativesIDs, ]
  if (!is.null(criteriaIDs)) {
    performanceTable <- performanceTable[, criteriaIDs]
    criteriaWeights <- criteriaWeights[criteriaIDs]
    if (!missing(positiveIdealSolutions)) 
      positiveIdealSolutions <- positiveIdealSolutions[criteriaIDs]
    if (!missing(negativeIdealSolutions)) 
      negativeIdealSolutions <- negativeIdealSolutions[criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  ### Useful variables
  critno <- length(criteriaWeights)
  altno <- nrow(performanceTable)
  pis <- c(1:critno)
  nis <- c(1:critno)
  
  ### Processing
  if (missing(positiveIdealSolutions) || missing(negativeIdealSolutions)) {
    for (i in 1:critno) {
      if (criteriaMinMax[i] == "max") {
        pis[i] <- max(performanceTable[, i])
        nis[i] <- min(performanceTable[, i])
      }
      else {
        pis[i] <- min(performanceTable[, i])
        nis[i] <- max(performanceTable[, i])
      }
    }
  } else {
    if (!(length(positiveIdealSolutions) == length(negativeIdealSolutions) || 
          length(positiveIdealSolutions) == critno)) 
      stop("the number of postive and negative ideal solutions need to equal the number of alternaitves.")
    pis <- positiveIdealSolutions
    nis <- negativeIdealSolutions
  }
  for (i in 1:critno)
    performanceTable[,i] <- (pis[i] - performanceTable[,i])/(pis[i] - nis[i])
  wnm <- t(t(performanceTable) * criteriaWeights)
  sj <- c(1:altno)
  rj <- c(1:altno)
  for (i in 1:altno) {
    sj[i] <- sum(wnm[i,])
    rj[i] <- max(wnm[i,])
  }
  results <- c(1:altno)
  for (i in 1:altno) {
    results[i] <- (v*(sj[i]-min(sj))/(max(sj)-min(sj)))+((1-v)*(rj[i]-min(rj))/(max(rj)-min(rj)))
  }
  names(results) <- rownames(performanceTable)
  return(results)
}

