#' PROMETHEE II
#' 
#' The PROMETHEE II constructs preference indices from the criteria evaluations
#' of alternatives and outputs a pre-order based on the outranking flows
#' between the alternatives.
#' 
#' 
#' @param performanceTable Matrix containing the evaluation table.  Each row
#' corresponds to an alternative, and each column to a criterion.  Rows (resp.
#' columns) must be named according to the IDs of the alternatives (resp.
#' criteria).
#' @param preferenceFunction A vector with preference
#' functions.preferenceFunction should be equal to Usual,U-shape,V-shape,
#' Level,V-shape-Indiff or Gaussian. The elements are named according to the
#' IDs of the criteria.
#' @param preferenceThreshold A vector containing threshold of strict
#' preference. The elements are named according to the IDs of the criteria.
#' @param indifferenceThreshold A vector containing threshold of indifference.
#' The elements are named according to the IDs of the criteria.
#' @param gaussParameter A vector containing parameter of the Gaussian
#' preference function. The elements are named according to the IDs of the
#' criteria.
#' @param criteriaWeights Vector containing the weights of the criteria.  The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @return The function returns a list containing the alternatives IDs in
#' decreasing order of preference. Each elements of the list can be a vector of
#' alternatives IDs.
#' @examples
#' 
#' # The evaluation table
#' 
#' performanceTable <- rbind(
#'   c(1,10,1),
#'   c(4,20,2),
#'   c(2,20,0),
#'   c(6,40,0),
#'   c(30,30,3))
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # The preference functions 
#' preferenceFunction<-c("Gaussian","Level","V-shape-Indiff")
#' 
#' #Preference threshold
#' preferenceThreshold<-c(5,15,3)
#' names(preferenceThreshold)<-colnames(performanceTable)
#' 
#' #Indifference threshold
#' indifferenceThreshold<-c(3,11,1)
#' names(indifferenceThreshold)<-colnames(performanceTable)
#' 
#' #Parameter of the Gaussian preference function
#' gaussParameter<-c(4,0,0)
#' names(gaussParameter)<-colnames(performanceTable)
#' 
#' #weights
#' 
#' criteriaWeights<-c(0.2,0.3,0.5)
#' names(criteriaWeights)<-colnames(performanceTable)
#' 
#' # criteria to minimize or maximize
#' 
#' criteriaMinMax<-c("min","min","max")
#' names(criteriaMinMax)<-colnames(performanceTable)
#' 
#' PROMETHEEII(performanceTable, preferenceFunction,preferenceThreshold,
#'             indifferenceThreshold,gaussParameter,criteriaWeights,
#'             criteriaMinMax)
#' 
#' 
#' @export PROMETHEEII
PROMETHEEII<-function(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  # This function consists of the (P, I) complete ranking which  is obtained from the net outranking
  # flow which is the balance between the positive and the negative outranking flows.This function returns two matrices P (for Preference relations) and I(for indifference relations).
  #Each matrix contains only 0 and 1. 1 (at the position (i,j) ) means that a_i P a_j (in the matrix P), or a_i I a_j (in the matrix I)
  # and 0 else.  
{ 
  numAlt<-dim(performanceTable)[1] # number of alternatives
  # Call of the function PROMETHEEOutrankingFlows
  result<-PROMETHEEOutrankingFlows(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  posflows<-result[[1]]
  negflows<-result[[2]]
  netflows<-posflows - negflows
  ord <- order(netflows,decreasing = TRUE)
  O <- list()
  prev <- Inf
  j <- 0
  for (i in ord)
  {
    if (netflows[[i]] == prev)
    {
      O[[j]] <- c(O[[j]],names(netflows)[i])
    }
    else
    {
      j <- j + 1
      O[[j]] <- names(netflows)[i]
      prev <- netflows[[i]]
    }
  }
  O
}
