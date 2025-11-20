#' Outranking flows for the PROMETHEE methods
#' 
#' This function computes the positive and negative outranking flows for the
#' PROMETHEE methods. It takes as input a performance table and converts the
#' evaluations to preference indices based on the given function types and
#' parameters for each criterion.
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
#' @return The function returns two vectors: The first one contains the
#' positive outranking flows and the second one contains the negative
#' outranking flows.
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
#' 
#' # Outranking flows
#' 
#' outrankingFlows<-PROMETHEEOutrankingFlows(performanceTable, preferenceFunction,
#'                                           preferenceThreshold,indifferenceThreshold,
#'                                           gaussParameter,criteriaWeights,
#'                                           criteriaMinMax)
#' 
#' 
#' @export PROMETHEEOutrankingFlows
PROMETHEEOutrankingFlows<- function(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  
{ 
  numAlt<-dim(performanceTable)[1] # number of alternatives
  outrankingFlowsPos<-rep(0,numAlt)  #the positive outranking flow
  outrankingFlowsNeg<-rep(0,numAlt)  #the negative outranking flow
  preferenceTable<-PROMETHEEPreferenceIndices(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  for(i in (1:numAlt)){
    for(j in (1:numAlt)){
      outrankingFlowsPos[i] <- outrankingFlowsPos[i]+preferenceTable[i,j]
      outrankingFlowsNeg[i] <- outrankingFlowsNeg[i]+preferenceTable[j,i]
    }
    outrankingFlowsPos[i] <- outrankingFlowsPos[i]/numAlt
    outrankingFlowsNeg[i] <- outrankingFlowsNeg[i]/numAlt
  }
  names(outrankingFlowsPos) = rownames(preferenceTable)
  names(outrankingFlowsNeg) = rownames(preferenceTable)
  list(outrankingFlowsPos=outrankingFlowsPos,outrankingFlowsNeg=outrankingFlowsNeg)
}
