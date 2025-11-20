#' Weighted sum of evaluations of alternatives.
#' 
#' Computes the weighted sum of the evaluations of alternatives, stored in a
#' performance table, with respect to a vector of criteria weights.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param criteriaWeights Vector containing the weights of the criteria. The
#' elements are named according to the IDs of the criteria.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the performance table should be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' performance table should be filtered.
#' @return The function returns a vector containing the weighted sum of the
#' alternatives with respect to the criteria weights.
#' @keywords methods
#' @examples
#' alts <- paste0("alt", 1:4)
#' crit <- paste0("x", 1:3)
#' performanceTable <- matrix(runif(length(alts)*length(crit)), 
#'                            nrow=length(alts), ncol=length(crit), 
#'                            dimnames=list(alts, crit))
#' weights <- setNames(c(1,2,3), crit)
#' # Overall
#' weightedSum(performanceTable, weights)
#' # Subset of alteratives and criteria
#' weightedSum(performanceTable, weights, 
#'             alternativesIDs=c("alt2","alt3"), criteriaIDs=c("x2","x3"))
#' 
#' @export weightedSum
weightedSum <- function(performanceTable, criteriaWeights, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performance table, should be a matrix or a data frame")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteria weights should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteria IDs should be in a vector")
  
  ## filter the performance table and the criteria according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)) performanceTable <- performanceTable[alternativesIDs,]
  
  if (!is.null(criteriaIDs)) performanceTable <- performanceTable[,criteriaIDs]
  
  if (!is.null(criteriaIDs)) criteriaWeights <- criteriaWeights[criteriaIDs]
  
  ## transform performanceTable to matrix
  
  performanceTable<-as.matrix(performanceTable)  
  
  ## now calculate the weighted sum
  
  out<-c()
  
  for (i in 1:dim(performanceTable)[1]){
    out<-rbind(out, crossprod(performanceTable[i,],criteriaWeights))
  }
  
  out<-as.vector(out)
  
  names(out) <- rownames(performanceTable)
  
  return(out)
}

