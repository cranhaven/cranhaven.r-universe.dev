#' SRMP: a simple ranking method using reference profiles
#' 
#' SRMP is a ranking method that uses dominating reference profiles, in a given
#' lexicographic ordering, in order to output a total preorder of a set of
#' alternatives.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param referenceProfiles Matrix containing, in each row, the reference
#' profiles. The columns are named according to the criteria.
#' @param lexicographicOrder A vector containing the indexes of the reference
#' profiles in a given order. This vetor needs to be of the same length as the
#' number of rows in referenceProfiles and it has to contain a permutation of
#' the indices of these rows.
#' @param criteriaWeights Vector containing the weights of the criteria. The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @return The function returns a vector containing the ranks of the
#' alternatives (the higher the better).
#' @references A. Rolland. Procédures d’agrégation ordinale de préférences avec
#' points de référence pour l’aide a la décision. PhD thesis, Université Paris
#' VI, 2008.
#' @keywords methods
#' @examples
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
#'                           c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
#'                           c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
#'                           c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))
#' 
#' referenceProfiles <- rbind(c(5,5,5),c(10,10,10),c(15,15,15))
#' 
#' lexicographicOrder <- c(2,1,3)
#' 
#' weights <- c(0.2,0.44,0.36)
#' 
#' criteriaMinMax <- c("max","max","max")
#' 
#' rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
#'                                 "a13","a14","a15","a16","a17","a18","a19","a20","a21","a22",
#'                                 "a23","a24")
#' 
#' colnames(performanceTable) <- c("c1","c2","c3")
#' 
#' colnames(referenceProfiles) <- c("c1","c2","c3")
#' 
#' names(weights) <- c("c1","c2","c3")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' expectedpreorder <- list('a16','a13',c('a3','a9'),'a14','a17',c('a1','a7'),'a18','a15',
#'                          c('a2','a8'),c('a11','a20','a22'),'a5',c('a10','a19','a24'),
#'                          'a4',c('a12','a21','a23'),'a6')
#' 
#' preorder<-SRMP(performanceTable, referenceProfiles, lexicographicOrder, weights, criteriaMinMax)
#' 
#' @export SRMP
SRMP <- function(performanceTable, referenceProfiles, lexicographicOrder, criteriaWeights, criteriaMinMax, alternativesIDs = NULL, criteriaIDs = NULL){
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.matrix(referenceProfiles)))
    stop("referenceProfiles should be a matrix")
  
  if (!(is.vector(lexicographicOrder)))
    stop("lexicographicOrder should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaWeights <- criteriaWeights[criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
    referenceProfiles <- referenceProfiles[,criteriaIDs]
  }
  
  numCrit <- dim(performanceTable)[2]
  numAlt <- dim(performanceTable)[1]
  numRefProf <- dim(referenceProfiles)[1]
  
  # data is filtered, check for some data consistency
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if(length(lexicographicOrder) != numRefProf)
    stop("lexicographicOrder does not contain the same number of profiles as referenceProfiles")
  
  if(!all(sort(lexicographicOrder) == 1:numRefProf))
    stop("lexicographicOrder should contain the indices of referenceProfiles")
  
  if(numRefProf > 1)
    for(k1 in 1:(numRefProf-1))
      for(k2 in (k1+1):numRefProf)
        for (i in 1:numCrit)
        {
          if (criteriaMinMax[i] == "max")
          {
            if(referenceProfiles[k1,i] > referenceProfiles[k2,i])
              stop("referenceProfiles should be ordered from the one with the worst performances to the one with the best; they should also be in a dominance relationship")
          }
          else
          {
            if(referenceProfiles[k1,i] < referenceProfiles[k2,i])
              stop("referenceProfiles should be ordered from the one with the worst performances to the one with the best; they should also be in a dominance relationship")
          }
        }
  
  # -------------------------------------------------------
  
  outranking <- function(alternativePerformances1, alternativePerformances2, profilePerformances, criteriaWeights, criteriaMinMax){
    for (k in lexicographicOrder)
    {
      weightedSum1 <- 0
      weightedSum2 <- 0
      for (i in 1:numCrit)
      {
        if (criteriaMinMax[i] == "min")
        {
          if (alternativePerformances1[[i]] %<=% profilePerformances[[k,i]])
            weightedSum1 = weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[[i]] %<=% profilePerformances[[k,i]])
            weightedSum2 = weightedSum2 + criteriaWeights[i]
        }
        else
        {
          if (alternativePerformances1[[i]] %>=% profilePerformances[[k,i]])
            weightedSum1 = weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[[i]] %>=% profilePerformances[[k,i]])
            weightedSum2 = weightedSum2 + criteriaWeights[i]
        }
      }
      
      # can we differentiate between the two alternatives?
      if(weightedSum1 > weightedSum2)
        return(1)
      else if(weightedSum1 < weightedSum2)
        return(-1)
    }
    # could not differentiate between the alternatives
    return(0)
  }
  
  alternativesValues <- rep(1, numAlt)
  
  names(alternativesValues) <- rownames(performanceTable)
  
  for (i in 2:numAlt)
  {
    minVal <- 0
    
    maxVal <- i
    
    for(j in 1:(i-1))
    {
      #print(c('j',j))
      #print(alternativesValues)
      if(alternativesValues[j] >= minVal && alternativesValues[j] <= maxVal)
      {
        comparison <- outranking(performanceTable[i,],performanceTable[j,],referenceProfiles, criteriaWeights, criteriaMinMax)
        
        if(comparison == 1)
        {
          # i is better than j
          minVal <- alternativesValues[j] + 1
        }
        else if(comparison == 0)
        {
          # i is the same as j
          alternativesValues[i] <- alternativesValues[j]
          
          minVal <- -1
          
          break
        }
        else
        {
          # i is worse than j
          maxVal <- alternativesValues[j] - 1
        }
      }
    }
    
    if(minVal == i)
      alternativesValues[i] <- i
    else if(minVal == 0)
      alternativesValues <- sapply(1:numAlt, function(index){if(index<i) (alternativesValues[index] + 1) else alternativesValues[index]})
    else if(minVal >= 0)
    {
      alternativesValues <- sapply(1:numAlt, function(index){if(index<i && alternativesValues[index] >= minVal) (alternativesValues[index] + 1) else alternativesValues[index]})
      
      alternativesValues[i] <- minVal
    }
  }
  
  return(alternativesValues)
}
