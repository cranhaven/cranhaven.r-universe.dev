#' Identify veto profiles evaluations that have an impact on the final
#' assignments of MRSort with large performance differences
#' 
#' MRSort is a simplified ELECTRE-TRI approach which assigns alternatives to a
#' set of ordered categories using delimiting profiles evaluations. In this
#' case, we also take into account large performance differences. This method
#' is used to identify which veto profiles evaluations have an impact on the
#' final assignment of at least one of the input alternatives.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table.  Each row corresponds to an alternative, and each column to a
#' criterion.  Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param assignments A vector containing the category to which each
#' alternative is assigned. The vector needs to be named using the alternatives
#' IDs.
#' @param categoriesRanks A vector containing the ranks of the categories (1
#' for the best, with higher values for increasingly less preferred
#' categories). The vector needs to be named with the categories names, whereas
#' the ranks need to be a range of values from 1 to the number of categories.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param majorityThreshold The majority threshold needed to determine when a
#' coalition of criteria is sufficient in order to validate an outranking
#' relation.
#' @param criteriaWeights Vector containing the weights of the criteria.  The
#' elements are named according to the IDs of the criteria.
#' @param profilesPerformances Matrix containing, in each row, the lower
#' profiles of the categories. The columns are named according to the criteria,
#' and the rows are named according to the categories. The index of the row in
#' the matrix corresponds to the rank of the category.
#' @param vetoPerformances Matrix containing in each row a vector defining the
#' veto values for the lower profile of the category. NA values mean that no
#' veto is defined.  A veto threshold for criterion i and category k represents
#' the performance below which an alternative is forbidden to outrank the lower
#' profile of category k, and thus is forbidden to be assigned to the category
#' k.  The rows are named according to the categories, whereas the columns are
#' named according to the criteria.
#' @param dictatorPerformances Matrix containing in each row a vector defining
#' the dictator values for the lower profile of the category. NA values mean
#' that no dictator is defined.  A dictator threshold for criterion i and
#' category k represents the performance above which an alternative outranks
#' the lower profile of category k regardless of the size of the coalition of
#' criteria in favor of this statement.  The rows are named according to the
#' categories, whereas the columns are named according to the criteria. By
#' default no dictator profiles are needed for this method.
#' @param majorityRule String denoting how the vetoes and dictators are
#' combined in order to form the assignment rule.  The values to choose from
#' are "V", "v", "d", "dV", "Dv", "dv".  "V" considers only the vetoes, "v" is
#' like "V" only that a dictator may invalidate a veto, "d" is like "D" only
#' that a veto may invalidate a dictator, "dV" is like "V" only that if there
#' is no veto we may then consider the dictator, "Dv" is like "D" only that
#' when there is no dictator we may consider the vetoes, while finally "dv" is
#' identical to using both dictator and vetoes only that when both are active
#' they invalidate each other, so the majority rule is considered in that case.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @return The function returns a matrix containing TRUE/FALSE inficators for
#' each evaluation of the veto profiles.
#' @keywords methods
#' @examples
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(
#'   c(1,27,1),
#'   c(6,20,1),
#'   c(2,20,0),
#'   c(6,40,0),
#'   c(30,10,3))
#' 
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' 
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # lower profiles of the categories (best category in the first position of the list)
#' 
#' categoriesLowerProfiles <- rbind(c(3, 11, 3),c(7, 25, 2),c(NA,NA,NA))
#' 
#' colnames(categoriesLowerProfiles) <- colnames(performanceTable)
#' 
#' rownames(categoriesLowerProfiles)<-c("Good","Medium","Bad")
#' 
#' # the order of the categories, 1 being the best
#' 
#' categoriesRanks <-c(1,2,3)
#' 
#' names(categoriesRanks) <- c("Good","Medium","Bad")
#' 
#' # criteria to minimize or maximize
#' 
#' criteriaMinMax <- c("min","min","max")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' # dictators
#' 
#' criteriaDictators <- rbind(c(1, 1, -1),c(1, 20, 0),c(NA,NA,NA))
#' 
#' colnames(criteriaDictators) <- colnames(performanceTable)
#' rownames(criteriaDictators) <- c("Good","Medium","Bad")
#' 
#' # vetos
#' 
#' criteriaVetos <- rbind(c(9, 50, 5),c(50, 50, 5),c(NA,NA,NA))
#' 
#' colnames(criteriaVetos) <- colnames(performanceTable)
#' rownames(criteriaVetos) <- c("Good","Medium","Bad")
#' 
#' # weights
#' 
#' criteriaWeights <- c(1/6,3/6,2/6)
#' 
#' names(criteriaWeights) <- colnames(performanceTable)
#' 
#' # assignments
#' 
#' assignments <- c("Good","Medium","Bad","Bad","Bad")
#' 
#' 
#' # LPDMRSortIndetifyUsedVetoProfiles
#' 
#' used<-LPDMRSortIdentifyUsedVetoProfiles(performanceTable, assignments,
#'                                         categoriesRanks, criteriaMinMax,
#'                                         0.5, criteriaWeights,
#'                                         categoriesLowerProfiles,
#'                                         criteriaVetos,
#'                                         criteriaDictators,
#'                                         "dv")
#' 
#' @export LPDMRSortIdentifyUsedVetoProfiles
LPDMRSortIdentifyUsedVetoProfiles <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, majorityThreshold, criteriaWeights, profilesPerformances, vetoPerformances, dictatorPerformances = NULL, majorityRule = 'V', alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!is.numeric(majorityThreshold))
    stop("majorityThreshold should be numeric")
  
  if (length(majorityThreshold) > 1)
    stop("majorityThreshold should be a single number")
  
  if (!(majorityThreshold %<=% 1 && majorityThreshold %>=% 0.5))
    stop("majorityThreshold should be a value between 0.5 and 1")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.matrix(profilesPerformances)))
    stop("profilesPerformances should be a matrix")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if (!is.matrix(vetoPerformances))
    stop("vetoPerformances should be a matrix")
  
  if (!(is.null(dictatorPerformances) || is.matrix(dictatorPerformances)))
    stop("dictatorPerformances should be a matrix")
  
  if (!is.character(majorityRule))
    stop("majorityRule should be a string")
  else if (!(majorityRule %in% c("V","v","d","dV","Dv","dv")))
    stop("majorityRule needs to take values in {'V','v','d','dV','Dv','dv'}")
  
  if (majorityRule %in% c("D","d","dV","Dv","dv") && is.null(dictatorPerformances))
    stop("majorityRule requires non-NULL dictatorPerformances")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,,drop = FALSE]
    assignments <- assignments[alternativesIDs,drop = FALSE]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs,drop = FALSE]
    criteriaMinMax <- criteriaMinMax[criteriaIDs,drop = FALSE]
  }
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  used <- matrix(rep(FALSE,numCat * numCrit),nrow=numCat,ncol=numCrit)
  
  rownames(used) <- names(sort(categoriesRanks))
  colnames(used) <- colnames(performanceTable)
  
  for (i in 1:numAlt)
  {
    for (k in (numCat-1):1)
    {
      cat <- names(categoriesRanks[categoriesRanks == k])
      
      weightedSum <- 0
      
      for (j in 1:numCrit)
      {
        if (criteriaMinMax[j] == "min")
        {
          if (performanceTable[i,j] %<=% profilesPerformances[cat,j])
            weightedSum <- weightedSum + criteriaWeights[j]
        }
        else
        {
          if (performanceTable[i,j] %>=% profilesPerformances[cat,j])
            weightedSum <- weightedSum + criteriaWeights[j]
        }
      }
      
      vetoActive <- rep(FALSE, numCrit)
      dictatorActive <- rep(FALSE, numCrit)
      
      for (j in 1:numCrit)
      {
        if (criteriaMinMax[j] == "min")
        {
          if (!is.null(vetoPerformances[cat,j]) && !is.na(vetoPerformances[cat,j]))
            if (performanceTable[i,j] %>=% vetoPerformances[cat,j])
              vetoActive[j] <- TRUE
          if(!is.null(dictatorPerformances))
          {
            if (!is.null(dictatorPerformances[cat,j]) && !is.na(dictatorPerformances[cat,j]))
              if (performanceTable[i,j] %<=% dictatorPerformances[cat,j])
                dictatorActive[j] <- TRUE
          }
        }
        else
        {
          if (!is.null(vetoPerformances[cat,j]) && !is.na(vetoPerformances[cat,j]))
            if (performanceTable[i,j] %<=% vetoPerformances[cat,j])
              vetoActive[j] <- TRUE
          if(!is.null(dictatorPerformances))
          {
            if (!is.null(dictatorPerformances[cat,j]) && !is.na(dictatorPerformances[cat,j]))
              if (performanceTable[i,j] %>=% dictatorPerformances[cat,j])
                dictatorActive[j] <- TRUE
          }
        }
      }
      # stopping condition
      if(majorityRule == 'V')
      {
        if(weightedSum < majorityThreshold || any(vetoActive))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'v')
      {
        if(weightedSum < majorityThreshold || (any(vetoActive) && !any(dictatorActive)))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold) || any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'd')
      {
        if(weightedSum < majorityThreshold && (!any(dictatorActive) || any(vetoActive)))
        {
          # was the veto necessary ?
          if(any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'dV')
      {
        if((weightedSum < majorityThreshold && !any(dictatorActive)) || any(vetoActive))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold) || any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'Dv')
      {
        if(!any(dictatorActive) && (any(vetoActive) || weightedSum < majorityThreshold))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'dv')
      {
        if((any(vetoActive) && !any(dictatorActive)) || (weightedSum < majorityThreshold && ((any(vetoActive) && any(dictatorActive)) || (!any(vetoActive) && !any(dictatorActive)))))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold) || any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
    }
  }
  
  return(used)
}
