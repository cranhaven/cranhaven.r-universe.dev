#' MRSort that takes into account large performance differences.
#' 
#' MRSort is a simplified ElectreTRI method that uses the pessimistic
#' assignment rule, without indifference or preference thresholds attached to
#' criteria. LPDMRSort considers both a binary discordance and a binary
#' concordance conditions including several interactions between them.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param categoriesLowerProfiles Matrix containing, in each row, the lower
#' profiles of the categories. The columns are named according to the criteria,
#' and the rows are named according to the categories. The index of the row in
#' the matrix corresponds to the rank of the category.
#' @param categoriesRanks A vector containing the ranks of the categories (1
#' for the best, with higher values for increasingly less preferred
#' categories). The vector needs to be named with the categories names, whereas
#' the ranks need to be a range of values from 1 to the number of categories.
#' @param criteriaWeights Vector containing the weights of the criteria. The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param majorityThreshold The cut threshold for the concordance condition.
#' Should be at least half of the sum of the weights.
#' @param criteriaVetos Matrix containing in each row a vector defining the
#' veto values for the lower profile of the category. NA values mean that no
#' veto is defined. A veto threshold for criterion i and category k represents
#' the performance below which an alternative is forbidden to outrank the lower
#' profile of category k, and thus is forbidden to be assigned to the category
#' k. The rows are named according to the categories, whereas the columns are
#' named according to the criteria.
#' @param criteriaDictators Matrix containing in each row a vector defining the
#' dictator values for the lower profile of the category. NA values mean that
#' no veto is defined.  A dictator threshold for criterion i and category k
#' represents the performance above which an alternative is guaranteed to
#' outrank the lower profile of category k, and thus may no be assigned below
#' category k. The rows are named according to the categories, whereas the
#' columns are named according to the criteria.
#' @param majorityRule String denoting how the vetoes and dictators are
#' combined in order to form the assignment rule.  The values to choose from
#' are "M", "V", "D", "v", "d", "dV", "Dv", "dv".  "M" corresponds to using
#' only the majority rule without vetoes or dictators, "V" considers only the
#' vetoes, "D" only the dictators, "v" is like "V" only that a dictator may
#' invalidate a veto, "d" is like "D" only that a veto may invalidate a
#' dictator, "dV" is like "V" only that if there is no veto we may then
#' consider the dictator, "Dv" is like "D" only that when there is no dictator
#' we may consider the vetoes, while finally "dv" is identical to using both
#' dictator and vetoes only that when both are active they invalidate each
#' other, so the majority rule is considered in that case.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param categoriesIDs Vector containing IDs of categories, according to which
#' the data should be filtered.
#' @return The function returns a vector containing the assignments of the
#' alternatives to the categories.
#' @references Bouyssou, D. and Marchant, T. An axiomatic approach to
#' noncompensatory sorting methods in MCDM, II: more than two categories.
#' European Journal of Operational Research, 178(1): 246--276, 2007.
#' 
#' Meyer, P. and Olteanu, A-L. Integrating large positive and negative
#' performance differences in majority-rule sorting models. European Journal of
#' Operational Research, submitted, 2015.
#' @keywords methods
#' @examples
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(c(10,10,9), c(10,9,10), c(9,10,10), c(9,9,10), 
#'                           c(9,10,9), c(10,9,9), c(10,10,7), c(10,7,10), 
#'                           c(7,10,10), c(9,9,17), c(9,17,9), c(17,9,9), 
#'                           c(7,10,17), c(10,17,7), c(17,7,10), c(7,17,10), 
#'                           c(17,10,7), c(10,7,17), c(7,9,17), c(9,17,7), 
#'                           c(17,7,9), c(7,17,9), c(17,9,7), c(9,7,17))
#' 
#' profilesPerformances <- rbind(c(10,10,10),c(0,0,0))
#' 
#' vetoPerformances <- rbind(c(7,7,7),c(0,0,0))
#' 
#' dictatorPerformances <- rbind(c(17,17,17),c(0,0,0))
#' 
#' rownames(performanceTable) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", 
#'                                 "a8", "a9", "a10", "a11", "a12",  "a13", 
#'                                 "a14", "a15", "a16", "a17", "a18", "a19", 
#'                                 "a20", "a21", "a22", "a23", "a24")
#' 
#' rownames(profilesPerformances) <- c("P","F")
#' 
#' rownames(vetoPerformances) <- c("P","F")
#' 
#' rownames(dictatorPerformances) <- c("P","F")
#' 
#' colnames(performanceTable) <- c("c1","c2","c3")
#' 
#' colnames(profilesPerformances) <- c("c1","c2","c3")
#' 
#' colnames(vetoPerformances) <- c("c1","c2","c3")
#' 
#' colnames(dictatorPerformances) <- c("c1","c2","c3")
#' 
#' lambda <- 0.5
#' 
#' weights <- c(1/3,1/3,1/3)
#' 
#' names(weights) <- c("c1","c2","c3")
#' 
#' categoriesRanks <-c(1,2)
#' 
#' names(categoriesRanks) <- c("P","F")
#' 
#' criteriaMinMax <- c("max","max","max")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' assignments <-rbind(c("P","P","P","F","F","F","F","F","F","F","F","F",
#'                     "F","F","F","F","F","F","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","P","P","P","P","P","P",
#'                     "P","P","P","P","P","P","P","P","P","P","P","P"), 
#'                     c("P","P","P","F","F","F","F","F","F","F","F","F",
#'                     "P","P","P","P","P","P","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","P","P","P","P","P","P",
#'                     "P","P","P","P","P","P","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","F","F","F","P","P","P",
#'                     "F","F","F","F","F","F","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","F","F","F","P","P","P",
#'                     "P","P","P","P","P","P","P","P","P","P","P","P"), 
#'                     c("P","P","P","F","F","F","F","F","F","P","P","P",
#'                     "P","P","P","P","P","P","F","F","F","F","F","F"))
#' 
#' colnames(assignments) <- rownames(performanceTable)
#' 
#' majorityRules <- c("V","D","v","d","dV","Dv","dv")
#' 
#' for(i in 1:7)
#' {
#'   ElectreAssignments<-LPDMRSort(performanceTable, profilesPerformances, 
#'                                 categoriesRanks,
#'                                 weights, criteriaMinMax, lambda, 
#'                                 criteriaVetos=vetoPerformances,
#'                                 criteriaDictators=dictatorPerformances,
#'                                 majorityRule = majorityRules[i])
#' 
#'   print(all(ElectreAssignments == assignments[i,]))
#' }
#' 
#' @export LPDMRSort
LPDMRSort <- function(performanceTable, categoriesLowerProfiles, categoriesRanks, criteriaWeights, criteriaMinMax, majorityThreshold, criteriaVetos = NULL, criteriaDictators = NULL, majorityRule = "M", alternativesIDs = NULL, criteriaIDs = NULL, categoriesIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.matrix(categoriesLowerProfiles)))
    stop("categoriesLowerProfiles should be a matrix")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if (!(is.null(categoriesIDs) || is.vector(categoriesIDs)))
    stop("categoriesIDs should be a vector")
  
  if (!(is.null(criteriaVetos) || is.matrix(criteriaVetos)))
    stop("criteriaVetos should be a matrix")
  
  if (!(is.null(criteriaDictators) || is.matrix(criteriaDictators)))
    stop("criteriaDictators should be a matrix")
  
  if (!is.character(majorityRule))
    stop("majorityRule should be a string")
  else if (!(majorityRule %in% c("M","V","D","v","d","dV","Dv","dv")))
    stop("majorityRule needs to take values in {'M','V','D','v','d','dV','Dv','dv'}")
  
  if (majorityRule %in% c("V","v","dV","Dv","dv") && is.null(criteriaVetos))
    stop("majorityRule requires non-NULL criteriaVetos")
  
  if (majorityRule %in% c("D","d","dV","Dv","dv") && is.null(criteriaDictators))
    stop("majorityRule requires non-NULL criteriaDictators")
  
  # check if we have a lower profile for the worst category
  
  worstCat <- names(categoriesRanks)[categoriesRanks == length(categoriesRanks)]
  
  if(!(worstCat %in% rownames(categoriesLowerProfiles)))
  {
    categoriesLowerProfiles <- rbind(categoriesLowerProfiles, rep(NA,length(criteriaMinMax)))
    
    rownames(categoriesLowerProfiles)[length(categoriesRanks)] <- worstCat
  }
  
  if (!is.null(criteriaVetos))
  {
    if(!(worstCat %in% rownames(criteriaVetos)))
    {
      criteriaVetos <- rbind(criteriaVetos, rep(NA,length(criteriaMinMax)))
      
      rownames(criteriaVetos)[length(categoriesRanks)] <- worstCat
    }
  }
  
  if (!is.null(criteriaDictators))
  {
    if(!(worstCat %in% rownames(criteriaDictators)))
    {
      criteriaDictators <- rbind(criteriaDictators, rep(NA,length(criteriaMinMax)))
      
      rownames(criteriaDictators)[length(categoriesRanks)] <- worstCat
    }
  }
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,,drop=FALSE]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs,drop=FALSE]
    criteriaWeights <- criteriaWeights[criteriaIDs,drop=FALSE]
    criteriaMinMax <- criteriaMinMax[criteriaIDs,drop=FALSE]
    categoriesLowerProfiles <- categoriesLowerProfiles[,criteriaIDs,drop=FALSE]
  }
  
  if ((!is.null(criteriaIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[,criteriaIDs,drop=FALSE] 
  }
  
  if ((!is.null(criteriaIDs)) && (!is.null(criteriaDictators))){
    criteriaDictators <- criteriaDictators[,criteriaIDs,drop=FALSE]
  }
  
  if ((!is.null(categoriesIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[categoriesIDs,,drop=FALSE]
  }
    
  if (!is.null(categoriesIDs)){
    categoriesLowerProfiles <- categoriesLowerProfiles[categoriesIDs,,drop=FALSE]
  }
  
  if ((!is.null(categoriesIDs)) && (!is.null(criteriaDictators))){
    criteriaDictators <- criteriaDictators[categoriesIDs,,drop=FALSE]
  }  
  
  # data is filtered, check for some data consistency
  
  
  if (!is.null(categoriesIDs)){
    # filter out categories
    categoriesRanks <- categoriesRanks[names(categoriesRanks) %in% categoriesIDs]
    # check if we took out all categories
    if(length(categoriesRanks) == 0)
      stop('categoriesIDs have filtered out all categories')
    # order the remaining ones
    categoriesRanks <- sort(categoriesRanks)
    # store their order
    catOrder <- names(categoriesRanks)
    # adjust their indices to a range from 1 to the number of remaining categories
    categoriesRanks <- 1:length(categoriesRanks)
    # rename them
    names(categoriesRanks) <- catOrder
  }
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  # -------------------------------------------------------
  
  getCategory <- function(i)
  {
    for (k in (numCat-1):1)
    {
      cat <- names(categoriesRanks)[categoriesRanks == k]
      
      weightedSum <- 0
      
      for (crit in names(criteriaMinMax))
      {
        if (criteriaMinMax[crit] == "min")
        {
          if (performanceTable[i,crit] %<=% categoriesLowerProfiles[cat,crit])
            weightedSum <- weightedSum + criteriaWeights[crit]
        }
        else
        {
          if (performanceTable[i,crit] %>=% categoriesLowerProfiles[cat,crit])
            weightedSum <- weightedSum + criteriaWeights[crit]
        }
      }
      
      vetoActive <- FALSE
      
      if(majorityRule %in% c("V","v","d","dV","Dv","dv"))
      {
        for (crit in names(criteriaMinMax))
        {
          if(!is.na(criteriaVetos[cat,crit]) & !is.null(criteriaVetos[cat,crit]))
          {
            if (criteriaMinMax[crit] == "min")
            {
              if (performanceTable[i,crit] %>=% criteriaVetos[cat,crit])
              {
                vetoActive <- TRUE
                break
              }
            }
            else
            {
              if (performanceTable[i,crit] %<=% criteriaVetos[cat,crit])
              {
                vetoActive <- TRUE
                break
              }
            }
          }
        }
      }
      
      dictatorActive <- FALSE
      
      
      if(majorityRule %in% c("D","v","d","dV","Dv","dv"))
      {
        for (crit in names(criteriaMinMax))
        {
          if(!is.na(criteriaDictators[cat,crit]) & !is.null(criteriaDictators[cat,crit]))
          {
            if (criteriaMinMax[crit] == "min")
            {
              if (performanceTable[i,crit] %<=% criteriaDictators[cat,crit])
              {
                dictatorActive <- TRUE
                break
              }
            }
            else
            {
              if (performanceTable[i,crit] %>=% criteriaDictators[cat,crit])
              {
                dictatorActive <- TRUE
                break
              }
            }
          }
        }
      }
      # stopping condition
      if(majorityRule == 'M')
      {
        if(weightedSum < majorityThreshold)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'V')
      {
        if(weightedSum < majorityThreshold || vetoActive)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'D')
      {
        if(weightedSum < majorityThreshold && !dictatorActive)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'v')
      {
        if(weightedSum < majorityThreshold || (vetoActive && !dictatorActive))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'd')
      {
        if(weightedSum < majorityThreshold && (!dictatorActive || vetoActive))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      if(majorityRule == 'dV')
      {
        if((weightedSum < majorityThreshold && !dictatorActive) || vetoActive)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      if(majorityRule == 'Dv')
      {
        if(!dictatorActive && (vetoActive || weightedSum < majorityThreshold))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      if(majorityRule == 'dv')
      {
        if((vetoActive && !dictatorActive) || (weightedSum < majorityThreshold && ((vetoActive && dictatorActive) || (!vetoActive && !dictatorActive))))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
    }
    # better than all profiles -> top categ
    return(names(categoriesRanks)[categoriesRanks == 1])
  }
  
  assignments <- sapply(1:numAlt, getCategory)
  
  names(assignments) <- rownames(performanceTable)
  
  return(assignments)
}
