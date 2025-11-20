#' Electre TRI-like sorting method axiomatized by Bouyssou and Marchant.
#' 
#' This simplification of the Electre TRI method uses the pessimistic
#' assignment rule, without indifference or preference thresholds attached to
#' criteria.  Only a binary discordance condition is considered, i.e. a veto
#' forbids an outranking in any possible concordance situation, or not.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table.  Each row corresponds to an alternative, and each column to a
#' criterion.  Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param categoriesLowerProfiles Matrix containing, in each row, the lower
#' profiles of the categories.  The columns are named according to the
#' criteria, and the rows are named according to the categories.  The index of
#' the row in the matrix corresponds to the rank of the category.
#' @param categoriesRanks A vector containing the ranks of the categories (1
#' for the best, with higher values for increasingly less preferred
#' categories). The vector needs to be named with the categories names, whereas
#' the ranks need to be a range of values from 1 to the number of categories.
#' @param criteriaWeights Vector containing the weights of the criteria.  The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param majorityThreshold The cut threshold for the concordance condition.
#' Should be at least half of the sum of the weights.
#' @param criteriaVetos Matrix containing in each row a vector defining the
#' veto values for the lower profile of the category. NA values mean that no
#' veto is defined.  A veto threshold for criterion i and category k represents
#' the performance below which an alternative is forbidden to outrank the lower
#' profile of category k, and thus is forbidden to be assigned to the category
#' k.  The rows are named according to the categories, whereas the columns are
#' named according to the criteria.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param categoriesIDs Vector containing IDs of categories, according to which
#' the data should be filtered.
#' @return The function returns a vector containing the assignments of the
#' alternatives to the categories.
#' @references Bouyssou, D. and Marchant, T. An axiomatic approach to
#' noncompen- satory sorting methods in MCDM, II: more than two categories.
#' European Journal of Operational Research, 178(1): 246--276, 2007.
#' @keywords methods
#' @examples
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(
#'   c(1,10,1),
#'   c(4,20,2),
#'   c(2,20,0),
#'   c(6,40,0),
#'   c(30,30,3))
#' 
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' 
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # lower profiles of the categories 
#' # (best category in the first position of the list)
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
#' # vetos
#' 
#' criteriaVetos <- rbind(c(10, NA, NA),c(NA, NA, 1),c(NA,NA,NA))
#' 
#' colnames(criteriaVetos) <- colnames(performanceTable)
#' rownames(criteriaVetos) <- c("Good","Medium","Bad")
#' 
#' # weights
#' 
#' criteriaWeights <- c(1,3,2)
#' 
#' names(criteriaWeights) <- colnames(performanceTable)
#' 
#' 
#' # MRSort
#' 
#' assignments<-MRSort(performanceTable, categoriesLowerProfiles,
#'                     categoriesRanks,criteriaWeights,
#'                     criteriaMinMax, 3, 
#'                     criteriaVetos = criteriaVetos)
#'  
#' print(assignments)
#' 
#' # un peu de filtrage
#' 
#' assignments<-MRSort(performanceTable, categoriesLowerProfiles, 
#'                     categoriesRanks, criteriaWeights,
#'                     criteriaMinMax, 2,
#'                     categoriesIDs = c("Medium","Bad"), 
#'                     criteriaIDs = c("Price","Time"), 
#'                     alternativesIDs = c("RER", "BUS"))
#' 
#' print(assignments)
#' 
#' 
#' @export MRSort
MRSort <- function(performanceTable, categoriesLowerProfiles, categoriesRanks, criteriaWeights, criteriaMinMax, majorityThreshold, criteriaVetos = NULL, alternativesIDs = NULL, criteriaIDs = NULL, categoriesIDs = NULL){
  
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
  
  if ((!is.null(categoriesIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[categoriesIDs,,drop=FALSE]
  }
    
  if (!is.null(categoriesIDs)){
    categoriesLowerProfiles <- categoriesLowerProfiles[categoriesIDs,,drop=FALSE]
  }
  
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
      
      if(!is.null(criteriaVetos))
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
      
      # stopping condition
      if(weightedSum < majorityThreshold || vetoActive)
        return(names(categoriesRanks)[categoriesRanks == (k + 1)])
    }
    # better than all profiles -> top categ
    return(names(categoriesRanks)[categoriesRanks == 1])
  }
  
  assignments <- sapply(1:numAlt, getCategory)
  
  names(assignments) <- rownames(performanceTable)
  
  return(assignments)
}
