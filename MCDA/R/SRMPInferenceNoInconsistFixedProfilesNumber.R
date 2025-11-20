#' Exact inference of an SRMP model given the number of reference profiles - no
#' inconsistencies
#' 
#' Exact inference approach from pairwise comparisons of alternatives for the
#' SRMP ranking model. This method only outputs a result when an SRMP model
#' consistent with the provided pairwise comparisons exists. The number of
#' reference profiles is fixed and need to be provided. If such a model exists,
#' this method is significantly faster than the one which handles
#' inconsistencies.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param profilesNumber A strictly pozitive numerical value which gives the
#' number of reference profiles in the sought SRMP model.
#' @param preferencePairs A two column matrix containing on each row a pair of
#' alternative names where the first alternative is considered to be strictly
#' preferred to the second.
#' @param indifferencePairs A two column matrix containing on each row a pair
#' of alternative names the two alternatives are considered to indifferent with
#' respect to each other.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param timeLimit Allows to fix a time limit of the execution, in seconds. By
#' default NULL (which corresponds to no time limit).
#' @return The function returns a list containing: \item{criteriaWeights}{The
#' inferred criteria weights.} \item{referenceProfiles}{The inferred reference
#' profiles.} \item{lexicographicOrder}{The inferred lexicographic order of the
#' profiles.} \item{solverStatus}{The solver status as given by glpk.}
#' \item{humanReadableStatus}{A description of the solver status.}
#' @references A-L. OLTEANU, V. MOUSSEAU, W. OUERDANE, A. ROLLAND, Y. ZHENG,
#' Preference Elicitation for a Ranking Method based on Multiple Reference
#' Profiles, forthcoming 2018.
#' @keywords methods
#' @examples
#' 
#' \donttest{
#' performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
#'                           c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
#'                           c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
#'                           c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))
#' 
#' criteriaMinMax <- c("max","max","max")
#' 
#' rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
#'                                 "a13","a14","a15","a16","a17","a18","a19","a20","a21","a22",
#'                                 "a23","a24")
#' 
#' colnames(performanceTable) <- c("c1","c2","c3")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' preferencePairs <- matrix(c("a16","a13","a3","a14","a17","a1","a18","a15","a2","a11","a5",
#'                             "a10","a4","a12","a13","a3","a14","a17","a1","a18","a15","a2",
#'                             "a11","a5","a10","a4","a12","a6"),14,2)
#' indifferencePairs <- matrix(c("a3","a1","a2","a11","a11","a20","a10","a10","a19","a12","a12",
#'                               "a21","a9","a7","a8","a20","a22","a22","a19","a24","a24","a21",
#'                               "a23","a23"),12,2)
#' 
#' result<-SRMPInferenceNoInconsistFixedProfilesNumber(performanceTable, criteriaMinMax, 3,
#'                                                     preferencePairs, indifferencePairs,
#'                                                     alternativesIDs = c("a1","a2","a3","a4",
#'                                                     "a5","a6","a7","a8","a10","a11","a12",
#'                                                     "a14","a16","a17","a18","a19","a20","a21",
#'                                                     "a23","a24"))
#' }
#' @export SRMPInferenceNoInconsistFixedProfilesNumber
SRMPInferenceNoInconsistFixedProfilesNumber <- function(performanceTable, criteriaMinMax, profilesNumber, preferencePairs, indifferencePairs = NULL, alternativesIDs = NULL, criteriaIDs = NULL, timeLimit = NULL){
  
  ## check the input data
  if (!(is.matrix(performanceTable) || is.data.frame(performanceTable))) 
    stop("performanceTable should be a matrix or a data frame")
  
  if (!is.matrix(preferencePairs) || is.data.frame(preferencePairs)) 
    stop("preferencePairs should be a matrix or a data frame")
  
  if (!(is.null(indifferencePairs) || is.matrix(indifferencePairs) || is.data.frame(indifferencePairs))) 
    stop("indifferencePairs should be a matrix or a data frame")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.numeric(profilesNumber)))
    stop("profilesNumber should be numberic")
  
  profilesNumber <- as.integer(profilesNumber)
  
  if (!(is.null(timeLimit)))
  {
    if(!is.numeric(timeLimit))
      stop("timeLimit should be numeric")
    if(timeLimit <= 0)
      stop("timeLimit should be strictly pozitive")
  }
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if(dim(preferencePairs)[2] != 2)
    stop("preferencePairs should have two columns")
  
  if(!is.null(indifferencePairs))
    if(dim(indifferencePairs)[2] != 2)
      stop("indifferencePairs should have two columns")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    preferencePairs <- preferencePairs[(preferencePairs[,1] %in% alternativesIDs) & (preferencePairs[,2] %in% alternativesIDs),]
    if(dim(preferencePairs)[1] == 0)
      preferencePairs <- NULL
    if(!is.null(indifferencePairs))
    {
      indifferencePairs <- indifferencePairs[(indifferencePairs[,1] %in% alternativesIDs) & (indifferencePairs[,2] %in% alternativesIDs),]
      if(dim(indifferencePairs)[1] == 0)
        indifferencePairs <- NULL
    }
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if (is.null(dim(preferencePairs))) 
    stop("preferencePairs is empty or the provided alternativesIDs have filtered out everything from within")
  
  if (!(profilesNumber > 0))
    stop("profilesNumber should be strictly pozitive")
  
  lexicographicOrders <- permn(1:profilesNumber)
  
  startTime <- Sys.time()
  
  result <- (list(humanReadableStatus = "No solution found in the given time limit"))
  
  for(lexicographicOrder in lexicographicOrders)
  {
    currentTime <- Sys.time()
    
    timeLeft <- NULL
    
    if(!is.null(timeLimit))
    {
      timeLeft <- as.double(timeLimit - as.double(currentTime - startTime))
      if(timeLeft < 1)
        return(result)
    }
    
    result <- SRMPInferenceNoInconsistFixedLexicographicOrder(performanceTable, criteriaMinMax, lexicographicOrder, preferencePairs, indifferencePairs, alternativesIDs, criteriaIDs, timeLeft)
    
    if(result$solverStatus == 5)
      return(list(criteriaWeights = result$criteriaWeights, referenceProfiles = result$referenceProfiles, lexicographicOrder = lexicographicOrder, solverStatus = result$solverStatus, humanReadableStatus = result$humanReadableStatus))
  }
  
  return(result)
}
