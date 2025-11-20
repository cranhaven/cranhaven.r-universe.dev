#' Assign alternatives to categories according to thresholds.
#' 
#' Assign alternatives to categories according to thresholds representing the
#' lower bounds of the categories.
#' 
#' 
#' @param alternativesScores Vector representing the overall scores of the
#' alternatives. The elements are named according to the IDs of the
#' alternatives.
#' @param categoriesLowerBounds Vector containing the lower bounds of the
#' categories. An alternative is assigned to a category if it's score is higher
#' or equal to the lower bound of the category, and strictly lower to the lower
#' bound of the category above.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param categoriesIDs Vector containing IDs of categories, according to which
#' the data should be filtered.
#' @return The function returns a vector containing the assignments of the
#' alternatives to the categories.
#' @keywords methods
#' @examples
#' 
#' # the separation threshold
#' 
#' epsilon <-0.05
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(
#'   c(3,10,1),
#'   c(4,20,2),
#'   c(2,20,0),
#'   c(6,40,0),
#'   c(30,30,3))
#' 
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' 
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # ranks of the alternatives
#' 
#' alternativesAssignments <- c("good","medium","medium","bad","bad")
#' 
#' names(alternativesAssignments) <- row.names(performanceTable)
#' 
#' # criteria to minimize or maximize
#' 
#' criteriaMinMax <- c("min","min","max")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' # number of break points for each criterion
#' 
#' criteriaNumberOfBreakPoints <- c(3,4,4)
#' 
#' names(criteriaNumberOfBreakPoints) <- colnames(performanceTable)
#' 
#' # ranks of the categories
#' 
#' categoriesRanks <- c(1,2,3)
#' 
#' names(categoriesRanks) <- c("good","medium","bad")
#' 
#' x<-UTADIS(performanceTable, criteriaMinMax, criteriaNumberOfBreakPoints, 
#'             alternativesAssignments, categoriesRanks,0.1)
#' 
#' npt <- applyPiecewiseLinearValueFunctionsOnPerformanceTable(x$valueFunctions, 
#'                                                              performanceTable)
#' 
#' scores <- weightedSum(npt, c(1,1,1))
#' 
#' # add a lower bound for the "bad" category
#' 
#' lbs <- c(x$categoriesLBs,0)
#' 
#' names(lbs) <- c(names(x$categoriesLBs),"bad")
#' 
#' assignments<-assignAlternativesToCategoriesByThresholds(scores,lbs)
#' 
#' 
#' 
#' @export assignAlternativesToCategoriesByThresholds
assignAlternativesToCategoriesByThresholds <- function(alternativesScores, categoriesLowerBounds, alternativesIDs = NULL, categoriesIDs=NULL){
  
  ## check the input data
  
  if (!(is.vector(alternativesScores)))
    stop("alternatives scores should be in a vector")
    
  if (!(is.vector(categoriesLowerBounds)))
    stop("categories lower bounds should be in a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(categoriesIDs) || is.vector(categoriesIDs)))
    stop("categories IDs should be in a vector")
  
  ## filter the data according to the given criteria and alternatives
  
  if (!is.null(alternativesIDs)){
    alternativesScores <- alternativesScores[alternativesIDs]
  } 
  
  if (!is.null(categoriesIDs)){
    categoriesLowerBounds <- categoriesLowerBounds[categoriesIDs]
  } 
  
  # -------------------------------------------------------
    
  numAlt <- length(alternativesScores)
  
  categoriesIDs <- names(categoriesLowerBounds)
  
  numCat <- length(categoriesIDs)
  
  if (numCat<1)
    stop("no categories left after filtering, should be at least one")
  
  # -------------------------------------------------------
  
  assignments <- rep(NA,length(alternativesScores))
  names(assignments) <- names(alternativesScores)
  
  sortedCategoriesLowerBounds <- sort(categoriesLowerBounds, decreasing=T)
  
  for (i in 1:length(alternativesScores)){
    
    for (j in 1:length(categoriesLowerBounds)){
      if (j==1){
        if (alternativesScores[i]>=categoriesLowerBounds[j])
          assignments[names(alternativesScores[i])] <- names(categoriesLowerBounds)[j]
      }
      else
      {
        if ((alternativesScores[i]>=categoriesLowerBounds[j]) & alternativesScores[i]<categoriesLowerBounds[j-1])
          assignments[names(alternativesScores[i])] <- names(categoriesLowerBounds)[j]
      }   
    }
  }
  
  return(assignments)
}
