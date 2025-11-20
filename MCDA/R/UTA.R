#' UTA method to elicit value functions.
#' 
#' Elicits value functions from a ranking of alternatives, according to the UTA
#' method.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria. "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized). The elements are named according to the IDs of the
#' criteria.
#' @param criteriaNumberOfBreakPoints Vector containing the number of
#' breakpoints of the piecewise linear value functions to be determined.
#' Minimum 2. The elements are named according to the IDs of the criteria.
#' @param epsilon Numeric value containing the minimal difference in value
#' between two consecutive alternatives in the final ranking.
#' @param alternativesRanks Optional vector containing the ranks of the
#' alternatives. The elements are named according to the IDs of the
#' alternatives. If not present, then at least one of alternativesPreferences
#' or alternativesIndifferences should be given.
#' @param alternativesPreferences Optional matrix containing the preference
#' constraints on the alternatives. Each line of the matrix corresponds to a
#' constraint of the type alternative a is strictly preferred to alternative b.
#' If not present, then either alternativesRanks or alternativesIndifferences
#' should be given.
#' @param alternativesIndifferences Optional matrix containing the indifference
#' constraints on the alternatives. Each line of the matrix corresponds to a
#' constraint of the type alternative a is indifferent to alternative b. If not
#' present, then either alternativesRanks or alternativesPreferences should be
#' given.
#' @param criteriaLBs Vector containing the lower bounds of the criteria to be
#' considered for the elicitation of the value functions. If not specified, the
#' lower bounds present in the performance table are taken.
#' @param criteriaUBs Vector containing the upper bounds of the criteria to be
#' considered for the elicitation of the value functions. If not specified, the
#' upper bounds present in the performance table are taken.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param kPostOptimality A small positive threshold used during the
#' postoptimality analysis (see article on UTA by Siskos and Lagreze in EJOR,
#' 1982). If not specified, no postoptimality analysis is performed.
#' @return The function returns a list structured as follows :
#' \item{optimum}{The value of the objective function.} \item{valueFunctions}{A
#' list containing the value functions which have been determined. Each value
#' function is defined by a matrix of breakpoints, where the first row
#' corresponds to the abscissa (row labelled "x") and where the second row
#' corresponds to the ordinate (row labelled "y").} \item{overallValues}{A
#' vector of the overall values of the input alternatives.} \item{ranks}{A
#' vector of the ranks of the alternatives obtained via the elicited value
#' functions. Ties method = "min".} \item{Kendall}{Kendall's tau between the
#' input ranking and the one obtained via the elicited value functions. NULL if
#' no input ranking is given but alternativesPreferences or
#' alternativesIndifferences.} \item{errors}{A vector of the errors (sigma)
#' which have to be added to the overall values of the alternatives in order to
#' respect the input ranking.} \item{minimumWeightsPO}{In case a
#' post-optimality analysis is performed, the minimal weight of each criterion,
#' else NULL.} \item{maximumWeightsPO}{In case a post-optimality analysis is
#' performed, the maximal weight of each criterion, else NULL.}
#' \item{averageValueFunctionsPO}{In case a post-optimality analysis is
#' performed, average value functions respecting the input ranking, else NULL.}
#' @references E. Jacquet-Lagreze, J. Siskos, Assessing a set of additive
#' utility functions for multicriteria decision-making, the UTA method,
#' European Journal of Operational Research, Volume 10, Issue 2, 151--164, June
#' 1982.
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
#'   		c(3,10,1),
#' 			c(4,20,2),
#' 			c(2,20,0),
#' 			c(6,40,0),
#' 			c(30,30,3))
#' 
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' 
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # ranks of the alternatives
#' 
#' alternativesRanks <- c(1,2,2,3,4)
#' 
#' names(alternativesRanks) <- row.names(performanceTable)
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
#' x<-UTA(performanceTable, criteriaMinMax, 
#'         criteriaNumberOfBreakPoints, epsilon, 
#'         alternativesRanks = alternativesRanks)
#' 
#' # plot the value functions obtained
#' 
#' plotPiecewiseLinearValueFunctions(x$valueFunctions)
#' 
#' # apply the value functions on the original performance table
#' 
#' transformedPerformanceTable <- applyPiecewiseLinearValueFunctionsOnPerformanceTable(
#'   x$valueFunctions, 
#'   performanceTable)
#' 
#' # calculate the overall score of each alternative
#' 
#' weightedSum(transformedPerformanceTable,c(1,1,1))
#' 
#' # ----------------------------------------
#' # ranking some cars (from original article on UTA by Siskos and Lagreze, 1982)
#' 
#' # the separation threshold
#' 
#' epsilon <-0.01
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(      
#' c(173, 11.4, 10.01, 10, 7.88, 49500),
#' c(176, 12.3, 10.48, 11, 7.96, 46700),
#' c(142, 8.2, 7.30, 5, 5.65, 32100),
#' c(148, 10.5, 9.61, 7, 6.15, 39150), 
#' c(178, 14.5, 11.05, 13, 8.06, 64700), 
#' c(180, 13.6, 10.40, 13, 8.47, 75700),
#' c(182, 12.7, 12.26, 11, 7.81, 68593), 
#' c(145, 14.3, 12.95, 11, 8.38, 55000),
#' c(161, 8.6, 8.42, 7, 5.11, 35200), 
#' c(117, 7.2, 6.75, 3, 5.81, 24800)
#' )
#' 
#' rownames(performanceTable) <- c(
#'   "Peugeot 505 GR",
#'   "Opel Record 2000 LS",
#'   "Citroen Visa Super E",
#'   "VW Golf 1300 GLS",
#'   "Citroen CX 2400 Pallas",
#'   "Mercedes 230",
#'   "BMW 520",
#'   "Volvo 244 DL",
#'   "Peugeot 104 ZS",
#'   "Citroen Dyane")
#' 
#' colnames(performanceTable) <- c(
#'   "MaximalSpeed",
#'   "ConsumptionTown",
#'   "Consumption120kmh",
#'   "HP",
#'   "Space",
#'   "Price")
#' 
#' # ranks of the alternatives
#' 
#' alternativesRanks <- c(1,2,3,4,5,6,7,8,9,10)
#' 
#' names(alternativesRanks) <- row.names(performanceTable)
#' 
#' # criteria to minimize or maximize
#' 
#' criteriaMinMax <- c("max","min","min","max","max","min")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' # number of break points for each criterion
#' 
#' criteriaNumberOfBreakPoints <- c(5,4,4,5,4,5)
#' 
#' names(criteriaNumberOfBreakPoints) <- colnames(performanceTable)
#' 
#' # lower bounds of the criteria for the determination of value functions
#' 
#' criteriaLBs=c(110,7,6,3,5,20000)
#' 
#' names(criteriaLBs) <- colnames(performanceTable)
#' 
#' # upper bounds of the criteria for the determination of value functions
#' 
#' criteriaUBs=c(190,15,13,13,9,80000)
#' 
#' names(criteriaUBs) <- colnames(performanceTable)
#' 
#' x<-UTA(performanceTable, criteriaMinMax, 
#'         criteriaNumberOfBreakPoints, epsilon, 
#'         alternativesRanks = alternativesRanks,
#'         criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs)
#'         
#' 
#' # plot the value functions obtained
#' 
#' plotPiecewiseLinearValueFunctions(x$valueFunctions)
#' 
#' # apply the value functions on the original performance table
#' 
#' transformedPerformanceTable <- applyPiecewiseLinearValueFunctionsOnPerformanceTable(
#'       x$valueFunctions, 
#'       performanceTable)
#' 
#' # calculate the overall score of each alternative
#' 
#' weights<-c(1,1,1,1,1,1)
#' 
#' names(weights)<-colnames(performanceTable)
#' 
#' weightedSum(transformedPerformanceTable,c(1,1,1,1,1,1))
#' 
#' # the same analysis with less extreme value functions 
#' # from the post-optimality analysis
#' 
#' x<-UTA(performanceTable, criteriaMinMax, 
#'         criteriaNumberOfBreakPoints, epsilon,
#'         alternativesRanks = alternativesRanks,
#'         criteriaLBs = criteriaLBs, 
#'         criteriaUBs = criteriaUBs, 
#'         kPostOptimality = 0.01)
#'         
#' # plot the value functions obtained
#' 
#' plotPiecewiseLinearValueFunctions(x$averageValueFunctionsPO)
#' 
#' # apply the value functions on the original performance table
#' 
#' transformedPerformanceTable <- applyPiecewiseLinearValueFunctionsOnPerformanceTable(
#'       x$averageValueFunctionsPO, 
#'       performanceTable)
#' 
#' # calculate the overall score of each alternative
#' 
#' weights<-c(1,1,1,1,1,1)
#' 
#' names(weights)<-colnames(performanceTable)
#' 
#' weightedSum(transformedPerformanceTable,c(1,1,1,1,1,1))
#' 
#' 
#' # ----------------------------------------
#' # Let us consider only 2 criteria : Price and MaximalSpeed. What happens ? 
#' 
#' # x<-UTA(performanceTable, criteriaMinMax, 
#' #         criteriaNumberOfBreakPoints, epsilon,
#' #         alternativesRanks = alternativesRanks,
#' #         criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs,
#' #         criteriaIDs = c("MaximalSpeed","Price"))
#'         
#' 
#' # plot the value functions obtained
#' 
#' # plotPiecewiseLinearValueFunctions(x$valueFunctions, 
#' #                                   criteriaIDs = c("MaximalSpeed","Price"))
#' 
#' # apply the value functions on the original performance table
#' 
#' # transformedPerformanceTable <- applyPiecewiseLinearValueFunctionsOnPerformanceTable(
#' #   x$valueFunctions, 
#' #   performanceTable, 
#' #   criteriaIDs = c("MaximalSpeed","Price")
#' #   )
#' 
#' # calculate the overall score of each alternative
#' 
#' # weights<-c(1,1,1,1,1,1)
#' 
#' # names(weights)<-colnames(performanceTable)
#' 
#' # weightedSum(transformedPerformanceTable,
#' #          weights, criteriaIDs = c("MaximalSpeed","Price"))
#' 
#' # ----------------------------------------
#' # An example without alternativesRanks, but with alternativesPreferences
#' # and alternativesIndifferences
#' 
#' alternativesPreferences <- rbind(c("Peugeot 505 GR","Opel Record 2000 LS"),
#'                                 c("Opel Record 2000 LS","Citroen Visa Super E"))
#' 
#' alternativesIndifferences <- rbind(c("Peugeot 104 ZS","Citroen Dyane"))
#' 
#' x<-UTA(performanceTable, criteriaMinMax, 
#'         criteriaNumberOfBreakPoints, epsilon = 0.1,
#'         alternativesPreferences = alternativesPreferences,
#'         alternativesIndifferences = alternativesIndifferences,
#'         criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs
#'         )
#' 
#' 
#' @importFrom stats cor
#' @export UTA
UTA <- function(performanceTable, criteriaMinMax, criteriaNumberOfBreakPoints, epsilon, alternativesRanks = NULL, alternativesPreferences = NULL, alternativesIndifferences = NULL,  criteriaLBs=NULL, criteriaUBs=NULL, alternativesIDs = NULL, criteriaIDs = NULL, kPostOptimality = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.null(alternativesRanks) || is.vector(alternativesRanks)))
    stop("alternativesRanks should be a vector")
  
  if (!(is.null(alternativesPreferences) || is.matrix(alternativesPreferences)))
    stop("alternativesPreferences should be a matrix")
  
  if (!(is.null(alternativesIndifferences) || is.matrix(alternativesIndifferences)))
    stop("alternativesIndifferences should be a matrix")
  
  if (is.null(alternativesRanks) && is.null(alternativesPreferences) && is.null(alternativesIndifferences))
    stop("at least one of alternativesRanks, alternativesPreferences or alternativesIndifferences should not be NULL")
  
  if (!is.null(alternativesRanks) && (!is.null(alternativesPreferences) | !is.null(alternativesIndifferences)))
    stop("alternativesRanks and one of alternativesPreferences or alternativesIndifferences cannot be simultaneously not NULL")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaNumberOfBreakPoints)))
    stop("criteriaNumberOfBreakPoints should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be in a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be in a vector")
  
  if (!(is.null(criteriaLBs) || is.vector(criteriaLBs)))
    stop("criteriaLBs should be in a vector")
  
  if (!(is.null(criteriaUBs) || is.vector(criteriaUBs)))
    stop("criteriaUBs should be in a vector")
  
  ## filter the data according to the given alternatives and criteria
  ## in alternativesIDs and criteriaIDs
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    if (!is.null(alternativesRanks))
      alternativesRanks <- alternativesRanks[alternativesIDs]
    if (!is.null(alternativesPreferences)){
      tmpIds <- intersect(alternativesPreferences, alternativesIDs)
      tmpMatrix <- c()
      for (i in 1:dim(alternativesPreferences)[1]){
        if (all(alternativesPreferences[i,] %in% tmpIds))
          tmpMatrix <- rbind(tmpMatrix,alternativesPreferences[i,])
      }
      alternativesPreferences <- tmpMatrix
    }
    if (!is.null(alternativesIndifferences)){
      tmpIds <- intersect(alternativesIndifferences, alternativesIDs)
      tmpMatrix <- c()
      for (i in 1:dim(alternativesIndifferences)[1]){
        if (all(alternativesIndifferences[i,] %in% tmpIds))
          tmpMatrix <- rbind(tmpMatrix,alternativesIndifferences[i,])
      }
      alternativesIndifferences <- tmpMatrix
    }
  } 
  
  if (!is.null(criteriaIDs)){
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaNumberOfBreakPoints <- criteriaNumberOfBreakPoints[criteriaIDs]
  }
  
  if (!is.null(criteriaIDs) && !is.null(criteriaUBs)){
    criteriaUBs <- criteriaUBs[criteriaIDs]
  }
  
  if (!is.null(criteriaIDs) && !is.null(criteriaLBs)){
    criteriaLBs <- criteriaLBs[criteriaIDs]
  }
  
  # only the alternatives which are in the ranking should be considered for the calculation
  # we therefore take the intersection between the alternatives present in the performance
  # table and those of the ranking
  
  if (!is.null(alternativesRanks)){
    reallyActiveAlternatives <- intersect(rownames(performanceTable),names(alternativesRanks))
    
    if (length(reallyActiveAlternatives) != 0){
      performanceTable <- performanceTable[reallyActiveAlternatives,]
      alternativesRanks <- alternativesRanks[reallyActiveAlternatives]
    } else {
      stop("alternatives of alternativesRanks are not compatible with those of performanceTable")
    }
  }
  
  if (!is.null(alternativesPreferences) || !is.null(alternativesIndifferences)){
    reallyActiveAlternatives <- intersect(rownames(performanceTable),rbind(alternativesPreferences,alternativesIndifferences))
    
    if (length(reallyActiveAlternatives) != 0){
      performanceTable <- performanceTable[reallyActiveAlternatives,]
      
      if (!is.null(alternativesPreferences)){
        tmpIds <- intersect(alternativesPreferences, reallyActiveAlternatives)
        tmpMatrix <- c()
        for (i in 1:dim(alternativesPreferences)[1]){
          if (all(alternativesPreferences[i,] %in% tmpIds))
            tmpMatrix <- rbind(tmpMatrix,alternativesPreferences[i,])
        }
        alternativesPreferences <- tmpMatrix
      }
      
      if (!is.null(alternativesIndifferences)){
        tmpIds <- intersect(alternativesIndifferences, reallyActiveAlternatives)
        tmpMatrix <- c()
        for (i in 1:dim(alternativesIndifferences)[1]){
          if (all(alternativesIndifferences[i,] %in% tmpIds))
            tmpMatrix <- rbind(tmpMatrix,alternativesIndifferences[i,])
        }
        alternativesIndifferences <- tmpMatrix
      }
      
    } else {
      stop("alternatives of alternativesPreferences or alternativesIndifferences are not compatible with those of performanceTable")
    }
    
  }
  
  # data is filtered, check for some data consistency
  
  # are the upper and lower bounds given in the function compatible with the data in the performance table ?
  if (!(is.null(criteriaUBs))){
    if (!all(apply(performanceTable,2,max)<=criteriaUBs))
      stop("performanceTable contains higher values than criteriaUBs")
  }
  
  if (!(is.null(criteriaLBs))){
    if (!all(apply(performanceTable,2,min)>=criteriaLBs))
      stop("performanceTable contains lower values than criteriaLBs")
  }
  
  if (!all(criteriaNumberOfBreakPoints >= 2))
    stop("in criteriaNumberOfBreakPoints there should at least be 2 breakpoints for each criterion")
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  # if there are no alternatives left in the ranking or the pairwise preferences
  # we stop here
  
  if (is.null(alternativesRanks) && is.null(alternativesPreferences) && is.null(alternativesIndifferences))
    stop("after filtering none of alternativesRanks, alternativesPreferences or alternativesIndifferences is not NULL")
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  # -------------------------------------------------------
  
  criteriaBreakPoints <- list()
  
  for (i in 1:numCrit){
    
    tmp<-c()
    
    if (!is.null(criteriaLBs))
      mini <- criteriaLBs[i]
    else{
      mini <- min(performanceTable[,i])
    }
    
    if (!is.null(criteriaLBs))
      maxi <- criteriaUBs[i]
    else{
      maxi <- max(performanceTable[,i])
    }
    
    if (mini == maxi){
      # then there is only one value for that criterion, and the algorithm to build the linear interpolation
      # will not work correctly
      stop(paste("there is only one possible value left for criterion "),colnames(performanceTable)[i])
    }
    
    alphai <- criteriaNumberOfBreakPoints[i]
    
    for (j in 1:alphai)
      tmp<-c(tmp,mini + (j-1)/(alphai-1) * (maxi - mini))
    
    # due to this formula, the minimum and maximum values might not be exactly the same than the real minimum and maximum values in the performance table
    # to be sure there is no rounding problem, we recopy these values in tmp (important for the later comparisons to these values)
    
    tmp[1] <- mini
    tmp[alphai] <- maxi
    
    # if the criterion has to be maximized, the worst value is in the first position
    # else, we sort the vector the other way around to have the worst value in the first position
    
    if (criteriaMinMax[i] == "min")
      tmp<-sort(tmp,decreasing=TRUE)
    criteriaBreakPoints <- c(criteriaBreakPoints,list(tmp))
  }
  
  names(criteriaBreakPoints) <- colnames(performanceTable)
  
  # -------------------------------------------------------
  
  # a is a matrix decomposing the alternatives in the break point space and adding the sigma columns
  
  a<-matrix(0,nrow=numAlt, ncol=(sum(criteriaNumberOfBreakPoints)+numAlt))
  
  for (n in 1:numAlt){
    for (m in 1:numCrit){
      if (length(which(performanceTable[n,m]==criteriaBreakPoints[[m]]))!=0){
        # then we have a performance value which is on a breakpoint
        j<-which(performanceTable[n,m]==criteriaBreakPoints[[m]])
        if (m==1)
          pos <- j
        else
          pos<-sum(criteriaNumberOfBreakPoints[1:(m-1)])+j
        a[n,pos] <- 1
      }
      else{
        # then we have value which needs to be approximated by a linear interpolation
        # let us first search the lower and upper bounds of the interval of breakpoints around the value
        if (criteriaMinMax[m] == "min"){
          j<-which(performanceTable[n,m]>criteriaBreakPoints[[m]])[1]-1
        }
        else{
          j<-which(performanceTable[n,m]<criteriaBreakPoints[[m]])[1]-1			
        }
        if (m==1)
          pos <- j
        else
          pos<-sum(criteriaNumberOfBreakPoints[1:(m-1)])+j
        a[n,pos] <- 1-(performanceTable[n,m]-criteriaBreakPoints[[m]][j])/(criteriaBreakPoints[[m]][j+1] - criteriaBreakPoints[[m]][j])
        a[n,pos+1] <- (performanceTable[n,m]-criteriaBreakPoints[[m]][j])/(criteriaBreakPoints[[m]][j+1] - criteriaBreakPoints[[m]][j])
      }
      # and now for sigma
      a[n,dim(a)[2]-numAlt+n] <- 1
    }
  }
  
  # -------------------------------------------------------
  
  # the objective function : the first elements correspond to the ui's, the last one to the sigmas
  
  obj<-rep(0,sum(criteriaNumberOfBreakPoints))
  
  obj<-c(obj,rep(1,numAlt))
  
  # -------------------------------------------------------
  
  # we now build the part of the constraints matrix concerning the order / preferences / indifferences given by the decision maker
  
  preferenceConstraints<-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
  indifferenceConstraints <-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
  
  if (!is.null(alternativesRanks)){
    
    # determine now in which order the alternatives should be treated for the constraints
    indexOrder <- c()
    orderedAlternativesRanks <- sort(alternativesRanks)
    tmpRanks1 <- alternativesRanks
    tmpRanks2 <- alternativesRanks
    
    while (length(orderedAlternativesRanks) != 0){
      # search for the alternatives of lowest rank
      tmpIndex <- which(alternativesRanks == orderedAlternativesRanks[1])
      for (j in 1:length(tmpIndex))
        indexOrder<-c(indexOrder,tmpIndex[j])
      # remove the rank which has been dealt with now
      orderedAlternativesRanks<-orderedAlternativesRanks[-which(orderedAlternativesRanks==orderedAlternativesRanks[1])]
    }
    
    for (i in 1:(length(alternativesRanks)-1)){
      if (alternativesRanks[indexOrder[i]] == alternativesRanks[indexOrder[i+1]]){
        # then the alternatives are indifferent and their overall values are equal
        indifferenceConstraints <- rbind(indifferenceConstraints, a[indexOrder[i],] - a[indexOrder[i+1],])
      }
      else{
        # then the first alternative i is ranked better than the second one i+1 and i has an overall value higher than i+1
        preferenceConstraints <- rbind(preferenceConstraints, a[indexOrder[i],] - a[indexOrder[i+1],])
      } 
    }
  }
  
  if (!is.null(alternativesPreferences)){
    
    for (i in 1:dim(alternativesPreferences)[1]){
      preferenceConstraints <- rbind(preferenceConstraints, a[which(rownames(performanceTable)==alternativesPreferences[i,1]),] - a[which(rownames(performanceTable)==alternativesPreferences[i,2]),])
    }   
  }
  
  if (!is.null(alternativesIndifferences)){
    
    for (i in 1:dim(alternativesIndifferences)[1]){
      indifferenceConstraints <- rbind(indifferenceConstraints, a[which(rownames(performanceTable)==alternativesIndifferences[i,1]),] - a[which(rownames(performanceTable)==alternativesIndifferences[i,2]),])
    }   
  }
  
  # add this to the constraints matrix mat
  
  mat<-rbind(preferenceConstraints,indifferenceConstraints)
  
  # right hand side of this part of mat
  
  rhs <- c()
  
  if (dim(preferenceConstraints)[1]!=0){
    for (i in (1:dim(preferenceConstraints)[1]))
      rhs<-c(rhs,epsilon)
  }
  
  if (dim(indifferenceConstraints)[1]!=0){
    for (i in (1:dim(indifferenceConstraints)[1]))
      rhs<-c(rhs,0)
  }
  # direction of the inequality for this part of mat
  
  dir <- c()
  
  if (dim(preferenceConstraints)[1]!=0){
    for (i in (1:dim(preferenceConstraints)[1]))
      dir<-c(dir,">=")
  }
  
  if (dim(indifferenceConstraints)[1]!=0){
    for (i in (1:dim(indifferenceConstraints)[1]))
      dir<-c(dir,"==")
  }
  
  
  # -------------------------------------------------------
  
  # now the monotonicity constraints on the value functions
  
  monotonicityConstraints<-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
  
  for (i in 1:length(criteriaNumberOfBreakPoints)){
    for (j in 1:(criteriaNumberOfBreakPoints[i]-1)){
      tmp<-rep(0,sum(criteriaNumberOfBreakPoints)+numAlt)
      if (i==1)
        pos <- j
      else
        pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])+j
      tmp[pos] <- -1
      tmp[pos+1] <- 1
      monotonicityConstraints <- rbind(monotonicityConstraints, tmp)
    }
  }
  
  # add this to the constraints matrix mat
  
  mat<-rbind(mat,monotonicityConstraints)
  
  # the direction of the inequality
  
  for (i in (1:dim(monotonicityConstraints)[1]))
    dir<-c(dir,">=")
  
  # the right hand side of this part of mat
  
  for (i in (1:dim(monotonicityConstraints)[1]))
    rhs<-c(rhs,0)
  
  # -------------------------------------------------------
  
  # normalization constraint for the upper values of the value functions (sum = 1)
  
  tmp<-rep(0,sum(criteriaNumberOfBreakPoints)+numAlt)
  
  for (i in 1:length(criteriaNumberOfBreakPoints)){
    if (i==1)
      pos <- criteriaNumberOfBreakPoints[i]
    else
      pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])+criteriaNumberOfBreakPoints[i]
    tmp[pos] <- 1
  }
  
  # add this to the constraints matrix mat
  
  mat<-rbind(mat,tmp)
  
  # the direction of the inequality
  
  dir<-c(dir,"==")
  
  # the right hand side of this part of mat
  
  rhs<-c(rhs,1)
  
  # -------------------------------------------------------
  
  # now the normalizaiton constraints for the lower values of the value functions (= 0)
  
  minValueFunctionsConstraints<-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
  
  for (i in 1:length(criteriaNumberOfBreakPoints)){
    tmp<-rep(0,sum(criteriaNumberOfBreakPoints)+numAlt)
    if (i==1)
      pos <- i
    else
      pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])+1
    tmp[pos] <- 1
    minValueFunctionsConstraints <- rbind(minValueFunctionsConstraints,tmp)
  }
  
  # add this to the constraints matrix mat
  
  mat<-rbind(mat,minValueFunctionsConstraints)
  
  # the direction of the inequality
  
  for (i in (1:dim(minValueFunctionsConstraints)[1]))
    dir<-c(dir,"==")
  
  # the right hand side of this part of mat
  
  for (i in (1:dim(minValueFunctionsConstraints)[1]))
    rhs<-c(rhs,0)
  
  # -------------------------------------------------------
  
  lpSolution <- Rglpk_solve_LP(obj, mat, dir, rhs)
  
  # -------------------------------------------------------
  
  # create a structure containing the value functions
  
  valueFunctions <- list()
  
  for (i in 1:length(criteriaNumberOfBreakPoints)){
    tmp <- c() 
    if (i==1)
      pos <- 0
    else
      pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])
    for (j in 1:criteriaNumberOfBreakPoints[i]){
      tmp <- c(tmp,lpSolution$solution[pos+j])
    }
    tmp<-rbind(criteriaBreakPoints[[i]],tmp)
    colnames(tmp)<- NULL
    rownames(tmp) <- c("x","y")
    valueFunctions <- c(valueFunctions,list(tmp))
  }
  
  names(valueFunctions) <- colnames(performanceTable)
  
  # it might happen on certain computers that these value functions 
  # do NOT respect the monotonicity constraints (especially because of too small differences and computer arithmetics)
  # therefore we check if they do, and if not, we "correct" them
  
  for (i in 1:numCrit){
    for (j in 1:(criteriaNumberOfBreakPoints[i]-1)){
      if (valueFunctions[[i]][2,j] > valueFunctions[[i]][2,j+1]){
        valueFunctions[[i]][2,j+1] <- valueFunctions[[i]][2,j] 
      }
    }
  }
  
  # -------------------------------------------------------
  
  overallValues <- as.vector(t(a[,1:sum(criteriaNumberOfBreakPoints)]%*%lpSolution$solution[1:sum(criteriaNumberOfBreakPoints)]))
  
  names(overallValues) <- rownames(performanceTable)
  
  # -------------------------------------------------------
  
  # the error values for each alternative (sigma)
  
  errorValues <- as.vector(lpSolution$solution[(sum(criteriaNumberOfBreakPoints)+1):length(lpSolution$solution)])
  
  names(errorValues) <- rownames(performanceTable)
  
  # -------------------------------------------------------
  
  # the ranks of the alternatives 
  
  outRanks <- rank(-overallValues, ties.method="min")
  
  # -------------------------------------------------------
  
  if ((numAlt >= 3) && !is.null(alternativesRanks))
    tau = cor(alternativesRanks,outRanks,method="kendall")
  else
    tau = NULL
  
  # prepare the output
  
  out <- list(optimum = round(lpSolution$optimum, digits=5), valueFunctions = valueFunctions, overallValues = round(overallValues, digits=5), ranks = outRanks, errors = round(errorValues, digits=5), Kendall = tau)
  
  
  # -------------------------------------------------------
  
  # post-optimality analysis if the optimum is found and if kPostOptimality is not NULL, i.e. the solution space is not empty
  
  minWeights <- NULL
  maxWeights <- NULL
  averageValueFunctions <- NULL
  
  
  if (!is.null(kPostOptimality) && (lpSolution$optimum == 0)){
    
    # add F \leq F* + k(F*) to the constraints, where F* is the optimum and k(F*) is a positive threshold, which is a small proportion of F*
    
    mat <- rbind(mat,obj)
    dir <- c(dir,"<=")
    rhs <- c(rhs,kPostOptimality)
    
    minWeights <- c()
    maxWeights <- c()
    combinedSolutions <- c()
    
    for (i in 1:numCrit){
      
      # first maximize the best ui for each criterion, then minimize it
      # this gives the interval of variation for each weight
      # the objective function : the first elements correspond to the ui's, the last one to the sigmas
      
      obj<-rep(0,sum(criteriaNumberOfBreakPoints))
      obj<-c(obj,rep(0,numAlt))
      
      if (i==1)
        pos <- criteriaNumberOfBreakPoints[i]
      else
        pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])+criteriaNumberOfBreakPoints[i]
      
      obj[pos] <- 1
      
      lpSolutionMin <- Rglpk_solve_LP(obj, mat, dir, rhs)
      lpSolutionMax <- Rglpk_solve_LP(obj, mat, dir, rhs, max=TRUE)
      
      minWeights <- c(minWeights,lpSolutionMin$optimum)
      maxWeights <- c(maxWeights,lpSolutionMax$optimum)
      combinedSolutions <- rbind(combinedSolutions,lpSolutionMin$solution)
      combinedSolutions <- rbind(combinedSolutions,lpSolutionMax$solution)
    }
    
    names(minWeights) <- colnames(performanceTable)
    names(maxWeights) <- colnames(performanceTable)
    
    # calculate the average value function, for which each component is the average value obtained for each of the programs above
    averageSolution <- apply(combinedSolutions,2,mean)
    
    # create a structure containing the average value functions
    
    averageValueFunctions <- list()
    
    for (i in 1:length(criteriaNumberOfBreakPoints)){
      tmp <- c() 
      if (i==1)
        pos <- 0
      else
        pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])
      for (j in 1:criteriaNumberOfBreakPoints[i]){
        tmp <- c(tmp,averageSolution[pos+j])
      }
      tmp<-rbind(criteriaBreakPoints[[i]],tmp)
      colnames(tmp)<- NULL
      rownames(tmp) <- c("x","y")
      averageValueFunctions <- c(averageValueFunctions,list(tmp))
    }
    
    names(averageValueFunctions) <- colnames(performanceTable)
    
  }
  
  out <- c(out, list(minimumWeightsPO = minWeights, maximumWeightsPO = maxWeights, averageValueFunctionsPO = averageValueFunctions))
  
  return(out)
}
