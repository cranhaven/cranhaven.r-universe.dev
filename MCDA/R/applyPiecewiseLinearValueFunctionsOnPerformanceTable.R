#' Applies value functions on a performance table.
#' 
#' Transforms a performance table via given piecewise linear value functions.
#' 
#' 
#' @param valueFunctions A list containing, for each criterion, the piecewise
#' linear value functions defined by the coordinates of the break points. Each
#' value function is defined by a matrix of breakpoints, where the first row
#' corresponds to the abscissa (row labelled "x") and where the second row
#' corresponds to the ordinate (row labelled "y").
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @return The function returns a performance table which has been transformed
#' through the given value functions.
#' @keywords methods
#' @examples
#' 
#' 
#' # the value functions
#' 
#' v<-list(
#'   Price = array(c(30, 0, 16, 0, 2, 0.0875), 
#'     dim=c(2,3), dimnames = list(c("x", "y"), NULL)), 
#'   Time = array(c(40, 0, 30, 0, 20, 0.025, 10, 0.9), 
#'     dim = c(2, 4), dimnames = list(c("x", "y"), NULL)), 
#'   Comfort = array(c(0, 0, 1, 0, 2, 0.0125, 3, 0.0125), 
#'     dim = c(2, 4), dimnames = list(c("x", "y"), NULL)))
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(
#'     	c(3,10,1),
#' 			c(4,20,2),
#' 			c(2,20,0),
#' 			c(6,40,0),
#' 			c(30,30,3))
#' 
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' 
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # the transformed performance table
#' 
#' applyPiecewiseLinearValueFunctionsOnPerformanceTable(v,performanceTable)
#' 
#' @export applyPiecewiseLinearValueFunctionsOnPerformanceTable
applyPiecewiseLinearValueFunctionsOnPerformanceTable <- function(valueFunctions, performanceTable, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteria IDs should be in a vector")
  
  ## filter the data according to the given criteria and alternatives
  
  if (!is.null(criteriaIDs)){
    valueFunctions <- valueFunctions[criteriaIDs]
    performanceTable <- performanceTable[,criteriaIDs]
  }
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
  } 
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  # -------------------------------------------------------
  
  ## check if performanceTable values lie in the ranges of the valueFunctions values
  
  test <- TRUE
  i<-1
  
  while(test && (i<=numCrit)){
    mini <- min(performanceTable[,i])
    maxi <- max(performanceTable[,i])
    
    vFmini <- min(valueFunctions[[i]]["x",])
    vFmaxi <- max(valueFunctions[[i]]["x",])
    
    if (!((mini >= vFmini) && (maxi <= vFmaxi)))
      test<-FALSE
    
    i<-i+1
  }
  
  if (!test)
    stop("performanceTable ranges do not lie in valueFunctions ranges")
  
  
  ## reorder points of value functions to make them increasing w.r.t. y
  
  for (i in 1:numCrit){
    valueFunctions[[i]] <- valueFunctions[[i]][,order(valueFunctions[[i]][2,])]
  }
  
  ## determine if criteria are to be minimized or maximized according to the value functions
  
  criteriaMinMax<-c()
  
  for (i in 1:numCrit){
    if (all(diff(valueFunctions[[i]][1,]) %>=% 0)){
      criteriaMinMax <- c(criteriaMinMax, "max")
    }
    else if (all(diff(valueFunctions[[i]][1,]) %<=% 0)){
      criteriaMinMax <- c(criteriaMinMax, "min")
    }
    else stop("non monotonic value function")
  }
  
  # determine first how many breakpoints in each value function
  
  criteriaNumberOfBreakPoints <- c()
  
  for (i in 1:numCrit)
    criteriaNumberOfBreakPoints <- c(criteriaNumberOfBreakPoints, dim(valueFunctions[[i]])[2])
  
  criteriaBreakPoints <- list()
  
  for (i in 1:numCrit){
    tmp <- valueFunctions[[i]]["x",]
    criteriaBreakPoints <- c(criteriaBreakPoints,list(tmp))
  }
  
  names(criteriaBreakPoints) <- colnames(performanceTable)
  
  # -------------------------------------------------------
  
  # a is a matrix decomposing the alternatives in the break point space
  
  a<-matrix(0,nrow=numAlt, ncol=(sum(criteriaNumberOfBreakPoints)))
  
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
    }
  }
  
  # -------------------------------------------------------
  
  normalizedPerformanceTable <- matrix(nrow=numAlt, ncol=numCrit)
  
  for (m in 1:numCrit){
    if (m==1){
      start = 1
      end = criteriaNumberOfBreakPoints[m]
    }
    else
    {
      start = sum(criteriaNumberOfBreakPoints[1:(m-1)]) + 1
      end = sum(criteriaNumberOfBreakPoints[1:(m-1)]) + criteriaNumberOfBreakPoints[m]
    }
    normalizedPerformanceTable[,m] <- a[,start:end]%*%valueFunctions[[m]]["y",]
  }
  rownames(normalizedPerformanceTable) <- rownames(performanceTable)
  
  colnames(normalizedPerformanceTable) <- colnames(performanceTable)
  
  return(normalizedPerformanceTable)
}
