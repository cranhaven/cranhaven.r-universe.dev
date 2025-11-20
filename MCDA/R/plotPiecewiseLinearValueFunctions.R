#' Function to plot piecewise linear value functions.
#' 
#' Plots piecewise linear value function.
#' 
#' 
#' @param valueFunctions A list containing, for each criterion, the piecewise
#' linear value functions defined by the coordinates of the break points. Each
#' value function is defined by a matrix of breakpoints, where the first row
#' corresponds to the abscissa (row labelled "x") and where the second row
#' corresponds to the ordinate (row labelled "y").
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @keywords methods
#' @examples
#' 
#' 
#' v<-list(
#'   Price = array(c(30, 0, 16, 0, 2, 0.0875), 
#'     dim=c(2,3), dimnames = list(c("x", "y"), NULL)), 
#'   Time = array(c(40, 0, 30, 0, 20, 0.025, 10, 0.9), 
#'     dim = c(2, 4), dimnames = list(c("x", "y"), NULL)), 
#'   Comfort = array(c(0, 0, 1, 0, 2, 0.0125, 3, 0.0125), 
#'     dim = c(2, 4), dimnames = list(c("x", "y"), NULL)))
#' 
#' # plot the value functions
#' 
#' plotPiecewiseLinearValueFunctions(v)
#' 
#' @export plotPiecewiseLinearValueFunctions
plotPiecewiseLinearValueFunctions <- function(valueFunctions, criteriaIDs = NULL){
	
	## check the input data
  
	if (!(is.list(valueFunctions)))
        	stop("valueFunctions should be a list")
  
	if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
	  stop("criteriaIDs should be a vector")
  
	## filter the data according to the given criteria
	
	if (!is.null(criteriaIDs)){
	  valueFunctions <- valueFunctions[criteriaIDs]
	}
  
	if (is.null(valueFunctions[[1]]))
	  stop("no value functions left to plot")
  else 
    numCrit <- length(valueFunctions)
  
	# plotting symbol and color 
  
	par(pch=22, col="black")
  
	# determine how many plots per row and column
  
  if (numCrit <= 2)
    par(mfrow=c(1,2)) 
	else
	  par(mfrow=c(ceiling(log2(numCrit)),ceiling(log2(numCrit)))) 
  
  # plot the functions
    
	for(i in 1:numCrit){
	  heading = names(valueFunctions)[i]
	  plot(valueFunctions[[i]]["x",], valueFunctions[[i]]["y",], type="n", main=heading,xlab="", ylab="") 
	  lines(valueFunctions[[i]]["x",], valueFunctions[[i]]["y",], type="b") 
	}
  
}
