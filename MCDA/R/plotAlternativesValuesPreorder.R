#' Function to plot a preorder of alternatives, based on some score or ranking.
#' 
#' Plots a preorder of alternatives as a graph, representing the ranking of the
#' alternatives, w.r.t. some scores or ranks. A decreasing order or increasing
#' order can be specified, w.r.t. to these scores or ranks.
#' 
#' 
#' @param alternativesValues A vector containing some values related to
#' alternatives, as scores or ranks. The elements of the vector are named
#' according to the IDs of the alternatives.
#' @param decreasing A boolean to indicate if the alternatives are to be sorted
#' increasingly (FALSE) or decreasingly (TRUE) w.r.t. the alternativesValues.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the data should be filtered.
#' @param silent A boolean indicating if the order should be printed
#'               to the terminal or not. Default is FALSE.
#' @return A character vector with the names of alternatives sorted (invisibly).
#' @keywords methods
#' @examples
#' 
#' library(MCDA)
#' 
#' alternativesValues <- c(10,1,8,3,8,3,4,4,8,5)
#' 
#' names(alternativesValues) <- c("x10","x1","x9","x2","x8",
#'                                 "x3","x7","x4","x6","x5")
#' 
#' plotAlternativesValuesPreorder(alternativesValues, 
#'                                 decreasing=TRUE, 
#'                                 alternativesIDs=c("x10","x3","x7",
#'                                                     "x4","x6","x5"))
#' 
#' 
#' @export plotAlternativesValuesPreorder
plotAlternativesValuesPreorder <- function(alternativesValues,
                                           decreasing = TRUE,
                                           alternativesIDs = NULL, 
                                           silent=FALSE){
  # Check the input data
  test <- is.vector(alternativesValues)
  if(!test) stop("Argument 'alternativesValues' should be a vector")
  test <- length(alternativesValues)>=2
  if(!test) stop("Argument 'alternativesValues' must have 2 or ",
                 "more criteria oralternatives")
  test <- !is.null(names(alternativesValues))
  if(!test) stop("Argument 'alternativesValues' must be named")
  test <- is.null(alternativesIDs) || is.vector(alternativesIDs)
  if(!test) stop("Argument 'alternativesIDs' should be a vector")

  # Filter alternatives if requested
  if(!is.null(alternativesIDs)) alternativesValues <- alternativesValues[alternativesIDs]

  # Sort alts by value
  sorted <- alternativesValues[order(alternativesValues, decreasing=decreasing)]
  nAlt <- length(alternativesValues)

  # Create nodes
  nodes <- names(sorted)[1]
  for(i in 2:length(sorted)){
    namei <- names(sorted)[i]
    if(sorted[i]!=sorted[i-1]) nodes <- c(nodes, namei) else {
      nodes[length(nodes)] <- paste0(nodes[length(nodes)], ", ", namei)
    }
  }; rm(namei)

  # Print to screen
  if(!silent){
    indent <- max(floor(min(max(nchar(nodes)), getOption("width"))/2) - 1, 1)
    for(i in 1:length(nodes)){
      if(i>1) cat(rep(" ", indent), "\\/", "\n", sep="")
      blank <- max(floor(1 + indent - nchar(nodes[i])/2), 0)
      cat(rep(" ", blank), nodes[i], "\n", sep="")
    }
  }
  
  return(invisible(nodes))
}
