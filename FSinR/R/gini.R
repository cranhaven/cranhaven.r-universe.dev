#' @author Adan M. Rodriguez
#' @title Gini index measure
#' @description Generates an evaluation function that calculates the gini index \insertCite{Ceriani2012}{FSinR} of discrete features (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#' 
#' @return Returns a function that is used to generate an evaluation set measure using the Gini index value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to evaluate a set of features
#' ## Classification problem
#' 
#' # A discrete dataset is used (in this case we use only several discrete columns)
#' adult <- adult[,c(4,9,10,15)]
#' 
#' # Generate the evaluation function with Gini index
#' giniIndex_evaluator <- giniIndex()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' giniIndex_evaluator(adult,'income',c('race','sex','education'))
#' }
giniIndex <- function() {
  
  giniIndexEvaluator <- function(data, class, features) {
    # Get values of the features
    features <- unlist(features) 
    feature.data <- data[, features, drop=FALSE]
    
    # We will store the values of each feature in a hash table
    hash.vector <- as.factor(apply(feature.data, 1, digest)) 
    
    # Calculate Gini of each value of the feature
    gini <- function(vector) {
      gini.result <- 0
      for (i in vector)
        gini.result <- gini.result + (i / sum(vector)) ^ 2
      
      return(gini.result)
    }
    
    # Store the gini index of each feature value
    result <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
      return(gini(as.vector(table(classes)))) 
    }) 
    
    # Store the number of times each feature value appears
    ni <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
      return (sum(table(classes)))  
    })
    div <- (ni[[dim(ni)[2]]] / dim(feature.data)[1])
    
    # Final gini index of the feature
    final.gini <- (sum(div * result[[dim(result)[2]]]))
    
    return(final.gini)
  }
  attr(giniIndexEvaluator,'shortName') <- "giniIndex"
  attr(giniIndexEvaluator,'name') <- "Gini Index"
  attr(giniIndexEvaluator,'target') <- "maximize"
  attr(giniIndexEvaluator,'kind') <- "Set measure"
  attr(giniIndexEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(giniIndexEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(giniIndexEvaluator)
}