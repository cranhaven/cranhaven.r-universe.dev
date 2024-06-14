# Measures based on consistency
library(digest)

#' @author Adan M. Rodriguez
#' @title Rough Set consistency measure
#' @description Generates an evaluation function that calculates the rough sets consistency value \insertCite{Pawlak1982}{FSinR} \insertCite{Pawlak1991}{FSinR}, using hash tables (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the rough sets consistency value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import digest
#' @importFrom stats aggregate
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
#' # Generate the evaluation function with Rough Set Consistency
#' rsc_evaluator <- roughsetConsistency()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' rsc_evaluator(adult,'income',c('race','sex','education'))
#' }
roughsetConsistency <- function() {
  
  roughsetConsistencyEvaluator <- function(data, class, features) {
    if (!isDataframeDiscrete(data)) {
      warning("The data seems not to be discrete, as it should be")
    }
    # Get values of the features
    features <- unlist(features)
    feature.data <- data[, features, drop = FALSE]
    
    # Store the values of each feature in a hash table
    hash.vector <- as.factor(apply(feature.data, 1, digest))
    
    # If for one value of a feature, we have more than 1 different class, this examples will be inconsistent
    rough.set <- function(vector) {
      if ((length(which(vector != 0))) > 1) {
        return(0)
      } else {
        return(max(vector))
      }
    }
    
    # Calculate consistent examples
    result <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
      return(rough.set(as.vector(table(classes))))
    })
    
    # Calculate rough sets consistecy
    result <- sum(result[[dim(result)[2]]]) / dim(feature.data)[1]
    
    return(result)
  }
  
  attr(roughsetConsistencyEvaluator,'shortName') <- "roughsetConsistency"
  attr(roughsetConsistencyEvaluator,'name') <- "Rough Set Consistency"
  attr(roughsetConsistencyEvaluator,'target') <- "maximize"
  attr(roughsetConsistencyEvaluator,'kind') <- "Set measure"
  attr(roughsetConsistencyEvaluator,'needsDataToBeDiscrete') <- TRUE
  attr(roughsetConsistencyEvaluator,'needsDataToBeContinuous') <- FALSE

  return(roughsetConsistencyEvaluator)
}

#' @author Adan M. Rodriguez
#' @title Binary consistency measure
#' @description Generates an evaluation function that calculates the binary consistency, also known as "Sufficiency test" from FOCUS \insertCite{AlmuallimDietterich1991}{FSinR} (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the binary consistency value for the selected features.
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
#' # Generate the evaluation function with Binary Consistency
#' bc_evaluator <- binaryConsistency()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' bc_evaluator(adult,'income',c('race','sex','education'))
#' }
binaryConsistency <- function() {
  
  binaryConsistencyEvaluator <- function(data, class, features) {
    # Invoke the IEConsistency function
    binary <- IEConsistency()(data, class, features)
    
    # The feature will be consistent 100% or inconsistent 0%
    if (binary == 1) {
      result <- 1
    } else {
      result <- 0
    }
    
    return(result)
  }

  attr(binaryConsistencyEvaluator,'shortName') <- "binaryConsistency"
  attr(binaryConsistencyEvaluator,'name') <- "Binary Consistency"
  attr(binaryConsistencyEvaluator,'target') <- "maximize"
  attr(binaryConsistencyEvaluator,'kind') <- "Set measure"
  attr(binaryConsistencyEvaluator,'needsDataToBeDiscrete') <- TRUE
  attr(binaryConsistencyEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(binaryConsistencyEvaluator)
}

#' @author Adan M. Rodriguez
#' @title Inconsistent Examples consistency measure
#' @description Generates an evaluation function that calculates the inconsistent examples consistency value \insertCite{DashLiu2003}{FSinR}, using hash tables (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the inconsistent examples consistency value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import digest
#' @importFrom stats aggregate
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
#' # Generate the evaluation function with IE Consistency
#' IEC_evaluator <- IEConsistency()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' IEC_evaluator(adult,'income',c('race','sex','education'))
#' }
IEConsistency <- function() {
  
  IEConsistencyEvaluator <- function(data, class, features) {
    if (!isDataframeDiscrete(data)) {
      warning("The data seems not to be discrete, as it should be")
    }
    # Get values of the features
    features <- unlist(features)
    feature.data <- data[, features, drop = FALSE]
    
    # Store the values (column 1) of each feature in a hash table
    hash.vector <- as.factor(apply(feature.data, 1, digest))
    
    # The examples of the majoritarian class, will be the consistent examples
    result <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
      return(max(as.vector(table(classes))))
    })
    
    # Calculate the IEP consistency
    result <- sum(result[[dim(result)[2]]]) / dim(feature.data)[1]
    
    return(result)
  }

  attr(IEConsistencyEvaluator,'shortName') <- "IEConsistency"
  attr(IEConsistencyEvaluator,'name') <- "Inconsistent Examples Consistency"
  attr(IEConsistencyEvaluator,'target') <- "maximize"
  attr(IEConsistencyEvaluator,'kind') <- "Set measure"
  attr(IEConsistencyEvaluator,'needsDataToBeDiscrete') <- TRUE
  attr(IEConsistencyEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(IEConsistencyEvaluator)
}


#' @author Adan M. Rodriguez
#' @title Inconsistent Examples Pairs consistency measure
#' @description Generates an evaluation function that calculates the inconsistent examples pairs consistency value, using hash tables \insertCite{Arauzo2007}{FSinR} (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the inconsistent examples pairs consistency value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import digest
#' @importFrom stats aggregate
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
#' # Generate the evaluation function with IEP Consistency
#' IEPC_evaluator <- IEPConsistency()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' IEPC_evaluator(adult,'income',c('race','sex','education'))
#' }
IEPConsistency <- function() {
  
  IEPConsistencyEvaluator <- function(data, class, features) {
    if (!isDataframeDiscrete(data)) {
      warning("The data seems not to be discrete, as it should be")
    }
    # Get values of the features
    features <- unlist(features)
    feature.data = data[, features, drop=FALSE]
    
    # Store the values (column 1) of each feature in a hash table
    hash.vector = as.factor(apply(feature.data, 1, digest))
    
    # Count the number of inconsistent pairs
    example.pairs <- function(vector) {
      num.class <- sum(vector)
      
      # If the number of zeros equals to number of classes - 1... return 0
      if ((length(which(vector == 0))) == (length(vector) - 1)) {
        return(0)
      } else {
        different <- 0
        left <- num.class
        
        for (i in vector) {
          ne <- i
          left <- left - ne
          different <- different + (ne * left)
        }
        return(different)
      }
    }
    
    # For each value of the feature...
    inconsistent.pairs <- aggregate(data[[class]], list(hash = hash.vector), function(classes) {
      return(example.pairs(as.vector(table(classes))))
    })
    
    # Number of pairs = (number of elements * (number of elements - 1)) / 2
    num.pairs <- (dim(feature.data)[1] * (dim(feature.data)[1] - 1)) / 2
    
    # Calculate IEP consistency
    result <- 1 - (sum(inconsistent.pairs[[dim(inconsistent.pairs)[2]]]) / num.pairs)
    
    return(result)
  }

  attr(IEPConsistencyEvaluator,'shortName') <- "IEPConsistency"
  attr(IEPConsistencyEvaluator,'name') <- "Inconsistent Examples Pairs Consistency"
  attr(IEPConsistencyEvaluator,'target') <- "maximize"
  attr(IEPConsistencyEvaluator,'kind') <- "Set measure"
  attr(IEPConsistencyEvaluator,'needsDataToBeDiscrete') <- TRUE
  attr(IEPConsistencyEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(IEPConsistencyEvaluator)
}
