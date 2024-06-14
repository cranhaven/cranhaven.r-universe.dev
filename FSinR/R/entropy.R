entropy <- function(x) {
  # Calculate the frequency of each value of the feature
  freqs <- table(x) / length(x) 
  freqs <- as.vector(freqs)   
  freqs <- freqs[freqs != 0] # Delete non-appearing values
  # Calculate the entropy
  result <- -sum(freqs * log2(freqs))   
  
  return(result) 
}


entropyJ <- function(x) {
  # Calculate the frequency of each value of the feature in the selected class
  freqs <- x / sum(x) 
  freqs <- as.vector(freqs)    
  logs <- freqs * log2(freqs)
  logs <- replace(logs, is.na(logs), 0)
  
  # Calculate the joint entropy
  summatory <- -sum(logs)
  
  return(summatory)
}

#' @author Adan M. Rodriguez
#' @title The mutual information measure
#' @description Generates an evaluation function that calculates the mutual information value, using the information theory \insertCite{QianShu2015}{FSinR} (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the mutual information value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @importFrom digest digest
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
#' # Generate the evaluation function with Cramer
#' mi_evaluator <- mutualInformation()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' mi_evaluator(adult,'income',c('race','sex','education'))
#' }
mutualInformation <- function() {
  
  mutualInformationEvaluator <- function(data, class, features) {
    
    if (!isDataframeDiscrete(data)) {
      warning("The data seems not to be discrete, as it should be")
    }
    # Get values of the features
    features <- unlist(features) 
    feature.data <- data[, features, drop = FALSE] 
    
    # Calculate entropy of the class
    classes <- c(data[[class]]) # Obtenemos valores de la clase en numeros
    class.entropy <- entropy(classes)
    
    # To calculate the measure of only one feature...
    if (ncol(feature.data) == 1) {
      # Calculate the entropy of the feature
      features.entropies <- sapply(feature.data, entropy) 
      vector <- unlist(feature.data) 
      
      # Calculate joint entropy between the feature and the class
      joint.entropy <- entropyJ(table(vector, classes))
      
      # To calculate the measure of only 2 or more features...
    } else { 
      # Calculate joint entropy between the features
      features.entropies <- entropyJ(table(as.factor(apply(feature.data, 1, digest))))
      tabla <- data.frame(feature.data, classes)
      
      # Calculate joint entropy between the features and the class
      joint.entropy <- entropyJ(table(as.factor(apply(tabla, 1, digest))))
    }
    
    # Calculate the mutual information
    result <- class.entropy + unname(features.entropies) - joint.entropy
    
    return(result)
  }

  attr(mutualInformationEvaluator,'shortName') <- "mutualInformation"
  attr(mutualInformationEvaluator,'name') <- "Mutual Information"
  attr(mutualInformationEvaluator,'target') <- "maximize"
  attr(mutualInformationEvaluator,'kind') <- "Set measure"
  attr(mutualInformationEvaluator,'needsDataToBeDiscrete') <- TRUE
  attr(mutualInformationEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(mutualInformationEvaluator)
}

#' @author Adan M. Rodriguez
#' @title The gain ratio measure
#' @description Generates an evaluation function that calculates the gain ratio value \insertCite{Quinlan1986}{FSinR}, using the information theory (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the gain ratio value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @importFrom digest digest
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
#' # Generate the evaluation function with Cramer
#' gr_evaluator <- gainRatio()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' gr_evaluator(adult,'income',c('race','sex','education'))
#' }
gainRatio <- function() {
  
  gainRatioEvaluator <- function(data, class, features) {
    if (!isDataframeDiscrete(data)) {
      warning("The data seems not to be discrete, as it should be")
    }
    # Get values of the features
    features <- unlist(features) 
    feature.data <- data[, features, drop = FALSE] 
    
    # Calculate the entropy of the features
    if (ncol(feature.data) == 1) {
      features.entropies <- sapply(feature.data, entropy) 
    } else {
      features.entropies <- entropyJ(table(as.factor(apply(feature.data, 1, digest))))
    }
    
    # Invoke the mutual information function
    mutual.information <- mutualInformation()(data, class, features)
    
    # Calculate the Gain Ratio
    gain <- mutual.information / unname(features.entropies)
    
    if (is.nan(gain))
      gain <- 0
    
    return(gain)
  }

  attr(gainRatioEvaluator,'shortName') <- "gainRatio"
  attr(gainRatioEvaluator,'name') <- "Gain Ratio"
  attr(gainRatioEvaluator,'target') <- "maximize"
  attr(gainRatioEvaluator,'kind') <- "Set measure"
  attr(gainRatioEvaluator,'needsDataToBeDiscrete') <- TRUE
  attr(gainRatioEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(gainRatioEvaluator)
}

#' @author Adan M. Rodriguez
#' @title Symmetrical uncertain measure
#' @description Generates an evaluation function that calculates the symmetrical uncertain value \insertCite{WittenFrank2005}{FSinR}, using the information theory (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the symmetrical uncertain value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @importFrom digest digest
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
#' # Generate the evaluation function with Symmetrical Uncertain
#' su_evaluator <- symmetricalUncertain()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' su_evaluator(adult,'income',c('race','sex','education'))
#' }
symmetricalUncertain <- function() {
  
  symmetricalUncertainEvaluator <- function(data, class, features) {
    if (!isDataframeDiscrete(data)) {
      warning("The data seems not to be discrete, as it should be")
    }
    # Get values of the features
    features <- unlist(features) 
    feature.data <- data[, features, drop = FALSE] 
    
    # Calculate entropy of the class
    classes <- c(data[[class]]) # Obtenemos valores de la clase en numeros
    class.entropy <- entropy(classes)
    
    # Calculate the entropy of the features
    if (ncol(feature.data) == 1) {
      features.entropies <- sapply(feature.data, entropy) 
    } else {
      features.entropies <- entropyJ(table(as.factor(apply(feature.data, 1, digest))))
    }
    
    # Invoke the mutual information function
    mutual.information <- mutualInformation()(data, class, features)
    
    # calculate the symmetrical uncertain
    symm.uncertain <- (2 * mutual.information) / (class.entropy + unname(features.entropies))
    
    return(symm.uncertain)
  }

  attr(symmetricalUncertainEvaluator,'shortName') <- "symmetricalUncertain"
  attr(symmetricalUncertainEvaluator,'name') <- "Symmetrical Uncertain"
  attr(symmetricalUncertainEvaluator,'target') <- "maximize"
  attr(symmetricalUncertainEvaluator,'kind') <- "Set measure"
  attr(symmetricalUncertainEvaluator,'needsDataToBeDiscrete') <- TRUE
  attr(symmetricalUncertainEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(symmetricalUncertainEvaluator)
}
