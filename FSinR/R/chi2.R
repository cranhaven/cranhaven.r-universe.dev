# Measures based on the chi squared test. Individual Measures
#' Cramer V measure
#'
#' Generates an evaluation function that calculates Cramer's V value \insertCite{Cramer1946}{FSinR}, evaluating features individually (individual measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @importFrom Rdpack reprompt
#' @return Returns a function that is used to generate an individual evaluation measure using Cramer V.
#' @references
#'    \insertAllCited{}
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to individually evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with Cramer
#' cramer_evaluator <- cramer()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' cramer_evaluator(iris,'Species',c('Sepal.Length'))
#' }
cramer <- function() {
  
  cramerEvaluator <- function(data, class, features) {
    measures <- c()
    
    # Get values of the features and the class
    features <- unlist(features)
    feature.data <- data[, features, drop = FALSE]
    classes <- c(data[[class]])
    
    # For each individual feature...
    for (i in feature.data) {
      # Count the number of times that each class is repeated for an attribute value
      joint <- table(classes, i)
      
      # Count the number of times the class 'j' appears
      row.sums <- rowSums(joint)
      
      # Count the number of times the value 'i' appears in the feature
      feature.count <- colSums(joint)
      
      # Calculate frequency of each class
      num.elements <- length(i)
      class.prob <- as.matrix(row.sums) / num.elements
      
      # Transpose matrix with t() function, and calculate matrix multiplication
      expected <- t(as.matrix(feature.count) %*% t(as.matrix(class.prob)))
      # (observed - expected)² / expected
      # Fix to avoid joint = 0, expected = 0, chi.squared = NaN:
      jointExpectedRelation <- ((joint - expected) ^ 2) / expected
      jointExpectedRelation <- replace(jointExpectedRelation, is.na(jointExpectedRelation), 1)
      chi.squared <- sum(jointExpectedRelation)
      
      # Calculate Cramer V measure: sqrt( (X²/n) / (min(k-1, r-1))
      if (length(row.sums) < 2 || length(feature.count) < 2 || chi.squared == 0) {
        result <- 0
      } else {
        k <- ncol(joint)
        r <- nrow(joint)
        cramer <- sqrt( (chi.squared / num.elements) / min(k - 1, r - 1))
        result <- cramer
      }
      
      # Store a value for each feature
      measures[length(measures) + 1] <- result
    }
    
    if (length(features) == 1) {
      return(measures[1])
    }
    names(measures) <- features
    return(measures)
  }

  attr(cramerEvaluator,'name') <- "Cramer"
  attr(cramerEvaluator,'shortName') <- "cramer"
  attr(cramerEvaluator,'target') <- "maximize"
  attr(cramerEvaluator,'kind') <- "Individual measure"
  attr(cramerEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(cramerEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(cramerEvaluator)
}

#' Chi squared measure
#'
#' Generates an evaluation function that calculates the Chi squared value \insertCite{Pearson1900}{FSinR}, evaluating the selected features individually (individual measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @importFrom Rdpack reprompt
#' @return Returns a function that is used to generate an individual evaluation measure using chi squared.
#' @references
#'    \insertAllCited{}
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to individually evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with Chi squared
#' chiSquared_evaluator <- chiSquared()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' chiSquared_evaluator(iris,'Species',c('Sepal.Length'))
#' }
chiSquared <- function() {
  
  chiSquaredEvaluator <- function(data, class, features) {
    measures <- c()
    
    # Get values of the features and the class
    features <- unlist(features)
    feature.data <- data[, features, drop = FALSE]
    classes <- c(data[[class]])
    
    # For each individual feature...
    for (i in feature.data) {
      # Count the number of times that each class is repeated for an attribute value
      joint <- table(classes, i)
      
      # Count the number of times the class 'j' appears
      row.sums <- rowSums(joint)
      
      # Count the number of times the value 'i' appears in the feature
      feature.count <- colSums(joint)
      
      # Calculate frequency of each class
      num.elements <- length(i)
      class.prob <- as.matrix(row.sums) / num.elements
      
      # Transpose matrix with t() function, and calculate matrix multiplication
      expected <- t(as.matrix(feature.count) %*% t(as.matrix(class.prob)))
      expected.nonZero <- expected
      expected.nonZero[expected.nonZero == 0] <- 1
      chi.squared <- sum(((joint - expected) ^ 2) / expected.nonZero)
      
      # Store a value for each feature
      measures[length(measures) + 1] <- chi.squared
    }
    if (length(features) == 1) {
      return(measures[1])
    }
    names(measures) <- features
    return(measures)
  }

  attr(chiSquaredEvaluator,'shortName') <- "chiSquared"
  attr(chiSquaredEvaluator,'name') <- "Chi Squared"
  attr(chiSquaredEvaluator,'target') <- "maximize"
  attr(chiSquaredEvaluator,'kind') <- "Individual measure"
  attr(chiSquaredEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(chiSquaredEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(chiSquaredEvaluator)
}
