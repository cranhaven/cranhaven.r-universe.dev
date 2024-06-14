# Measures for regression problems, where the class is continous
#' @author Adan M. Rodriguez
#' @title R Squared, to continous features
#' @description Generates an evaluation function that calculates the determinantion coefficient \insertCite{rsquared}{FSinR} of continuous features (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the R squared value for the selected features.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with Determination Coefficient
#' dc_evaluator <- determinationCoefficient()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' dc_evaluator(longley, 'Employed', c('GNP', 'Population','Year'))
#' }
determinationCoefficient <- function() {
  
  determinationCoefficientEvaluator <- function(data, class, features) {
    # Create the formula formed with the features and the class
    #data2$class <- as.numeric(as.character(data2$class))
    formula <- as.formula(paste(class, "~", paste(features, collapse = "+")))
    
    # Fit the linear model
    model <- lm(formula = formula, data = data)
    
    # Take the measure
    value <- suppressWarnings(summary(model)$r.squared)
    
    return(value)
  }

  attr(determinationCoefficientEvaluator,'shortName') <- "determinationCoefficient"
  attr(determinationCoefficientEvaluator,'name') <- "Determination Coefficient"
  attr(determinationCoefficientEvaluator,'target') <- "maximize"
  attr(determinationCoefficientEvaluator,'kind') <- "Set measure"
  attr(determinationCoefficientEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(determinationCoefficientEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(determinationCoefficientEvaluator)
}
