#' F-score measure
#'
#' Generates an evaluation function that calculates the F-score approach defined in \insertCite{Wang2018}{FSinR} (individual measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an individual evaluation measure using the F-score.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import dplyr
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to individually evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with F-Score
#' fscore_evaluator <- fscore()
#' # Evaluate the features (parameters: dataset, target variable and features)
#' fscore_evaluator(ToothGrowth, 'supp', c('len'))
#' }
fscore <- function() {

  fscoreEvaluator <- function(data, class, features) {
    if (!length(features)) {
      return(0);
    }
    
    feature.classes <- unique(as.data.frame(data[,class,drop = FALSE]))
    if (nrow(feature.classes) != 2) {
      stop('Data set is required to have only 2 classes');
    }
    
    
    measures <- c()  
    for (feature in features) {
      x.mean = mean(data[,feature])
      
      x_plus  <- data %>%
        filter(UQ(as.name(class)) == feature.classes[1,1]) %>%
        select(feature) %>%
        as.matrix()
      x_plus.mean = mean(x_plus)
      x_plus.n = nrow(x_plus)
      
      x_minus  <- data %>%
        filter(UQ(as.name(class)) == feature.classes[2,1]) %>%
        select(feature) %>%
        as.matrix()
      x_minus.mean = mean(x_minus)
      x_minus.n = nrow(x_minus)
      
      x_plus.sum = 0
      for (x in x_plus) {
        x_plus.sum = x_plus.sum + (x - x_plus.mean)^2
      }
      x_plus.sum = x_plus.sum / (x_plus.n - 1)
      
      x_minus.sum = 0
      for (x in x_minus) {
        x_minus.sum = x_minus.sum + (x - x_minus.mean)^2
      }
      x_minus.sum = x_minus.sum / (x_minus.n - 1)
      measures[length(measures) + 1] <- ((x_plus.mean - x.mean)^2 + (x_minus.mean - x.mean)^2) / (x_plus.sum + x_minus.sum)
    }
    if (length(features) == 1) {
      return(measures[1])
    }
    names(measures) <- features
    return(measures)
  }

  attr(fscoreEvaluator,'shortName') <- "fscore"
  attr(fscoreEvaluator,'name') <- "F-score"
  attr(fscoreEvaluator,'target') <- "maximize"
  attr(fscoreEvaluator,'kind') <- "Individual measure"
  attr(fscoreEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(fscoreEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(fscoreEvaluator)
}