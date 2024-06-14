#' @author Alfonso Jiménez-Vílchez
#' @title Jd evaluation measure
#' @description Generates an evaluation function that applies the discriminant function designed by Narendra and Fukunaga \insertCite{Narendra1977}{FSinR} to generate an evaluation measure for a set of features (set measure). This function is called internally within the \code{\link{filterEvaluator}} function.
#'
#' @return Returns a function that is used to generate an evaluation set measure using the Jd.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @import dplyr
#' @importFrom stats cov
#' @import rlang
#' @importFrom rlang UQ
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to evaluate a set of features
#' ## Classification problem
#' 
#' # Generate the evaluation function with JD
#' Jd_evaluator <- Jd()
#' # Evaluate the features (parametes: dataset, target variable and features)
#' Jd_evaluator(ToothGrowth,'supp',c('len','dose'))
#' }
Jd <- function() {

  JdEvaluator <- function(data, class, features) {
    if (!length(features)) {
      return(0);
    }
    
    feature.classes <- unique(as.data.frame(data[,class,drop = FALSE]))
    if (nrow(feature.classes) != 2) {
      stop('Data set is required to have only 2 classes');
    }
    
    vectors <- data %>%
      select(features, class) %>%
      group_by_at(class) %>%
      summarise_at(features,list(mean)) %>%
      select(features)
    vector <- unlist(vectors[1,] - vectors[2,])
    
    matrixA  <- data %>%
      filter(UQ(as.name(class)) == feature.classes[1,1]) %>%
      select(features) %>%
      as.matrix() %>%
      cov()
    
    matrixB  <- data %>%
      filter(UQ(as.name(class)) == feature.classes[2,1]) %>%
      select(features) %>%
      as.matrix() %>%
      cov()
    
    return (as.numeric(t(vector) %*% solve((matrixA + matrixB)/2) %*% vector))
  }
  
  attr(JdEvaluator,'shortName') <- "Jd"
  attr(JdEvaluator,'name') <- "Jd"
  attr(JdEvaluator,'target') <- "maximize"
  attr(JdEvaluator,'kind') <- "Set measure"
  attr(JdEvaluator,'needsDataToBeDiscrete') <- FALSE
  attr(JdEvaluator,'needsDataToBeContinuous') <- FALSE
  
  return(JdEvaluator)
}
