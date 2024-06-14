# Direct Feature Selection Proccess

#' @author Francisco Aragón Royón
#' @title Direct Feature Selection Proccess
#' @description Performs the direct feature selection process. Given a direct search algorithm and an evaluation method, it uses the direct search algorithm in combination with the evaluation results to guide the feature selection process to an optimal subset. 
#' 
#' @param data A data.frame with the input dataset where the examples are in the rows and the features and the target variable are in the columns. The dataset should be discrete (feature columns are expected to be factors) if the following filter methods are used as evaluation methods: Rough Set Consistency, Binary Consistency, IE Consistency, IEP Consistency, Mutual Information, Gain Ratio, Symmetrical Uncertain, Gini Index or MDLC. The Jd and F-Score filter methods only work on classification problems with 2 classes in the target variable.
#' @param class The name of the dependent variable
#' @param directSearcher The algorithm to conduct the direct feature search. See \code{\link{directSearchAlgorithm}}.
#' @param evaluator The evaluation method to obtain a measure of the features. The evaluation method can be a filter (see \code{\link{filterEvaluator}}) or a wrapper method (see \code{\link{wrapperEvaluator}}).
#'
#' @return A list is returned with the results of the direct feature selection process:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0.}
#'   \item{featuresSelected}{The names of the returned features sorted according to the result of the evaluation measure}
#'   \item{valuePerFeature}{The evaluation measures of the returned features}
#'   \item{evaluationType}{Type of evaluation based on how the features have been evaluated.}
#'   \item{evaluationMethod}{Evaluation method used.}
#'   \item{searchMethod}{Search method used during the feature selection process.}
#'   \item{target}{A character indicating if the objective of the process is to minimize or maximize the evaluation measure.}
#'   \item{numFeatures}{Number of features in the problem.}
#'   \item{xNames}{Name of the features.}
#'   \item{yNames}{Name of the dependent variable.}
#'   \item{time}{Value of class 'proc_time' containing the user time, system time, and total time of the feature selection process.}
#' } 
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## Examples of the direct feature selection process
#' ## Classification problem with filter
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('ReliefFeatureSetMeasure')
#' # Generates the direct search function
#' direct_search_method <- directSearchAlgorithm('selectKBest')
#' # Runs the direct feature selection process
#' res <- directFeatureSelection(iris, 'Species', direct_search_method, filter_evaluator)
#'
#'
#' ## Classification problem with wrapper
#' 
#' # Generates the wraper evaluation function
#' wrapper_evaluator <- wrapperEvaluator('knn')
#' # Generates the direct search function
#' direct_search_method <- directSearchAlgorithm('selectKBest')
#' # Runs the direct feature selection process
#' res <- directFeatureSelection(iris, 'Species', direct_search_method, wrapper_evaluator)
#'
#'
#' ## Examples of the direct feature selection process (with parameters)
#' ## Regression problem with filter
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('relief', list(neighbours.count = 4))
#' # Generates the direct search function
#' direct_search_method <- directSearchAlgorithm('selectKBest', list(k=2))
#' # Runs the direct feature selection process
#' res <- directFeatureSelection(mtcars, 'mpg', direct_search_method, filter_evaluator)
#'
#'
#' ## Regression problem with wrapper
#' 
#' # Values for the caret trainControl function (resampling parameters)
#' resamplingParams <- list(method = "cv", repeats = 5)
#' # Values for the caret train function (fitting parameters)
#' fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
#'                       tuneGrid = expand.grid(k = c(1:12)))
#' # Generates the wraper evaluation function
#' wrapper_evaluator <- wrapperEvaluator('knn', resamplingParams, fittingParams)
#' # Generates the direct search function
#' direct_search_method <- directSearchAlgorithm('selectKBest',list(k=2))
#' # Runs the direct feature selection process
#' res <- directFeatureSelection(mtcars, 'mpg', direct_search_method, wrapper_evaluator)
#' }
#'
directFeatureSelection <- function(data, class, directSearcher, evaluator){
  
  # Check that the name of the dependent variable exists
  if(!class%in%colnames(data)){
    stop('The dependent variable does not exist in the dataset.')
  }
  # Check the number of columns
  if((ncol(data)-1)<2){
    stop('The feature selection process requires more than 1 feature')
  }
  # Check for missing data
  if( any( apply(data, 2, function(x) { any(is.na(x)) } ) ) ){
    stop('Feature selection cannot be performed with missing values. Try to impute them previously with the preProcces function of the caret package')
  }
  
  
  # The direct search algorithm is called with the evaluation method
  t <- proc.time() 
  dfsp <- directSearcher(data, class, evaluator) 
  time <- proc.time()-t   

  
  # List with the results
  res <- list()
  res[[1]] <- dfsp$bestFeatures
  res[[2]] <- dfsp$featuresSelected
  res[[3]] <- dfsp$valuePerFeature
  aux <- attr(evaluator,"name")
  auxSeparate <- strsplit(aux, " ")
  if(auxSeparate[[1]][length(auxSeparate[[1]])]=="Wrapper"){
    res[[4]] <- "Wrapper"
    res[[5]] <- auxSeparate[[1]][1]
  }else{
    res[[4]] <- "Filter"
    res[[5]] <- aux
  }
  res[[6]] <- attr(directSearcher,"name")
  res[[7]] <- attr(evaluator,"target")
  res[[8]] <- ncol(data)-1
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  res[[9]] <- column.names[-class.position]
  res[[10]] <- column.names[class.position]
  res[[11]] <- time
  names(res) <- c("bestFeatures", "featuresSelected", "valuePerFeature", "evaluationType", "evaluationMethod", "searchMethod", "target", "numFeatures", "xNames", "yName", "time")

  return(res)
}
