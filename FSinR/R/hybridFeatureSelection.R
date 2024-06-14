# Hybrid Feature Selection Proccess

#' @author Francisco Aragón Royón
#' @title Hybrid Feature Selection Proccess
#' @description Performs the hybrid feature selection process. Given a hybrid search algorithm and an two evaluation methods, it uses the hybrid search algorithm in combination with the evaluation results to guide the feature selection process to an optimal subset. 
#' 
#' @param data A data.frame with the input dataset where the examples are in the rows and the features and the target variable are in the columns. The dataset should be discrete (feature columns are expected to be factors) if the following filter methods are used as evaluation methods: Rough Set Consistency, Binary Consistency, IE Consistency, IEP Consistency, Mutual Information, Gain Ratio, Symmetrical Uncertain, Gini Index or MDLC. The Jd and F-Score filter methods only work on classification problems with 2 classes in the target variable.
#' @param class The name of the dependent variable
#' @param hybridSearcher The algorithm to guide the hybrid search in the feature space. See \code{\link{hybridSearchAlgorithm}}.
#' @param evaluator_1 The first evaluation method. This method can be a filter (see \code{\link{filterEvaluator}}) or a wrapper method (see \code{\link{wrapperEvaluator}}). 
#' @param evaluator_2 The second evaluation method. This method can be a filter (see \code{\link{filterEvaluator}}) or a wrapper method (see \code{\link{wrapperEvaluator}}). If the LCC algorithm is used, the measure must evaluate feature sets.
#'
#' @return A list is returned with the results of the hybrid feature selection process:
#' \describe{
#'   \item{bestFeatures}{A vector with all features. Selected features are marked with 1, unselected features are marked with 0.}
#'   \item{bestValue}{Evaluation measure obtained with the feature selection.}
#'   \item{evaluationType_1}{Type of evaluation based on how the features have been evaluated.}
#'   \item{evaluationMethod_1}{Evaluation method used for the first evaluator.}
#'   \item{measureType_1}{Type of evaluation measure for the first evaluator.}
#'   \item{evaluationType_2}{Type of evaluation based on how the features have been evaluated for the first evaluator.}
#'   \item{evaluationMethod_2}{Evaluation method used for the second evaluator.}
#'   \item{measureType_2}{Type of evaluation measure for the second evaluator.}
#'   \item{searchMethod}{Search method used during the feature selection process for the second evaluator.}
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
#' ## Examples of the hybrid feature selection process
#' ## Classification problem with filter
#' 
#' # Generates the first filter evaluation function (individual or set measure)
#' f_evaluator_1 <- filterEvaluator('determinationCoefficient')
#' # Generates the second filter evaluation function (mandatory set measure)
#' f_evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure')
#' # Generates the hybrid search function
#' hybrid_search_method <- hybridSearchAlgorithm('LCC')
#' # Runs the hybrid feature selection process
#' res <- hybridFeatureSelection(iris,'Species',hybrid_search_method,f_evaluator_1,f_evaluator_2)
#'
#'
#' ## Classification problem with wrapper
#' 
#' # Generates the first wrapper evaluation function (individual or set measure)
#' w_evaluator_1 <- wrapperEvaluator('rf')
#' # Generates the second wrapper evaluation function (mandatory set measure)
#' w_evaluator_2 <- wrapperEvaluator('knn')
#' # Generates the hybrid search function
#' hybrid_search_method <- hybridSearchAlgorithm('LCC')
#' # Runs the hybrid feature selection process
#' res <- hybridFeatureSelection(iris,'Species',hybrid_search_method,w_evaluator_1,w_evaluator_2)
#'
#'
#' ## Classification problem mixed (with filter & wrapper)
#' 
#' # Generates the first filter evaluation function (individual or set measure)
#' f_evaluator <- filterEvaluator('determinationCoefficient')
#' # Generates the second wrapper evaluation function (mandatory set measure)
#' w_evaluator <- wrapperEvaluator('knn')
#' # Generates the hybrid search function
#' hybrid_search_method <- hybridSearchAlgorithm('LCC')
#' # Runs the hybrid feature selection process
#' res <- hybridFeatureSelection(iris, 'Species', hybrid_search_method, f_evaluator, w_evaluator)
#' }
#'
hybridFeatureSelection <- function(data, class, hybridSearcher, evaluator_1, evaluator_2){
  
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
  
  
  # The search algorithm is called with the evaluation method
  t <- proc.time() 
  fsp <- hybridSearcher(data, class, evaluator_1, evaluator_2) 
  time <- proc.time()-t   

  
  # List with the results
  res <- list()
  res[[1]] <- fsp$bestFeatures
  res[[2]] <- fsp$bestFitness
  aux <- attr(evaluator_1,"name")
  auxSeparate <- strsplit(aux, " ")
  if(auxSeparate[[1]][length(auxSeparate[[1]])]=="Wrapper"){
    res[[3]] <- "Wrapper"
    res[[4]] <- auxSeparate[[1]][1]
  }else{
    res[[3]] <- "Filter"
    res[[4]] <- aux
  }
  res[[5]] <- attr(evaluator_1,"kind")
  aux <- attr(evaluator_2,"name")
  auxSeparate <- strsplit(aux, " ")
  if(auxSeparate[[1]][length(auxSeparate[[1]])]=="Wrapper"){
    res[[6]] <- "Wrapper"
    res[[7]] <- auxSeparate[[1]][1]
  }else{
    res[[6]] <- "Filter"
    res[[7]] <- aux
  }
  res[[8]] <- attr(evaluator_2,"kind")  
  res[[9]] <- attr(hybridSearcher,"name")
  res[[10]] <- attr(evaluator_1,"target")
  res[[11]] <- ncol(data)-1
  column.names <- names(data) 
  class.position <- which(column.names == class) 
  res[[12]] <- column.names[-class.position]
  res[[13]] <- column.names[class.position]
  res[[14]] <- time
  names(res) <- c("bestFeatures", "bestValue", "evaluationType_1", "evaluationMethod_1", "measureType_1", "evaluationType_2", "evaluationMethod_2", "measureType_2","searchMethod", "target", "numFeatures", "xNames", "yName", "time")

  return(res)
}
