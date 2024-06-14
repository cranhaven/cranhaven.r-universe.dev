# Hybrid Search Generator

#' @author Francisco Aragón Royón
#' @title Hybrid search algorithm generator
#' @description Generates a hybrid search function. This function in combination with the evaluator guides the feature selection process. Specifically, the result of calling this function is another function that is passed on as a parameter to the \code{\link{featureSelection}} function. However, you can run this function directly to perform a search process in the features space.
#'
#' @param hybridSearcher Name of the hybrid search algorithm. The available hybrid search algorithms are:
#' \describe{
#'   \item{LCC}{Linear Consistency-Constrained algorithm (LCC). See \code{\link{LCC}} }
#' }
#' @param params List with the parameters of each hybrid search method. For more details see each method. Default: empty list.
#' 
#' @return Returns a hybrid search function that is used to guide the feature selection process.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## Examples of a hybrid search algorithm generation
#'
#' hybrid_search_method <- hybridSearchAlgorithm('LCC')
#'
#'
#' ## Examples of a hybrid search algorithm generation (with parameters)
#'
#' hybrid_search_method <- hybridSearchAlgorithm('LCC', list(threshold = 0.8))
#'
#'
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to perform a hybrid search process on a feature space
#' ## Classification problem
#' 
#' # Generates the first filter evaluation function (individual or set measure)
#' filter_evaluator_1 <- filterEvaluator('determinationCoefficient')
#' # Generates the second filter evaluation function (mandatory set measure)
#' filter_evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure')
#' 
#' # Generates the hybrid search function
#' hybrid_search_method <- hybridSearchAlgorithm('LCC')
#' # Run the search process directly (params: dataset, target variable, evaluator1 & evaluator2)
#' hybrid_search_method(iris, 'Species', filter_evaluator_1, filter_evaluator_2)
#' }
hybridSearchAlgorithm <- function(hybridSearcher, params=list()){
  
  # Check the parameter
  if(!is.list(params)){
    stop('The hybrid search algorithm parameters are not in a list.')
  }
  
  # List of supported hybrid search algorithms
  algorithms <- list('LCC')
  
  switch(hybridSearcher, 
         'LCC'={
           do.call(LCC,params)
         },
         {
           stop('The hybrid search algorithm passed as a parameter is not supported by the package.')
         }
  )
    

}
attr(hybridSearchAlgorithm,'shortName') <- "hybridSearchAlgorithm"
attr(hybridSearchAlgorithm,'name') <- "Hybrid search algorithm generator"
attr(hybridSearchAlgorithm,'methods') <- c('LCC')