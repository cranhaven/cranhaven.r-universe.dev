# Direct Search Generator

#' @author Francisco Aragón Royón
#' @title Direct search algorithm generator
#' @description Generates a direct search function. This function in combination with the evaluator composes the feature selection process. Specifically, the result of calling this function is another function that is passed on as a parameter to the \code{\link{directFeatureSelection}} function. However, you can run this function directly to perform a direct search process.
#'
#' @param directSearcher Name of the direct search algorithm. The available direct search algorithms are:
#' \describe{
#'   \item{selectKBest}{See \code{\link{selectKBest}} }
#'   \item{selectPercentile}{See \code{\link{selectPercentile}} }
#'   \item{selectThreshold}{See \code{\link{selectThreshold}} }
#'   \item{selectThresholdRange}{See \code{\link{selectThresholdRange}} }
#'   \item{selectDifference}{See \code{\link{selectDifference}} }
#'   \item{selectSlope}{See \code{\link{selectSlope}} }
#' }
#' @param params List with the parameters of each direct search method. For more details see each method. Default: empty list.
#' 
#' @return Returns a direct search function that is used in the feature selection process.
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## Examples of a direct search algorithm generation
#'
#' direct_search_method_1 <- directSearchAlgorithm('selectKBest')
#' direct_search_method_2 <- directSearchAlgorithm('selectPercentile')
#' direct_search_method_3 <- directSearchAlgorithm('selectThreshold')
#'
#'
#' ## Examples of a direct search algorithm generation (with parameters)
#'
#' direct_search_method_1 <- directSearchAlgorithm('selectKBest', list(k=2))
#' direct_search_method_2 <- directSearchAlgorithm('selectPercentile', list(percentile=25))
#' direct_search_method_3 <- directSearchAlgorithm('selectThreshold', list(threshold=0.55))
#'
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to perform a direct search process
#' ## Classification problem
#' 
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the direct search function
#' direct_search_method <- directSearchAlgorithm('selectKBest')
#' # Performs the diret search process directly (parameters: dataset, target variable and evaluator)
#' direct_search_method(iris, 'Species', filter_evaluator)
#' }
directSearchAlgorithm <- function(directSearcher, params=list()){
  
  # Check the parameter
  if(!is.list(params)){
    stop('The direct search algorithm parameters are not in a list.')
  }
  
  # List of supported direct search algorithms
  algorithms <- list('selectKBest', 'selectPercentile', 'selectThreshold', 'selectThresholdRange', 'selectDifference', 'selectSlope')
  
  switch(directSearcher, 
         'selectKBest'={
           do.call(selectKBest,params)
         },
         'selectPercentile'={
           do.call(selectPercentile,params)
         },
         'selectThreshold'={
           do.call(selectThreshold,params)
         },
         'selectThresholdRange'={
           do.call(selectThresholdRange,params)
         },
         'selectDifference'={
           do.call(selectDifference,params)
         },
         'selectSlope'={
           do.call(selectSlope,params)
         },
         {
           stop('The direct search algorithm passed as a parameter is not supported by the package.')
         }
  )
    

}
attr(directSearchAlgorithm,'shortName') <- "directSearchAlgorithm"
attr(directSearchAlgorithm,'name') <- "Direct search algorithm generator"
attr(directSearchAlgorithm,'methods') <- c('selectKBest', 'selectPercentile', 'selectThreshold', 'selectThresholdRange', 'selectDifference', 'selectSlope')