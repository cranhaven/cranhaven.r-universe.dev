# Search Generator

#' @author Francisco Aragón Royón
#' @title Search algorithm generator
#' @description Generates a search function. This function in combination with the evaluator guides the feature selection process. Specifically, the result of calling this function is another function that is passed on as a parameter to the \code{\link{featureSelection}} function. However, you can run this function directly to perform a search process in the features space.
#'
#' @param searcher Name of the search algorithm. The available search algorithms are:
#' \describe{
#'   \item{antColony}{Ant colony optimization (ACO). See \code{\link{antColony}} }
#'   \item{breadthFirst}{Breadth first search. See \code{\link{breadthFirst}} }
#'   \item{deepFirst}{Deep first search. See \code{\link{deepFirst}} }
#'   \item{geneticAlgorithm}{Genetic algorithm (GA). See \code{\link{geneticAlgorithm}} }
#'   \item{hillClimbing}{Hill-Climbing (HC). See \code{\link{hillClimbing}} }
#'   \item{LasVegas}{Las Vegas (LV). See \code{\link{LasVegas}} }
#'   \item{sequentialBackwardSelection}{Sequential backward selection (sbs). See \code{\link{sequentialBackwardSelection}} }
#'   \item{sequentialFloatingForwardSelection}{Sequential floating forward selection (sffs). See \code{\link{sequentialFloatingForwardSelection}} }
#'   \item{sequentialFloatingBackwardSelection}{Sequential floating backward selection (sfbs). See \code{\link{sequentialFloatingBackwardSelection}} }
#'   \item{sequentialForwardSelection}{Sequential forward selection (sfs). See \code{\link{sequentialForwardSelection}} }
#'   \item{simulatedAnnealing}{Simulated annealing (SA). See \code{\link{simulatedAnnealing}} }
#'   \item{tabu}{Tabu search (TS). See \code{\link{tabu}} }
#'   \item{whaleOptimization}{Whale optimization algorithm (WOA). See \code{\link{whaleOptimization}} }
#' }
#' @param params List with the parameters of each search method. For more details see each method. Default: empty list.
#' 
#' @return Returns a search function that is used to guide the feature selection process
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## Examples of a search algorithm generation
#'
#' search_method_1 <- searchAlgorithm('antColony')
#' search_method_2 <- searchAlgorithm('sequentialBackwardSelection')
#' search_method_3 <- searchAlgorithm('tabu')
#'
#'
#' ## Examples of a search algorithm generation (with parameters)
#'
#' search_method_1 <- searchAlgorithm('antColony', list(population=25, iter=50, verbose=TRUE))
#' search_method_2 <- searchAlgorithm('sequentialBackwardSelection', list(stop=TRUE))
#' search_method_3 <- searchAlgorithm('tabu', list(intensification=1, iterIntensification=25))
#'
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to perform a search process on a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function
#' search_method <- searchAlgorithm('hillClimbing')
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' search_method(iris, 'Species', filter_evaluator)
#' }
searchAlgorithm <- function(searcher, params=list()){
  
  # Check the parameter
  if(!is.list(params)){
    stop('The search algorithm parameters are not in a list.')
  }
  
  # List of supported search algorithms
  algorithms <- list('whaleOptimization', 'tabu', 'sequentialForwardSelection', 'sequentialBackwardSelection', 'sequentialFloatingForwardSelection', 'sequentialFloatingBackwardSelection', 'simulatedAnnealing', 'LasVegas', 'hillClimbing', 'geneticAlgorithm', 'antColony', 'breadthFirst', 'deepFirst')
  
  switch(searcher, 
         'sequentialForwardSelection'={
           do.call(sequentialForwardSelection,params)
         },
         'sequentialBackwardSelection'={
           do.call(sequentialBackwardSelection,params)
         },
         'sequentialFloatingBackwardSelection'={
           do.call(sequentialFloatingBackwardSelection,params)
         },
         'sequentialFloatingForwardSelection'={
           do.call(sequentialFloatingForwardSelection,params)
         },
         'deepFirst'={
           do.call(deepFirst,params)
         },
         'breadthFirst'={
           do.call(breadthFirst,params)
         },
         'geneticAlgorithm'={
           do.call(geneticAlgorithm,params)
         },
         'simulatedAnnealing'={
           do.call(simulatedAnnealing,params)
         },
         'whaleOptimization'={
           do.call(whaleOptimization,params)
         },
         'antColony'={
           do.call(antColony,params)
         },
         'hillClimbing'={
           do.call(hillClimbing,params)
         },
         'tabu'={
           do.call(tabu,params)
         },
         'LasVegas'={
           do.call(LasVegas,params)
         },
         {
           stop('The search algorithm passed as a parameter is not supported by the package.')
         }
  )
    

}
attr(searchAlgorithm,'shortName') <- "searchAlgorithm"
attr(searchAlgorithm,'name') <- "Search algorithm generator"
attr(searchAlgorithm,'methods') <- c('whaleOptimization', 'tabu', 'sequentialForwardSelection', 'sequentialBackwardSelection', 'sequentialFloatingForwardSelection', 'sequentialFloatingBackwardSelection', 'simulatedAnnealing', 'LasVegas', 'hillClimbing', 'geneticAlgorithm', 'antColony', 'breadthFirst', 'deepFirst')