## Examples of a search algorithm generation

search_method_1 <- searchAlgorithm('antColony')
search_method_2 <- searchAlgorithm('sequentialBackwardSelection')
search_method_3 <- searchAlgorithm('tabu')


## Examples of a search algorithm generation (with parameters)

search_method_1 <- searchAlgorithm('antColony', list(population=25, iter=50, verbose=TRUE))
search_method_2 <- searchAlgorithm('sequentialBackwardSelection', list(stop=TRUE))
search_method_3 <- searchAlgorithm('tabu', list(intensification=1, iterIntensification=25))


## The direct application of this function is an advanced use that consists of using this 
# function directly to perform a search process on a feature space
## Classification problem
 
# Generates the filter evaluation function
filter_evaluator <- filterEvaluator('determinationCoefficient')
 
# Generates the search function
search_method <- searchAlgorithm('hillClimbing')
# Performs the search process directly (parameters: dataset, target variable and evaluator)
search_method(iris, 'Species', filter_evaluator)