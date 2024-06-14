## Examples of the feature selection process
## Classification problem with filter
 
# Generates the filter evaluation function
filter_evaluator <- filterEvaluator('ReliefFeatureSetMeasure')
# Generates the search function
search_method <- searchAlgorithm('hillClimbing')
# Runs the feature selection process
res <- featureSelection(iris, 'Species', search_method, filter_evaluator)


## Classification problem with wrapper
 
# Generates the wraper evaluation function
wrapper_evaluator <- wrapperEvaluator('knn')
# Generates the search function
search_method <- searchAlgorithm('hillClimbing')
# Runs the feature selection process
res <- featureSelection(iris, 'Species', search_method, wrapper_evaluator)


## Examples of the feature selection process (with parameters)
## Regression problem with filter
 
# Generates the filter evaluation function
filter_evaluator <- filterEvaluator('ReliefFeatureSetMeasure', list(iterations = 10))
# Generates the search function
search_method <- searchAlgorithm('hillClimbing', list(repeats=2))
# Runs the feature selection process
res <- featureSelection(mtcars, 'mpg', search_method, filter_evaluator)


## Regression problem with wrapper
 
# Values for the caret trainControl function (resampling parameters)
resamplingParams <- list(method = "cv", repeats = 5)
# Values for the caret train function (fitting parameters)
fittingParams <- list(preProc = c("center", "scale"), metric="RMSE",
                       tuneGrid = expand.grid(k = c(1:12)))
# Generates the wraper evaluation function
wrapper_evaluator <- wrapperEvaluator('knn', resamplingParams, fittingParams)
# Generates the search function
search_method <- searchAlgorithm('geneticAlgorithm',list(popSize=10, maxiter=25, verbose=TRUE))
# Runs the feature selection process
res <- featureSelection(mtcars, 'mpg', search_method, wrapper_evaluator)