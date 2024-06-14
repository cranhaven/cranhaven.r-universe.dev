## Examples of a wrapper evaluator generation

wrapper_evaluator_1 <- wrapperEvaluator('knn')
wrapper_evaluator_2 <- wrapperEvaluator('mlp')
wrapper_evaluator_3 <- wrapperEvaluator('randomForest')


## Examples of a wrapper evaluator generation (with parameters)

# Values for the caret trainControl function (resampling parameters)
resamplingParams <- list(method = "repeatedcv", repeats = 3)
# Values for the caret train function (fitting parameters)
fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
                       tuneGrid = expand.grid(k = c(1:12)))
                       
wrapper_evaluator <- wrapperEvaluator('knn', resamplingParams, fittingParams)
 
 
## The direct application of this function is an advanced use that consists of using this 
# function directly to evaluate a set of features
## Classification problem
 
# Generates the wrapper evaluation function
wrapper_evaluator <- wrapperEvaluator('knn')
# Evaluates features directly (parameters: dataset, target variable and features)
wrapper_evaluator(iris,'Species',c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))