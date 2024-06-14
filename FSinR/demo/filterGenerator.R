## Examples of a filter evaluator generation
 
filter_evaluator_1 <- filterEvaluator('cramer')
filter_evaluator_2 <- filterEvaluator('gainRatio')
filter_evaluator_3 <- filterEvaluator('MDLC')
 
 
## Examples of a filter evaluator generation (with parameters)
 
filter_evaluator_1 <- filterEvaluator('relief', list(neighbours.count=4, sample.size=15))
filter_evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure', list(iterations = 10))
 
 
## The direct application of this function is an advanced use that consists of using this 
# function directly to evaluate a set of features
## Classification problem
 
# Generates the filter evaluation function
filter_evaluator <- filterEvaluator('ReliefFeatureSetMeasure')
# Evaluates features directly (parameters: dataset, target variable and features)
filter_evaluator(iris,'Species',c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))