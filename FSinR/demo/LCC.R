## The direct application of this function is an advanced use that consists of using this 
# function directly and performing a hybrid search process in a feature space
## Classification problem
 
# Generates the first filter evaluation function (individual or set measure)
filter_evaluator_1 <- filterEvaluator('determinationCoefficient')
# Generates the second filter evaluation function (mandatory set measure)
filter_evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure')
 
   
# Generates the hybrid search function with LCC
LCC_hybrid_search <- LCC()
# Run the search process directly (params: dataset, target variable, evaluator1 & evaluator2)
res <-LCC_hybrid_search(iris, 'Species', filter_evaluator_1, filter_evaluator_2)

print(res)