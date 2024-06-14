## The direct application of this function is an advanced use that consists of using this 
# function directly and performing a direct search process
## Classification problem
 
 
# Generates the filter evaluation function
filter_evaluator <- filterEvaluator('determinationCoefficient')
 
# Generates the direct search function with percentile
sp_direct_search <- selectPercentile()
# Performs the direct search process directly (parameters: dataset, target variable and evaluator)
res <- sp_direct_search(iris, 'Species', filter_evaluator)

print(res)