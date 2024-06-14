## The direct application of this function is an advanced use that consists of using this 
# function directly and performing a search process in a feature space
## Classification problem
 
# Generates the filter evaluation function
filter_evaluator <- filterEvaluator('determinationCoefficient')
 
# Generates the search function with Deep first
dfs_search <- deepFirst()
# Performs the search process directly (parameters: dataset, target variable and evaluator)
res <- dfs_search(iris, 'Species', filter_evaluator)

print(res)