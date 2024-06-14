## The direct application of this function is an advanced use that consists of using this 
# function directly and performing a search process in a feature space
## Classification problem
 
# Generates the filter evaluation function with ACO
filter_evaluator <- filterEvaluator('determinationCoefficient')
 
# Generates the search function
aco_search <- antColony()
# Performs the search process directly (parameters: dataset, target variable and evaluator)
res <- aco_search(iris, 'Species', filter_evaluator)

print(res)