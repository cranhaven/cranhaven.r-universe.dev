## The direct application of this function is an advanced use that consists of using this 
# function directly and performing a direct search process
## Classification problem
 
 
 # Generates the filter evaluation function
filter_evaluator <- filterEvaluator('determinationCoefficient')
 
# Generates the direct search function with threshold
st_direct_search <- selectThreshold()
# Performs the direct search process directly (parameters: dataset, target variable and evaluator)
res <- st_direct_search(iris, 'Species', filter_evaluator) 

print(res)