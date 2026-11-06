## Load data
data('cardealers4')
input <- cardealers4[, c('Employees', 'Depreciation')]
output <- cardealers4[, c('CarsSold', 'WorkOrders')]

## Solve dea model using adea from adea package
adea(input, output)

## Solve again and store result in sol.dea
sol.adea <- adea(input, output)
## Scores
sol.adea$eff
## Weights
cbind(sol.adea$ux, sol.adea$vy)
## Input variable loads
sol.adea$loads$input
## Output variable loads
sol.adea$loads$output
## Model load
sol.adea$loads$load

## Compute all dea models in hierarchical way
adea_hierarchical(input, output)
## Compute again and store result in sol.ah
sol.ah <- adea_hierarchical(input, output)
## Get the model with 3 variables
sol.ah$models[[3]]
## Get efficiencies for model with 3 variables
sol.ah$models[[3]]$eff
## Get input names in model with 3 variables
sol.ah$inputnames[[3]]
## Get output namess in model with 3 variables
sol.ah$outputnames[[3]]

## Compute all dea models in parametric way
adea_parametric(input, output)
## Compute again and store result in sol.ap
sol.ap <- adea_parametric(input, output)
## Get the model with 3 variables
sol.ap$models[[3]]
## Get efficiencies for model with 3 variables
sol.ap$models[[3]]$eff
## Get input names in model with 3 variables
sol.ap$inputnames[[3]]
## Get output names in model with 3 variables
sol.ap$outputnames[[3]]

