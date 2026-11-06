## Load data and setup rownames
data('spanishuniversities2018')
rownames(spanishuniversities2018) <- spanishuniversities2018$University
input <- spanishuniversities2018[, c('TeachingStaff'), drop = FALSE]
output <- spanishuniversities2018[, c("FPU2018s", "FPI2018s", "Patents", "PhDTheses", "JCRs", "Sixs", "Projects")]

## Solve dea model
cat('\nSolution using dea\n')
cat('--- Scores ---\n')
(sol.dea <- dea(input, output))
cat('--- Weights ---\n')
cbind(sol.dea$ux, sol.dea$vy)

## Solve adea model
cat('\nadea model\n')
cat('--- Scores ---\n')
(sol.adea <- adea(input, output))
cat('--- Weights ---\n')
cbind(sol.adea$ux, sol.adea$vy)
cat('--- Input variable Loads ---\n')
sol.adea$loads$input
cat('--- Output variable Loads ---\n')
sol.adea$loads$output
cat('--- Model load ---\n')
sol.adea$loads$load

