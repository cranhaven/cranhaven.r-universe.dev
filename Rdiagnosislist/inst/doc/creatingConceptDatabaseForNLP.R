## -----------------------------------------------------------------------------
library(Rdiagnosislist)
require(data.table)

# Use one thread only for CRAN
data.table::setDTthreads(threads = 1)

# Load the SNOMED dictionary (for this example we are using the
# sample included with the package)
SNOMED <- sampleSNOMED()

# Create a concept database environment
miniCDB <- createCDB(SNOMED = SNOMED)

# Create a decomposition
D <- decompose('Cor pulmonale', CDB = miniCDB, noisy = TRUE)

print(D)

