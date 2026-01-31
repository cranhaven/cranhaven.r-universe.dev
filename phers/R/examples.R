example1 = function() {
  ex = "
@examples
library('data.table')
library('survival')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights using the prevalence method
weightsPrev = getWeights(demoSample, phecodeOccurrences)

# calculate weights using the prevalence method
# (assign negative weights to those with zero phecode occurrence)
weightsPrevNeg = getWeights(
  demoSample, phecodeOccurrences, negativeWeights = TRUE)

# calculate weights using the logistic method
weightsLogistic = getWeights(
  demoSample, phecodeOccurrences, method = 'logistic', methodFormula = ~ sex)

# calculate weights using the loglinear method
phecodeOccurrences2 = phecodeOccurrences[, .(
  num_occurrences = uniqueN(entry_date)), by = .(person_id, phecode)]
weightsLoglinear = getWeights(
  demoSample, phecodeOccurrences2, method = 'loglinear', methodFormula = ~ sex)

# calculate weights using the cox method
phecodeOccurrences3 = phecodeOccurrences[, .(
  first_occurrence_date = min(entry_date)) , by = .(person_id, phecode)]
phecodeOccurrences3 = merge(
  phecodeOccurrences3, demoSample[, .(person_id, dob)], by = 'person_id')
phecodeOccurrences3[,
  occurrence_age := as.numeric((first_occurrence_date - dob)/365.25)]
phecodeOccurrences3[, `:=`(first_occurrence_date = NULL, dob = NULL)]
demoSample3 = demoSample[, .(
  person_id, sex,
  first_age = as.numeric((first_visit_date - dob)/365.25),
  last_age = as.numeric((last_visit_date - dob)/365.25))]
weightsCox = getWeights(
  demoSample3, phecodeOccurrences3, method = 'cox', methodFormula = ~ sex)

# calculate weights using pre-calculated weights based on data from
# Vanderbilt University Medical Center
weightsPreCalc = getWeights(
  demoSample, phecodeOccurrences, method = 'prevalence_precalc')
"
  return(strsplit(ex, split = '\n')[[1L]])}


example2 = function() {
  ex = "
@examples
library('data.table')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights (using the prevalence method)
weights = getWeights(demoSample, phecodeOccurrences)

# OMIM disease IDs for which to calculate phenotype risk scores
diseaseId = 154700

# map diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate scores
scores = getScores(weights, diseasePhecodeMap[disease_id == diseaseId])

# calculate residual scores
rscores = getResidualScores(demoSample, scores, lmFormula = ~ sex)
"
  return(strsplit(ex, split = '\n')[[1L]])}


example3 = function() {
  ex = "
@examples
library('data.table')


icdSample1 = merge(icdSample, demoSample[, .(person_id, dob)], by = 'person_id')
icdSample1[, occurrence_age := as.numeric((entry_date - dob)/365.25)]
icdSample1[, `:=`(entry_date = NULL, dob = NULL)]

dxStatus = getDxStatus(demoSample, icdSample1)
"
  return(strsplit(ex, split = '\n')[[1L]])}


example4 = function() {
  ex = "
@examples
library('data.table')
library('BEDMatrix')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights
weights = getWeights(demoSample, phecodeOccurrences)

# OMIM disease IDs for which to calculate phenotype risk scores
diseaseId = 154700

# map diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate scores
scores = getScores(weights, diseasePhecodeMap[disease_id == diseaseId])

# map diseases to genetic variants
nvar = 10
diseaseVariantMap = data.table(disease_id = diseaseId, variant_id = paste0('snp', 1:nvar))

# load sample genetic data
npop = 50
genoSample = BEDMatrix(system.file('extdata', 'geno_sample.bed', package = 'phers'))
colnames(genoSample) = paste0('snp', 1:nvar)
rownames(genoSample) = 1:npop

# run genetic association tests
genoStats = getGeneticAssociations(
  scores, genoSample, demoSample, diseaseVariantMap, lmFormula = ~ sex,
  modelType = 'additive')
"
  return(strsplit(ex, split = '\n')[[1L]])}
