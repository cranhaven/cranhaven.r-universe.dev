## -----------------------------------------------------------------------------
# Load package
require(Rdiagnosislist)
require(data.table)

# Use one thread only for CRAN
data.table::setDTthreads(threads = 1)

CUSTOM <- sampleSNOMED()

NEW_DESCRIPTION <- data.table::data.table(
  conceptId = bit64::as.integer64(1:7),
  term = c(
  'IX Diseases of the circulatory system (icdchapter)',
  'I50 Heart failure (icd3)',
  'I50.0 Congestive heart failure (leaf)',
  'I50.1 Left ventricular failure (leaf)',
  'I50.9 Heart failure, unspecified (leaf)',
  'I11.0 Hypertensive heart disease with (congestive) heart failure (leaf)',
  'Heart failure (phenotype)'
  ))
NEW_RELATIONSHIP <- data.table::data.table(
  parent = bit64::as.integer64(c(1,2,2,2,2,7,7)),
  child =  bit64::as.integer64(c(2,3,4,5,6,2,6)))

CUSTOM$CONCEPT <- NEW_DESCRIPTION[, list(id = conceptId,
  effectiveTime = as.Date(Sys.Date()), active = TRUE,
  moduleId = 0, definitionStatusId = 0)]
CUSTOM$DESCRIPTION <- NEW_DESCRIPTION[, list(id = conceptId,
  effectiveTime = as.Date(Sys.Date()), active = TRUE,
  moduleId = 0, conceptId = conceptId, languageCode = 'en',
  typeId = bit64::as.integer64('900000000000003001'), # fully specified name
  term, caseSignificanceId = 0)]
CUSTOM$STATEDRELATIONSHIP <- NEW_RELATIONSHIP[, list(id = 1:.N,
  effectiveTime = as.Date(Sys.Date()), active = TRUE,
  moduleId = 0, sourceId = child, destinationId = parent,
  relationshipGroup = 0, typeId = bit64::as.integer64('116680003'), # is a
  characteristicTypeId = 0, modifierId = 0)]
CUSTOM$RELATIONSHIP <- CUSTOM$STATEDRELATIONSHIP[0]

# Using the new dictionaries
myconcept <- SNOMEDconcept('Diseases of the circulatory system',
  exact = FALSE, SNOMED = CUSTOM)
myphenotype <- SNOMEDconcept('Heart failure (phenotype)',
  SNOMED = CUSTOM)
  
# Show the concept using the new DESCRIPTION table
description(myconcept, SNOMED = CUSTOM)

# Create and view a codelist based on ICD10 and the phenotype
mycodelist <- SNOMEDcodelist(c(myconcept, myphenotype),
  include_desc = TRUE, SNOMED = CUSTOM)

# Create HTML codelist (not run)
# temp = paste0(tempdir(), 'test.html')
# htmlCodelistHierarchy(mycodelist, file = temp, SNOMED = CUSTOM)
# system(paste0('firefox ', temp, ' &')) # open in firefox (on Linux)

