## -----------------------------------------------------------------------------
# Load packages
require(Rdiagnosislist)
require(data.table)

# Use one thread only for CRAN
data.table::setDTthreads(threads = 1)

# The sampleSNOMED() function returns an environment containing
# the sample dictionaries
TEST <- sampleSNOMED()

# TEST is now an environment containing the sample SNOMED CT dictionary.
# Objects within the environment can be retrieved using the $ operator
# or the 'get' function. We will export the sample dictionaries to a
# temporary folder to show how to reload them using loadSNOMED()
exportSNOMEDenvir(TEST, tempdir())
dir(tempdir())

# loadSNOMED searches for files containing '_Concept_', '_Description_',
# '_StatedRelationship_', '_Relationship_', 'Refset_SimpleMap',
# 'Refset_ExtendedMap' or 'Refset_Simple', as in the actual SNOMED CT
# release files.

# Import using the loadSNOMED function
SNOMED <- loadSNOMED(tempdir(), active_only = FALSE)

## -----------------------------------------------------------------------------
# Make sure the SNOMED environment is available and contains the SNOMED dictionary
SNOMEDconcept('Heart failure', SNOMED = SNOMED)

# To use the sample SNOMED dictionary for testing
SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())

# If an object named SNOMED containing the SNOMED dictionary is available
# in the current environment, it does not need to be stated in the
# function call
SNOMED <- sampleSNOMED()
SNOMEDconcept('Heart failure')

# The argument 'exact' can be used to specify whether a regular expression
# search should be done, e.g.
SNOMEDconcept('Heart f', exact = FALSE)

# The 'description' function can be used to return the descriptions of
# the concepts found. It returns a data.table with the fully specified 
# name for each term.
description(SNOMEDconcept('Heart f', exact = FALSE))

# The 'semantic type' function returns the semantic type of the concept
# from the Fully Specified Name
semanticType(SNOMEDconcept('Heart failure'))

# Functions which expect a SNOMEDconcept object, such as semanticType,
# will automatically convert their argument to SNOMEDconcept using the
# function as.SNOMEDconcept
semanticType('Heart failure')

## -----------------------------------------------------------------------------
# A list of concepts with a description containing the term 'heart'
# (not that all synonyms are searched, not just the Fully Specified Names)
heart <- SNOMEDconcept('Heart|heart', exact = FALSE, SNOMED = sampleSNOMED())

# A list of concepts containing the term 'fail'
fail <- SNOMEDconcept('Fail|fail', exact = FALSE, SNOMED = sampleSNOMED())

# Concepts with heart and fail
intersect(heart, fail)

# Concepts with heart and not fail
setdiff(heart, fail)

# Concepts with heart or fail
union(heart, fail)

## -----------------------------------------------------------------------------
SNOMED <- sampleSNOMED()

# Parents (immediate ancestors)
parents('Acute heart failure')

# Ancestors
ancestors('Acute heart failure')

# Children (immediate descendants)
children('Acute heart failure')

# Descendants
descendants('Acute heart failure')

## -----------------------------------------------------------------------------
SNOMED <- sampleSNOMED()

# List all the attributes of a concept
print(attrConcept('Heart failure'))

# 'Finding site' of a particular disorder
relatedConcepts('Heart failure', 'Finding site')

# Disorders with a 'Finding site' of 'Heart'
relatedConcepts('Heart', 'Finding site', reverse = TRUE)

## -----------------------------------------------------------------------------
SNOMED <- sampleSNOMED()

# Create a codelist containing all the descendants of
# the concept 'Heart failure'
my_heart_failure_codelist <- SNOMEDcodelist(
  SNOMEDconcept('Heart failure'), include_desc = TRUE,
  format = 'simple', codelist_name = 'Heart failure')

# Original codelist
print(my_heart_failure_codelist)

# Convert to tree format
tree <- SNOMEDcodelist(my_heart_failure_codelist, format = 'tree')
print(tree)

# Write out codelist to file
# Metadata are stored in a column named 'metadata'
# export(tree, file = paste0(tempdir(), '/hf_codes.csv'))

# Reload codelist from file (including metadata)
# reloaded_codelist <- as.SNOMEDcodelist(
#  data.table::fread(paste0(tempdir(), '/hf_codes.csv')))
# print(reloaded_codelist)

## -----------------------------------------------------------------------------
SNOMED = sampleSNOMED()

# Obtain a list of available refsets with descriptions and counts
merge(SNOMED$REFSET[, .N, by = list(conceptId = refsetId)],
  SNOMED$DESCRIPTION[, list(conceptId, term)], by = 'conceptId')

# Obtain a refset as a SNOMEDconcept vector
renal_ref <- getRefset('Renal clinical finding simple reference set')

# Find out whether a concept is included in a refset
SNOMEDconcept('Renal failure') %in% renal_ref

## -----------------------------------------------------------------------------
# Example: creating an ICD-10 heart failure codelist using SNOMED CT
SNOMED <- sampleSNOMED()

my_heart_failure_codelist <- SNOMEDcodelist(
  SNOMEDconcept('Heart failure'), include_desc = FALSE)

getMaps(my_heart_failure_codelist, to = c('icd10'))

my_pacemaker_codelist <- SNOMEDcodelist(
  SNOMEDconcept('Implantation of cardiac pacemaker'),
  include_desc = FALSE)

getMaps(my_pacemaker_codelist, to = c('opcs4'))

## -----------------------------------------------------------------------------
# Example: creating a Read heart failure codelist using SNOMED CT
SNOMED <- sampleSNOMED()
data(READMAPS)

# Start off with a SNOMED CT codelist containing the descendants of
# the concept 'Heart failure'
my_heart_failure_codelist <- SNOMEDcodelist(
  SNOMEDconcept('Heart failure'), include_desc = TRUE)

single_row_maps <- getMaps(my_heart_failure_codelist,
  mappingtable = READMAPS, to = c('read2', 'ctv3'),
  single_row_per_concept = TRUE)

# Display the maps - one row per concept (long terms truncated)
print(single_row_maps[, list(term = substr(term, 1, 10), read2_code,
  ctv3_concept)])

multi_row_maps <- getMaps(my_heart_failure_codelist,
  mappingtable = READMAPS, to = 'read2',
  single_row_per_concept = FALSE)

# Display the maps - multiple rows per concept (long terms truncated)
print(multi_row_maps[, list(term = substr(term, 1, 10), read2_code,
  read2_term = substr(read2_term, 1, 30))])
  
# Create a standalone Read2 codelist
read_codelist <- data.table::data.table(
  code = unlist(single_row_maps$read2_code),
  term = unlist(single_row_maps$read2_term))
print(read_codelist[!duplicated(read_codelist)][order(code)])

