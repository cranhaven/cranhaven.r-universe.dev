## -----------------------------------------------------------------------------
library(Rdiagnosislist)
require(data.table)

# Use one thread only for CRAN
data.table::setDTthreads(threads = 1)

SNOMED <- sampleSNOMED()

# Create a codelist for right heart failure
rhf <- SNOMEDcodelist('Right heart failure', include_desc = TRUE)

addInactiveConcepts(rhf)

## -----------------------------------------------------------------------------
oldReadCodelist <- fread('
readcode|readterm
G54z500|Valvular heart disease"                     
G5yyC00|Diastolic dysfunction"                      
G5y3100|Ventricular dilatation"
')

# Get sample dictionaries
data(READMAPS)
SNOMED <- sampleSNOMED()

# Create a mapping table for Read to SNOMED (from NHSD table in READMAPS)
NHSD_READ_TO_SNOMED <- READMAPS[, list(readcode = unlist(read2_code)),
	by = conceptId]
mapped <- SNOMEDcodelist(NHSD_READ_TO_SNOMED[oldReadCodelist, on = 'readcode']$conceptId,
	codelist_name = 'heart_dis_from_read', version = 0, author = 'Me', date = 'Feb 2022')

# Export to HTML
# htmlCodelistHierarchy(mapped, file = paste0(tempdir(), '/test.html'))

# Command to open file in web browser (Linux only)
# system(paste0('google-chrome ', tempdir(), '/test.html &'))

# Reimport edited list
importedlist <- fread('
conceptId,term,include_desc,included,checked,comment
368009,"Heart valve disorder (disorder)",FALSE,TRUE,FALSE,""
3545003,"Diastolic dysfunction (finding)",FALSE,TRUE,FALSE,""
6210001,"Dilatation of cardiac ventricle (disorder)",FALSE,TRUE,FALSE,""
418304008,"Diastolic heart failure (disorder)",FALSE,FALSE,FALSE,""
441530006,"Chronic diastolic heart failure (disorder)",FALSE,FALSE,FALSE,""
443343001,"Acute diastolic heart failure (disorder)",FALSE,FALSE,FALSE,""
443344007,"Acute on chronic diastolic heart failure (disorder)",FALSE,FALSE,FALSE,""
153931000119109,"Acute combined systolic and diastolic heart failure (disorder)",FALSE,FALSE,FALSE,""
153951000119103,"Acute on chronic combined systolic and diastolic heart failure (disorder)",FALSE,FALSE,FALSE,""
153941000119100,"Chronic combined systolic and diastolic heart failure (disorder)",FALSE,FALSE,FALSE,""
120891000119109,"Diastolic heart failure stage C (disorder)",FALSE,FALSE,FALSE,""
120881000119106,"Diastolic heart failure stage D (disorder)",FALSE,FALSE,FALSE,""
')

# Convert to parsimonious (tree format) codelist
treecodelist <- as.SNOMEDcodelist(importedlist, format = 'tree',
	codelist_name = 'heart_dis_from_read', version = 1, author = 'Me', date = 'Feb 2022')

