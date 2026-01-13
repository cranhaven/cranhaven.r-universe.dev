library(amberr)

# Start Amber session
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")

#
# Interview designs
#
# Find all interview designs
amber.interview_designs(a)
# Find interview designs in a study, by the study name
amber.interview_designs(a, study = "liftup")
# Find a interview design by name
amber.interview_design(a, id = "treocapa_lt")

amber.campaigns(a)
amber.campaigns(a, study = "liftup")
amber.campaign(a, id = "base")

# Find all participants
amber.participants(a)
# Find participants that are not eligible
amber.participants(a, valid = FALSE)
# Find participants from specific study or campaign
amber.participants(a, study = "liftup")
amber.participants(a, campaign = "base")
amber.participant(a, "9WVV87")

#
# Interviews (raw)
# are collected in the context of one or more study form
# and contain some technical information (state, audit of actions)
# along with the raw data
#
# Find all interviews
amber.interviews(a)
# Find all completed interviews
amber.interviews(a, state = "completed")
# Find all interviews in progress and which participant is not valid any more
amber.interviews(a, state = "in_progress", participantValid = FALSE)
# Find all interviews of an interview designs
amber.interviews(a, interviewDesign = "treocapa_lt")

#
# Interview records (export)
# are interview exports: data in tabular format along with the data dictionary
# (based on the form's metadata)
#
# Export data from all interviews
amber.interview_export(a)
# Export data from completed interviews
amber.interview_export(a, state = "completed")
# Export data from interviews in progress and which participant is not valid any more
amber.interview_export(a, state = "in_progress", participantValid = FALSE)
# Export data from all interviews in a range of time
amber.interview_export(a, from = "2022-01-12 00:00", to = "2023-12-31")
# Export data from all interviews for a specific participant/patient study identifier
amber.interview_export(a, identifier = "67567567")
# Export data from all interviews for a specific participant code
amber.interview_export(a, code = "OL41ET")
# Export data from all interviews having their identifier matching a regular expression
amber.interview_export(a, query = list(`identifier[$search]` = "^67"))
# Export data from records collected with a study's interview design and campaign
tables <- amber.interview_export(a, study = "liftup", interviewDesign = "treocapa_lt", campaign = "base")
# Result contains both data and dictionary
tables
# Tables are named with the <interview design name>-<form name>-<revision> pattern
names(tables)
# Merge datasets of a study interview step form in different versions if relevant
dplyr::bind_rows(lapply(tables[startsWith(names(tables), "treocapa_lt-treocapa")], function (t) {
  t$data
}))

# End Amber session
amber.logout(a)

#
# Save interview records in Opal
#
library(opalr)

# Start Opal session
o <- opal.login(username = "administrator", password = "password", url = "https://opal-demo.obiba.org")
#o <- opal.login(username = "administrator", password = "password", url = "http://localhost:8080")

tableName <- "treocapa_lt-treocapa-37"

# Work on a specific table
table <- tables[[tableName]]

# Decorate the data with the dictionary
data <- dictionary.apply(table$data, variables = table$dictionary$variables, categories = table$dictionary$categories)
data$PLAY.COPY1
attributes(data$PLAY.COPY1)

# Get the entity type from the dictionary (first variable)
entityType <- table$dictionary$variables[1,]$entityType

if (!opal.project_exists(o, "amber")) {
  opal.project_create(o, project = "amber", database = TRUE)
}
# Save table in Opal
opal.table_save(o, data, "amber", tableName, id.name = "_id", type = entityType, overwrite = TRUE, force = TRUE)

# [optional] Update dictionary in Opal
opal.table_dictionary_update(o, "amber", tableName, variables = table$dictionary$variables, categories = table$dictionary$categories)

# Get back the table from Opal
opal.table_get(o, project = "amber", tableName)

# End Opal session
opal.logout(o)

#
# Save in R file
#

rds <- tempfile(fileext = ".rds")
# save in RDS format
saveRDS(data, rds)
# read back
tbl_rds <- readRDS(rds)
tbl_rds

#
# Save in file using haven: SPSS, SAS etc.
#
# Note that using the data frame decorated with opalr::dictionary.apply (see above)
# saves the data dictionary within the file (as much as the file format can support).
#

library(haven)

# SPSS
sav <- tempfile(fileext = ".sav")
# rename column starting with underscore char
data_sav <- dplyr::rename(data, id = "_id")
# save in SPSS format
haven::write_sav(data_sav, path = sav)
# read back
tbl_sav <- haven::read_sav(sav)
tbl_sav
attributes(tbl_sav$PLAY.COPY1)
attributes(data_sav$PLAY.COPY1)

# SAS
sas <- tempfile(fileext = ".sas")
# rename column starting with underscore char
data_sas <- dplyr::rename(data, id = "_id")
# rename columns with dot char
data_sas <- dplyr::rename_with(data_sas, function(col) {
  gsub("\\.", "_", col)
})
# shorten some variable names for SAS
data_sas <- dplyr::rename_with(data_sas, function(col) {
  gsub("CARE_ADMISSIONS", "CARE_ADM", col)
})
# save in SAS format
haven::write_xpt(data_sas, path = sas)
# read back
tbl_sas <- haven::read_xpt(sas)
tbl_sas
attributes(tbl_sas$PLAY_COPY1)
attributes(data_sas$PLAY_COPY1)
