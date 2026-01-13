library(amberr)

# Start Amber session
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")

#
# Case report forms
# are based on a specific or the last revision of a study's form
#
# Find all CRFs
amber.case_report_forms(a)
# Find CRFs based on a form, whatever the revision, by the form name
amber.case_report_forms(a, form = "Adult trauma")
# Find CRFs in a study, by the study name
amber.case_report_forms(a, study = "Trauma Registry")
# Find CRFs with a specific form revision (result can be NULL if there are none)
amber.case_report_forms(a, form = "maps", revision = 1)
# Find CRFs based on the latest form revision
amber.case_report_forms(a, form = "image", revision = NULL)
# Find a CRF by name
amber.case_report_form(a, id = "Adult trauma - baseline")

#
# Case report records (raw)
# are collected in the context of a case report form (a study's form revision)
# and contain some technical information (state, audit of actions)
# along with the raw data
#
# Find all case reports
amber.case_reports(a)
# Find all case reports of a CRF
amber.case_reports(a, caseReportForm = "Pediatric trauma - followup")

#
# Case report records (export)
# are case report exports: data in tabular format along with the data dictionary
# (based on the form's metadata)
#
# Find all case reports
amber.case_report_export(a)
# Find all case reports in a range of time
amber.case_report_export(a, from = "2022-01-12 00:00", to = "2023-12-31")
# Find all case reports for a specific participant/patient identifier
amber.case_report_export(a, pId = "1231")
# Find all case reports having their identifier matching a regular expression
amber.case_report_export(a, query = list(`data._id[$search]` = "^12"))
# Find all case reports which form data is matching some value (will not work if the data are encrypted in the database)
amber.case_report_export(a, query = list(data.PATIENT.ORIGIN_REGION = "xyz"))
# Export records collected with a study's form in a specific version
amber.case_report_export(a, study = "Trauma Registry", form = "Adult trauma", query = list(revision = 6))
# Export records collected with a study's form in all versions used
amber.case_report_export(a, caseReportForm = "Adult trauma - baseline")
# Export records collected with a study's form in all versions used
tables <- amber.case_report_export(a, study = "Trauma Registry")
# Result contains both data and dictionary
tables
# Tables are named with the <case report form name>-<revision> pattern
names(tables)
# Merge datasets from different versions if relevant
dplyr::bind_rows(lapply(tables, function (t) {
  t$data
}))

# End Amber session
amber.logout(a)

#
# Save case report records in Opal
#
library(opalr)

# Start Opal session
o <- opal.login(username = "administrator", password = "password", url = "https://opal-demo.obiba.org")
#o <- opal.login(username = "administrator", password = "password", url = "http://localhost:8080")

tableName <- "Adult trauma - baseline-13"

# Work on a specific table
table <- tables[[tableName]]

# Decorate the data with the dictionary
data <- dictionary.apply(table$data, variables = table$dictionary$variables, categories = table$dictionary$categories)
data$MECHANISM.INJURY_CAUSE
attributes(data$MECHANISM.INJURY_CAUSE)

# Get the entity type from the dictionary (first variable)
entityType <- table$dictionary$variables[1,]$entityType

if (!opal.project_exists(o, "amber")) {
  opal.project_create(o, project = "amber", database = TRUE)
}
# Save table in Opal
opal.table_save(o, data, "amber", tableName, id.name = "_id", type = entityType, overwrite = TRUE, force = TRUE)

# [optional] Update dictionary in Opal
opal.table_dictionary_update(o, "amber", tableName, variables = table$dictionary$variabl, categories = table$dictionary$categories)

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
data_sav <- dplyr::rename(data, pid = "id", id = "_id")
# save in SPSS format
haven::write_sav(data_sav, path = sav)
# read back
tbl_sav <- haven::read_sav(sav)
tbl_sav
attributes(tbl_sav$TRANSFER.FROM)
attributes(data_sav$TRANSFER.FROM)

# SAS
sas <- tempfile(fileext = ".sas")
# rename column starting with underscore char
data_sas <- dplyr::rename(data, pid = "id", id = "_id")
# rename columns with dot char
data_sas <- dplyr::rename_with(data_sas, function(col) {
  gsub("\\.", "_", col)
})
# save in SAS format
haven::write_xpt(data_sas, path = sas)
# read back
tbl_sas <- haven::read_xpt(sas)
tbl_sas
attributes(tbl_sas$TRANSFER_FROM)
attributes(data_sas$TRANSFER_FROM)
