# TestGenerator 0.3.3

* `generateTestTable()` creates an Excel file with sheets that correspond to OMOP-CDM tables.

* `patientsCDM()` now accepts `cdmName` as argument to allow for custom cdm name.

* `getEmptyCDM()` returns an empty cdm object.

# TestGenerator 0.3.2

* Fixed bug related to empty tables pushed to the duckdb CDM.

# TestGenerator 0.3.1

* `readPatients()` now has a parameter to select either Excel or CSV files as an input. 

* `readPatients.xl()` and `readPatients.csv` are also exported functions for convenience.

* `graphCohort()` provides a visualisation of cohort timelines.

* JSONS are saved in the testthat/testCases folder as default for better test self-containment.

* Tested with MIMIC database.

# TestGenerator 0.2.5

* Using omopgenerics for checking Excel data.

# TestGenerator 0.2.4

* Updated DESCRIPTION.

# TestGenerator 0.2.3

* Updated examples.

# TestGenerator 0.2.2

* Updated messages in functions.

# TestGenerator 0.2.1

* Updated documentation and vignette explaining sample data.

# TestGenerator 0.2.0

* Initial CRAN submission.

# TestGenerator 0.1.0

* Tests passed and finishes documentation.
