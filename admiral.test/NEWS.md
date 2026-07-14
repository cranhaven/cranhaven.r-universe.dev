# admiral.test 0.7.0

 - New messaging on attachment of package to indicate the archival of package in favor of `pharmaversesdtm` (#145)

# admiral.test 0.6.0

 - Updated `get_terms()` function to handle variable renames in query data set. Renamed
variables are as follows:
  
     - `VAR_PREFIX` to `PREFIX`
     - `QUERY_NAME` to `GRPNAME`
     - `QUERY_ID` to `GRPID`
     - `QUERY_SCOPE` to `SCOPE`
     - `QUERY_SCOPE_NUM` to `SCOPEN`
     - `TERM_LEVEL` to `SRCVAR`
     - `TERM_NAME` to `TERMNAME`
     - `TERM_ID` to `TERMID`
  
  - Maintainer of package switched from Thomas Neitmann to Ben Straub (#139)

# admiral.test 0.5.0
 - Updated AE to add variable AELAT for ophthalmology package (#99)
 - Updated TR, TU, RS for admiralonco package (#103):
     - tumor location `"LYMPH NODE"` added
     - TR test codes `"LDIAM"` and `"LPERP"` added
     - unscheduled visits added
     - incomplete assessments of target lesions added
 - Updated QS to add questionnaire for ophthalmology package (#120)
 - Created SDTM SC dataset for ophthalmology package (#102)
 - Created SDTM OE dataset for ophthalmology package (#101)
 - Using {metatools} to add labels to datasets (#87)
 - `admiral.test` has SDTM data for [oncology](https://pharmaverse.github.io/admiralonco/main/index.html)
 and [ophthalmology](https://pharmaverse.github.io/admiralophtha/main/reference/index.html)
 - Removed `library(admiral.test)` from all programs in the `./dev` directory.
 
# admiral.test 0.4.0
 - `get_smq_select()` and `get_sdq_select()` have been deprecated in favor of `get_terms()` to accommodate changes in {admiral} (#94)
 - Updated labels for PP and PC datasets (#77)
 - Various updates to improve CI/CD workflows

# admiral.test 0.3.0
 - Updated README for instructions on installation, how to add and update to data (#53)
 - Adding new data to MH (#51)
 - Updates to percent differentials in LB (#52)
 - Implemented admiral CI/CD workflows (#50)

# admiral.test 0.2.0

- Renaming all datasets to `admiral_` (#30) 
- Extending the test data offering by adding PK datasets - PC and PP (#1) 
- Adding Oncology test data - RS, TU, TR, SUPPTR (#2)
- Adding SV from CDISC pilot project (#20)

# admiral.test 0.1.2

- CRAN release version with all comments resolved (#18)

# admiral.test 0.1.1

- Addressing CRAN comments (#16)

# admiral.test 0.1.0

- Test data for the {admiral} package taken from the CDISC pilot project - mainly safety data.
