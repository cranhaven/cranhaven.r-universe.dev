# rsurvstat 0.1.4

* change imported function for R CMD check from sf due to reverse dependency.

# rsurvstat 0.1.3

* Change resolution of `CountyKey71Map` to address CRAN old release Mac specific
  issue.

# rsurvstat 0.1.2

* Bug fixes
* CRAN Submission changes.
* Removed internal function `.tree()` from exports

# rsurvstat 0.1.1

* Initial CRAN candidate version with the following functionality
  + Download data from Robert Koch Institute `SurvStat` service 
  + Paging and controllable caching of downloads
  + Weekly timeseries or snapshots of data from a single disease season
  + Stratification and filtering by disease (121 options), age group (8 options), geography 
    (3+1 options)
  + Stratification by disease subtype given parent disease
  + Time varying population denominator inference based on geography and age 
    group stratification.
  + Linked map data included.
