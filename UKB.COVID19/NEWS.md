# UKB.COVID19 0.1.0

## Major changes

- Initial release on CRAN

# UKB.COVID19 0.1.1

## Bug fixes

- Fixed a check problem: writing files in the user’s home filespace

# UKB.COVID19 0.1.2

## Minor changes

- Improved the test files in test/testhtat/.
- Changed a few function names: `risk.factor()` to `risk_factor()`, `comorbidity.asso()` to `comorbidity_asso()`, `comorbidity.summary()` to `comorbidity_summary()`, `data.reform()` to `data_reform()`.
- Removed the argument for output file name in `risk_factor()`, `comorbidity_asso()`, `comorbidity_summary()`, and `makePhentypes()`. The functions only return objects but not output files. Users can choose if they want to wrtie the outputs into files and can custmise the output files by themselves.
- Added in `risk_factor()` cheching ukb.tab file and return error messages if the file doesn't include the fields needed for the analyses.
- Defined returned objects under S3 class.

# UKB.COVID19 0.1.3

## Minor changes

- Added GWAS scripts under inst/GWAS/.

# UKB.COVID19 0.1.4

## Minor changes

- Reduce the test timing to less than 5 min.

# UKB.COVID19 0.1.5

## Minor changes

- Added Vignettes.

# UKB.COVID19 0.1.6

## Minor changes

- Fixed Vignette codes which writes to covid_example("results").