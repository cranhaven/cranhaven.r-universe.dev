## devRate 0.2.5
* new functions for Helicoverpa armigera phenology modelling
* fix devRateIBMparam function for negative values when stocha parameter is too high

## devRate 0.2.3 and 0.2.4
* fix package documentation PKGNAME-package \alias

## devRate 0.2.2
* devRateQlStat function optimized

## devRate 0.2.1
* fix URLs

## devRate 0.2.0
* fix argument name: from df to dfData in devRateModel
* simplify functions: 
    - devRatePrint: delete T and rT arguments
    - devRatePlot: delete T and rT arguments

## devRate 0.1.12
* function to fit all equations
* function to assess goodness-of-fit
* function for model comparison

## devRate 0.1.11
* coverage with testthat
* typos and minor changes
* new function devRateIBMparam

## devRate 0.1.10
* vignette problem fixed with knitr version 1.23

## devRate 0.1.9
* stage-specific stochasticity in individual-based models (devRateIBM function)

## devRate 0.1.8
* fixed typos in main_spanish vignette 
* update in Description file with papers and DOI

## devRate 0.1.7
* vignette in Spanish
* small adjustments in graphic displays
* BDD update : 37 models ; 2257 entries ; 799 species

## devRate 0.1.6
* fix bug in devRatePlotInfo for eq. with NA parameter values
* fix bug in devRatePlotInfo for eq. briere2_99
* README and devRate package info updated

## devRate 0.1.5
* main vignette improved
* new sec01 vignette for model evaluation
* CITATION file
* BDD update: 37 models ; 2249 entries ; 799 species

## devRate 0.1.4
* fixed typos and misspellings in the documentation 
* vignette improved and output is generated automatically
* devRateFind function returns number of parameters and enforce single value

## devRate 0.1.3
* typos and grammatical errors in manual fixed
* example from the vignette included in the package
* vignette with more information and interpretation
* devRateFind function now returns a data.frame
* new function to model phenology from the package database
* BDD modif: refs with new column entries
* BDD update: 35 models and 619 entries ; 138 species

## devRate 0.1.2
* equations in latex in PDF manual using \eqn{latex}{ascii}
* fixed some typos in manual
* linear models with auto-starting values

## devRate 0.1.1
* vignette improved
* help page for function devRateMap improved
* new devRatePrint function (custom output for nls object)
* bug in phenological model corrected
* Sharpe and DeMichele equation now in Celsius: transformed into Kelvins within the equation (cf. Sharpe and DeMichele 1977)
* Structural equations of each regression model in the package manual
* testing units using testthat package
* BDD update: 35 models ; 543 entries ; 131 species

## devRate 0.1.0
* Initial submission
