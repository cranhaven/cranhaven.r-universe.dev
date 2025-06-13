[![Build Status](https://travis-ci.org/kaushikrch/rucm.svg?branch=master)](https://travis-ci.org/kaushikrch/rucm)
# rucm
### Implementation of Unobserved Components Models (UCM) in R ###

### Description
Unobserved Components Models (introduced in Harvey, A. (1989), Forecasting, structural time series models and the Kalman filter, Cambridge New York: Cambridge University Press) decomposes a time series into components such as trend, seasonal, cycle, and the regression effects due to predictor series which captures the salient features of the series to predict its behavior.

### Comments
* This repository is a working version of R package rucm. 
* A stable version is available for download on [CRAN](http://cran.r-project.org/web/packages/rucm/index.html). To download package ```install.packages("rucm")```.
* Package vignette can be found [here](http://cran.r-project.org/web/packages/rucm/vignettes/rucm_vignettes.html).

### Work-in-progress
* Implement time-varying independent variable estimation.
* Update `predict.ucm()` to use argument `newdata` for causal forecasting.

### Issues
Issues can be reported [here](https://github.com/kaushikrch/rucm/issues).

### Package News
#### rucm v0.4 
Changes:

* Changes in S3 method of printing an UC model. Added p - values for estimates of predictor variables.
* Added a vignette "Unobserved Components Model in R".
* Submitted to CRAN on 2014-09-06.

#### rucm v0.3
* First submitted to CRAN on 2014-08-25.





