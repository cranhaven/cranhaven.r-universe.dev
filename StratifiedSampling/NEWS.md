# NEWS for Package "StratifiedSampling"


## StratifiedSampling 0.4.1
* (ERROR fixed, the cumsum/floor function does not have the same result on mac. This is what was producing the error.)

## StratifiedSampling 0.4.0
* add Deville systematic functions (sys_deville, sys_devillepi2)
* add EPS in stratifedcube and landingRM
* add full option in osod
* add cps C++ implementation of the UPmaxentropy function from sampling package.
* add balseq stream sampling
  - add distunitk in src folder.
  - add Rglpk as depends in DESCRIPTION

## StratifiedSampling 0.3.0
* add One step One decision sampling method (osod, c_bound, inclprob)
* remove test_that

## StratitfiedSampling 0.2.0
* fix a findB when the number of columns is greater than the number of rows. 
* add calibRaking, harmonize, otmatch and bsmatch (a proposed method to do statistical matching).
* add vignettes to explain how to use statistical matching.
* change README to add information about statistical matching.
* add arXiv link whether it is necessary.
  
## StratifiedSampling 0.1.0

### CRAN resubmission
* Please do not modify the user's global environment or the user's home filespace in your examples or vignettes by deleting objects:
rm(list = ls())
- remove rm(list = ls()) from R environment of cpp files. 


### CRAN resubmission
* moved https://arxiv.org/abs/arXiv:2101.05568 to https://arxiv.org/abs/2101.05568


### CRAN resubmission
* add \value to Rd file landingRM
* remove all rm(list = ls())
* remove all call to sampling:: and put sampling in suggest

### CRAN submission