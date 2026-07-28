## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  
#  library(devtools)
#  install_github("EnquistLab/RNSR/NSR")
#  
#  

## -----------------------------------------------------------------------------
library(NSR)
NSR_simple(species = "Acer rubrum",
           country =  "Canada",
           state_province = "Ontario")



## -----------------------------------------------------------------------------

head(NSR_template())


## -----------------------------------------------------------------------------

#First, pull the example data that are included with the package
data("nsr_testfile")
head(nsr_testfile)

example_results <- NSR(nsr_testfile)

head(example_results)


## -----------------------------------------------------------------------------

sources <- NSR_metadata()

checklist_per_political_divisions <- NSR_political_divisions()

# If you'd rather see which political divisions are available within each checklist, set "by_country" to FALSE

political_divisions_per_checklist <- NSR_political_divisions(by_country = FALSE)



