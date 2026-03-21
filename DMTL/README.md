## `DMTL`

[![Build Status](https://www.travis-ci.com/dhruba018/DMTL.svg?branch=main)](https://www.travis-ci.com/dhruba018/DMTL)

DMTL is an R package for applying distribution mapping based transfer learning. DMTL employs the widely renowned concept of histogram matching and extend it to include distribution estimates like kernel density estimates. The typical use case would be if somebody wants to utilize data from multiple sources for similar kind of experiments in statistical modeling but there exists significant distribution shift between both predictors and response values. In this case, DMTL can alleviate this shift by generating a distribution matching based map and transfer the target data to the source domain to utilize the available source data for modeling using various predictive modeling techniques.  

#### Note: The package is currently waiting evaluation from the CRAN submission team. 
In the meanwhile- if you want to install it on your local machine, you will need the `devtools` package which is [available](https://CRAN.R-project.org/package=devtools) in CRAN. You can install it using the following command -  
		
		install.packages("devtools")

Once you have it, you need to use the following code chunk -  
		
		library(devtools)  
		install_github("dhruba018/DMTL")  


