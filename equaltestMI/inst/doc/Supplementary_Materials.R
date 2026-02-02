## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
## load package
# install.packages("equaltestMI")
library(equaltestMI)

## ----setup2, message=FALSE, warning=FALSE-------------------------------------
# install.packages("devtools")
# library(devtools)
# devtools::install_github("gabriellajg/equaltestMI", force=TRUE)
library(equaltestMI)

## -----------------------------------------------------------------------------
data(LeeAlOtaiba)
# contains sample covariance matrices and sample means of four groups

## -----------------------------------------------------------------------------
## group 1 = boys ineligible for free-reduced lunches
Group1 <- LeeAlOtaiba$BoysIneligible
Group1 <- as.matrix(Group1)

## group 2 = boys eligible for free-reduced lunches
Group2 <- LeeAlOtaiba$BoysEligible
Group2 <- as.matrix(Group2)

# sample means:
M1 <- Group1[1,]
M2 <- Group2[1,]

# sample covariance matrices:
Cov1 <- Group1[2:7,]
Cov2 <- Group2[2:7,]

## ---- echo=FALSE--------------------------------------------------------------
M1

## ---- echo=FALSE--------------------------------------------------------------
round(Cov1, 3)

## ---- echo=FALSE--------------------------------------------------------------
M2

## ---- echo=FALSE--------------------------------------------------------------
round(Cov2, 3)

## -----------------------------------------------------------------------------
## lavaan model syntax
model <- '
AlphabetKnowledge =~ Letter_Name+ Letter_Sound
PhonologicalAwareness =~ Blending + Elision
Spelling =~ Real_Words + Pseudo_Words
'

## ---- eval=FALSE--------------------------------------------------------------
#  ## the results using equivalence testing and projection method
#  ## full R output will be presented in Part 3
#  test <- eqMI.main(model = model,
#  	sample.nobs = c(78, 174),
#  	sample.mean = list(M1, M2),
#  	sample.cov = list(Cov1, Cov2),
#  	meanstructure = TRUE,
#  	output = 'both',
#  	quiet = TRUE, 	
#  	equivalence.test = TRUE, adjRMSEA = TRUE,
#  	projection = TRUE, bootstrap = FALSE)

## ---- echo=FALSE--------------------------------------------------------------
## the results using equivalence testing and projection method
test <- eqMI.main(model = model, 
	sample.nobs = c(78, 174), 
	sample.mean = list(M1, M2), 
	sample.cov = list(Cov1, Cov2),
	meanstructure = TRUE, 
	output = 'both', 
	quiet = FALSE, 	
	equivalence.test = TRUE, adjRMSEA = TRUE, 
	projection = TRUE, bootstrap = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  test1 <- eqMI.main(model = model,
#  	sample.nobs = c(78, 174), sample.cov = list(Cov1, Cov2),
#  	sample.mean = list(M1, M2), meanstructure = TRUE,
#  	equivalence.test = FALSE, adjRMSEA = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  test2 <- eqMI.main(model = model,
#  	sample.nobs = c(78, 174), sample.cov = list(Cov1, Cov2),
#  	sample.mean = list(M1, M2), meanstructure = TRUE,
#  	equivalence.test = FALSE, adjRMSEA = FALSE,
#  	projection = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  test3 <- eqMI.main(model = model,
#  	sample.nobs = c(78, 174), sample.cov = list(Cov1, Cov2),
#  	sample.mean = list(M1, M2), meanstructure = TRUE,
#  	equivalence.test = TRUE, adjRMSEA = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  test4 <- eqMI.main(model = model,
#  	sample.nobs = c(78, 174), sample.cov = list(Cov1, Cov2),
#  	sample.mean = list(M1, M2), meanstructure = TRUE,
#  	equivalence.test = TRUE, adjRMSEA = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  test5 <- eqMI.main(model = model,
#  	sample.nobs = c(78, 174), sample.cov = list(Cov1, Cov2),
#  	sample.mean = list(M1, M2), meanstructure = TRUE,
#  	equivalence.test = TRUE, adjRMSEA = TRUE,
#  	projection = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  test6 <- eqMI.main(model = model, structure = 'mean',
#  	sample.nobs = c(78, 174), sample.cov = list(Cov1, Cov2),
#  	sample.mean = list(M1, M2), meanstructure = TRUE,
#  	equivalence.test = TRUE, adjRMSEA = TRUE,
#  	projection = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  test7 <- eqMI.main(model = model, data = literacy.dat,
#  	group = "FRL", meanstructure = TRUE,
#  	equivalence.test = TRUE, adjRMSEA = TRUE,
#  	projection = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  test8 <- eqMI.main(model = model, data = literacy.dat,
#  	group = "FRL", meanstructure = TRUE,
#  	equivalence.test = TRUE, adjRMSEA = TRUE,
#  	projection = TRUE, bootstrap = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  test9 <- eqMI.main(model = model, data = literacy.dat,
#  	group = "FRL", meanstructure = TRUE,
#  	equivalence.test = TRUE, adjRMSEA = TRUE,
#  	projection = TRUE, bootstrap = FALSE,
#  	quite = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  test10 <- eqMI.main(model = model, data = literacy.dat,
#  	group = "FRL", meanstructure = TRUE,
#  	group.partial = c("Spelling=~Real_Words", "Blending~1"),
#  	equivalence.test = TRUE, adjRMSEA = TRUE,
#  	projection = TRUE)

## ---- echo=FALSE--------------------------------------------------------------
library(printr) 
?eqMI.main

## ---- include=FALSE-----------------------------------------------------------

# For a complete view of the help page of function eqMI.main(), please install R package printr and type ?eqMI.main in R console:

#library(printr) 
#?eqMI.main

#output: pdf_document
#devtools::build_vignettes()
#R CMD Rd2pdf "~/Box Sync/MacSync/Research/Frontier/equaltestMI"
#R CMD check --as-cran equaltestMI_0.6.0.tar.gz
#https://bookdown.org/yihui/rmarkdown-cookbook/package-vignette.html

