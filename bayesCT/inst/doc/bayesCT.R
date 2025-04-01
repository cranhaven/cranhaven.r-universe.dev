## ----setup, echo=FALSE, results="hide"----------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  collapse = TRUE,
  comment = "#>"
)
suppressWarnings(RNGversion("3.5.0"))
set.seed(43232)

## ---- eval = FALSE, echo=TRUE-------------------------------------------------
#  install.packages("bayesCT")

## ---- eval = FALSE, echo=TRUE-------------------------------------------------
#  devtools::install_github("thevaachandereng/bayesCT@vx.xx.x")
#  
#  # or
#  
#  devtools::install_version("bayesCT", version = "x.x.x", repos = "http://cran.us.r-project.org")

## ---- cache=FALSE, warning=FALSE, comment=FALSE, eval=FALSE, echo=TRUE--------
#  devtools::install_github("thevaachandereng/bayesCT")

## ----lib, results="asis", eval=TRUE, echo=TRUE--------------------------------
library(bayesCT)

## ---- echo=TRUE---------------------------------------------------------------
enrollment(param = c(0.3, 0.7, 0.9, 1.2), N_total = 50, time = c(5, 10, 15))

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  randomization(N_total = 140, block = 7, allocation = c(2, 1))

## ---- eval = TRUE, echo = TRUE------------------------------------------------
randomization(N_total = 140, block = 6, allocation = c(2, 1))

## ---- eval = TRUE, echo = TRUE------------------------------------------------
randomization(N_total = 120, block = 120, allocation = c(2, 1))

