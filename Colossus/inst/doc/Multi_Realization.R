## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)


## -----------------------------------------------------------------------------
names <- c('a','r','s')
term_n <- c(0,0,0)
tform <- c("loglin","loglin","loglin")
modelform <- "M"
fir <- 0

a_n <- c(0.1, 0.1, 0.1)

## ----eval=FALSE---------------------------------------------------------------
#  dose_index <- c('r', 's') #the two columns in the model to replace are the radiation and sleeping covariates
#  dose_realizations <- matrix(c('r0','r1','r2','r3','r4','s0','s1','s2','s3','s4'), nrow=2) #columns to be used for realizations 0-4, rows for each column being replaced

