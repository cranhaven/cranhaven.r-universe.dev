## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(scAnnotate)

## -----------------------------------------------------------------------------
data(pbmc1)
data(pbmc2)

## ----eval=FALSE---------------------------------------------------------------
#  ?pbmc1
#  ?pbmc2

## ----eval=FALSE---------------------------------------------------------------
#  predict_label=scAnnotate(train=pbmc1,
#                           test=pbmc2[,-1],
#                           distribution="normal",
#                           correction ="auto",
#                           screening = "wilcox",
#                           threshold=0,
#                           lognormalized=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  eva_cal(prediction = predict_label,cell_label = pbmc2[,1])

