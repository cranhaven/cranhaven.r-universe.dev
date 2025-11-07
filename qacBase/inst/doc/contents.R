## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(qacBase)
library(knitr)

## ----message=FALSE, warning=FALSE---------------------------------------------
data(cars74)

## ----message=FALSE, warning=FALSE---------------------------------------------
contents(cars74)


## ----message=FALSE, warning=FALSE---------------------------------------------
df_plot(cars74)


## ----message=FALSE, warning=FALSE---------------------------------------------
barcharts(cars74)


## ----message=FALSE, warning=FALSE---------------------------------------------
histograms(cars74)

densities(cars74)


