## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  eval=rmarkdown::pandoc_available("1.12.3")
)

library(MBNMAtime)
library(rmarkdown)
library(knitr)
library(dplyr)
#load(system.file("extdata", "vignettedata.rda", package="MBNMAtime", mustWork = TRUE))

## ---- echo=FALSE--------------------------------------------------------------
kable(head(osteopain), digits=2) 

## ---- echo=FALSE--------------------------------------------------------------
kable(head(alog_pcfb), digits=2) 

## ---- echo=FALSE--------------------------------------------------------------
kable(head(copd), digits=2) 

## ---- echo=FALSE--------------------------------------------------------------
kable(head(obesityBW_CFB), digits=2) 

## ---- echo=FALSE--------------------------------------------------------------
kable(head(goutSUA_CFB), digits=2) 

