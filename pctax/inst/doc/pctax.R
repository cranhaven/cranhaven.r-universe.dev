## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE, cache = TRUE,
    comment = "#>",
    dpi = 72
)

## ----setup,include=FALSE------------------------------------------------------
library(pctax)
library(pcutils)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("Asa12138/pcutils", dependencies = T)
#  
#  devtools::install_github("Asa12138/pctax", dependencies = T)

## ----lib----------------------------------------------------------------------
library(pctax)
library(pcutils)
data(otutab, package = "pcutils")
# help(otutab)

head(otutab)
head(metadata)
head(taxonomy)

