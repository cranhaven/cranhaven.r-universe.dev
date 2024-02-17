## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)

## ----dublincore---------------------------------------------------------------
print(as_dublincore(iris_dataset), 'Bibtex')

## ----datacite-----------------------------------------------------------------
print(as_datacite(iris_dataset), 'Bibtex')

## ----provenance---------------------------------------------------------------
provenance(iris_dataset)

## ----structural-metadata------------------------------------------------------
## Only the first variable is printed:
DataStructure(iris_dataset)[[1]]

