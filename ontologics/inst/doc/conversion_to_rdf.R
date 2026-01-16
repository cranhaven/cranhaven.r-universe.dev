## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)

## ----setup--------------------------------------------------------------------
library(ontologics)
library(tibble)
library(dplyr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
ontoDir <- system.file("extdata", "crops.rds", package = "ontologics")
onto <- load_ontology(path = ontoDir)

export_as_rdf(ontology = onto, filename = "onto.ttl")

