## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)

## ----provenance---------------------------------------------------------------
provenance(iris_dataset)

## ----update-provenance--------------------------------------------------------
provenance(iris_dataset) <- list( 
  wasInformedBy = "http://id.loc.gov/authorities/subjects/sh2002004407"
  )

## ----review-provenance, warning=FALSE-----------------------------------------
provenance(iris_dataset)$wasInformedBy

## ----provenance-triples-------------------------------------------------------
provenance_df <- as.data.frame(
  lapply(provenance(iris_dataset), function(x) x[[1]])
  )
provenance_df            <- id_to_column(provenance_df, "eg:dataset-1")
provenance_df$started_at <- xsd_convert(provenance_df$started_at)
provenance_df$ended_at   <- xsd_convert(provenance_df$ended_at)

dataset_to_triples(provenance_df, idcol="rowid")

