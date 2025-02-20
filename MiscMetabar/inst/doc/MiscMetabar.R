## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE
)

## ----example------------------------------------------------------------------
library("MiscMetabar")
library("phyloseq")
data("data_fungi")
summary_plot_pq(data_fungi)

## ----results="asis"-----------------------------------------------------------
data("GlobalPatterns", package = "phyloseq")
tax_datatable(subset_taxa(
  GlobalPatterns,
  rowSums(GlobalPatterns@otu_table) > 100000
))

## -----------------------------------------------------------------------------
gp <- subset_taxa(GlobalPatterns, GlobalPatterns@tax_table[, 1] == "Archaea")
sankey_pq(gp, taxa = c(1:5))

## -----------------------------------------------------------------------------
upset_pq(gp, "SampleType", taxa = "Class")

## -----------------------------------------------------------------------------
sessionInfo()

