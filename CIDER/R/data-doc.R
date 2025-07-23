#' Pancreas Metadata
#'
#' Cell-level metadata for cross-species pancreatic data.
#'
#' @description This dataset provides cell-level metadata for the human and mouse 
#' pancreatic data used in the study.
#'
#' @format A data frame with 10127 rows and 3 columns:
#' \describe{
#'   \item{Batch}{Species information (human or mouse).}
#'   \item{Group}{Cell type annotation.}
#'   \item{Sample}{Donor information.}
#' }
#'
#' @usage data(pancreas_meta)
#'
#' @source The metadata were downloaded alongside the count matrix from NCBI GEO accession GSE84133.  
#' Reference: Baron M, Veres A, Wolock SL, Faust AL, Gaujoux R, Vetere A, et al. 
#' A single-cell transcriptomic map of the human and mouse pancreas reveals inter- and intra-cell 
#' population structure. Cell Syst. 2016;3:346–360.e4.
#'
"pancreas_meta"

