## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Import the package 
  library(kmeRs)

## -----------------------------------------------------------------------------
# Simple BLOSUM62 similarity matrix for all amino acid nucleotides
  BLOSUM62 <- kmeRs_similarity_matrix(submat = "BLOSUM62")
# Fancy knitr table
  knitr::kable(BLOSUM62)

## -----------------------------------------------------------------------------
# Given hexamers
  kmers_given <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
# Matrix calculation 
  kmers_mat <- kmeRs_similarity_matrix(q = c("GATTACA"), x = kmers_given , submat = "BLOSUM62") 
# Fancy knitr table
  knitr::kable(kmers_mat) 

## -----------------------------------------------------------------------------
# Score and sort the matrix  
  kmers_res <- kmeRs_score(kmers_mat)
# Fancy knitr table
  knitr::kable(kmers_res)

## -----------------------------------------------------------------------------
# Given hexamers
  kmers_given <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
# Matrix calculation 
  kmers_mat <- kmeRs_similarity_matrix(q = kmers_given, submat = "BLOSUM62")
# Score the matrix and sort by decreasing score 
  kmers_res <- kmeRs_score(kmers_mat)
# Fancy knitr table
  knitr::kable(kmers_res)
  

## -----------------------------------------------------------------------------
# Calculate stats 
  kmers_stats <- kmeRs_statistics(kmers_res)
# Fancy knitr table
  knitr::kable(kmers_stats[ ,1:(dim(kmers_stats)[2] - 4) ])

## -----------------------------------------------------------------------------
# Heatmap without sum column
  kmeRs_heatmap(kmers_res[, -8])  

