## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
  row.print = 25,
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ssdGSA)
library(GSVA)

## -----------------------------------------------------------------------------
Data_matrix <- ssdGSA::data_matrix_entrezID
knitr::kable(head(round(Data_matrix, 3)))

## -----------------------------------------------------------------------------
knitr::kable(head(round(ssdGSA::data_matrix, 3)))

## ----message=FALSE, warning=FALSE---------------------------------------------
Data_matrix <- transform_ensembl_2_entrez(ssdGSA::data_matrix)
knitr::kable(head(round(Data_matrix, 3)))

## ----echo=FALSE---------------------------------------------------------------
Gene_sets <- ssdGSA::gene_sets[c(1,2,4)]
head(Gene_sets)

## ----echo=FALSE---------------------------------------------------------------
Direction_matrix <- ssdGSA::direction_matrix
knitr::kable(head(round(Direction_matrix, 3)))

## -----------------------------------------------------------------------------
ssdGSA(Data = Data_matrix,
       Gene_sets = Gene_sets,
       Direction_matrix = Direction_matrix, 
       GSA_weight = "group_weighted",
       GSA_weighted_by = "sum.ES", #options are: "sum.ES", "avg.ES", "median.ES"
       GSA_method = "gsva", #"options are: "gsva", "ssgsea", "zscore", "avg.exprs", and "median.exprs"
       min.sz = 1,  # GSVA parameter
       max.sz = 2000, # GSVA parameter
       mx.diff = TRUE # GSVA parameter
)

## -----------------------------------------------------------------------------
ssdGSA(Data = Data_matrix,
       Gene_sets = Gene_sets,
       Direction_matrix = NULL, 
       GSA_weight = "group_weighted",
       GSA_weighted_by = "sum.ES", #options are: "sum.ES", "avg.ES", "median.ES"
       GSA_method = "gsva", #"options are: "gsva", "ssgsea", "zscore", "avg.exprs", and "median.exprs"
       min.sz = 6,  # GSVA parameter
       max.sz = 2000, # GSVA parameter
       mx.diff = TRUE # GSVA parameter
)

## -----------------------------------------------------------------------------
ssdGSA_individual(Data = Data_matrix,
                  Gene_sets = Gene_sets,
                  Direction_matrix = Direction_matrix
)

