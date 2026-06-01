#' pbmc1
#'
#' A subset of human Peripheral Blood Mononuclear Cells (PBMC) scRNA-seq data that
#' was sequenced using Drop-seq platform. The Seurat(version 4.0.5) package was used for
#' normalized using the NormalizeData function with the "LogNormalize" method and
#' a scale factor of 10,000. After modeling the mean-variance relationship with the
#' FindVariableFeautre function within "vst" methods, we selected the top 2,000 highly
#' variable genes and only used this selection going forward. The dataframe of the
#' cell type label and a gene expression matrix of 598 cells in the row and 2,000
#' genes in the column.
#'
#' @docType data
#' @usage data(pbmc1, package="scAnnotate")
#' @format a data frame
#' @keywords datasets
#' @references Ding,  J.et  al.(2019).   Systematic  comparative  analysis  of  single  cellrna-sequencing methods.bioRxiv
#'
"pbmc1"

#' pbmc2
#'
#' A subset of human PBMC scRNA-seq data that was sequenced using inDrops platform.
#' The Seurat(version 4.0.5) package was used for normalized using the NormalizeData
#' function with the "LogNormalize" method and a scale factor of 10,000. After modeling
#' the mean-variance relationship with the  FindVariableFeautre function within "vst"
#' methods, we selected the top 2,000 highly variable genes and only used this selection
#' going forward. The dataframe of the cell type label and a gene expression matrix of
#' 644 cells in the row and 2,000 genes in the column.
#'
#' @docType data
#' @usage data(pbmc2, package="scAnnotate")
#' @format a data frame
#' @keywords datasets
#' @references Ding,  J.et  al.(2019).   Systematic  comparative  analysis  of  single  cellrna-sequencing methods.bioRxiv
#'
"pbmc2"

#' predict_label
#'
#' Cell type annotation of pbmc2 data that training from pbmc1 data by 'scAnnotate'.
#'
#' @docType data
#' @usage data(predict_label, package="scAnnotate")
#' @format a data frame
#' @keywords datasets
#'
"predict_label"
