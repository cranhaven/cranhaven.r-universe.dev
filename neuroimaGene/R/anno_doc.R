#' NIDP annotation data
#'
#' annotation data for all NIDPs taken from the UKbiobank
#'
#' @docType data
#' @name anno
#' @format A data.table with 10 columns and 3935 rows:
#' \describe{
#'   \item{gwas_phenotype}{character: UKB-derived neuroimaging derived phenotype (NIDP)}
#'   \item{modality}{character: MRI neuroimaging modality}
#'   \item{atlas}{character: neuroimaging cortical atlas}
#'   \item{side}{character: right or left hemisphere or midline/whole brain}
#'   \item{primary}{character: primary cortical region}
#'   \item{secondary}{character: secondary cortical region}
#'   \item{region}{character: named region of the brain}
#'   \item{measurement}{character: morphology measurement}
#'   \item{fMRI_node_1}{character: fMRI node 1}
#'   \item{fMRI_node_2}{character: fMRI node 2}
#'   \item{NIDP}{character: user-friendly name for each NIDP}
#' }
#' @source Bledsoe, X. (2024) A transcriptomic atlas of the human brain reveals genetically determined aspects of neuropsychiatric health
#' @returns This script has no return. This is a documentation file for the
#' annotation dataset for all neuroimaging derived phenotypes.
NULL
