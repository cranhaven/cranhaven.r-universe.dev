#' NeuroimaGene object for vignette illustration
#'
#' NeuroimaGene data table containing select associations used in the package vignette.
#'
#' @docType data
#' @name ng_vignette
#' @usage data(ng_vignette)
#' @format A data.table with 6 columns and 3824 rows:
#' \describe{
#'   \item{gene}{character: ENSEMBL Gene ID}
#'   \item{gene_name}{character: HUGO gene name}
#'   \item{gwas_phenotype}{character: neuroimaging derived phenotype}
#'   \item{training_model}{character: JTI derived tissue gene expression model}
#'   \item{zscore}{numeric: normalized effect size of GReX on NIDP morphology}
#'   \item{mod_BHpval}{character: Benjamini Hochberg corrected pvalue corrected by modality }
#' }
#' @source Bledsoe, X. (2024) A transcriptomic atlas of the human brain reveals genetically determined aspects of neuropsychiatric health
#' @returns This script has no return. This is a documentation file for the
#' neuroimaGene data subset required to build the vignette.
NULL
