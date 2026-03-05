#' NIDP freesurfer annotation data
#'
#' Freesurfer names for select cortical and subcortical NIDPs
#' @docType data
#' @name fs_anno
#' @usage data(fs_anno)
#' @format A data.table with 7 columns and 890 rows:
#' \describe{
#'   \item{gwas_phenotype}{character: UKB-derived neuroimaging derived phenotype}
#'   \item{atl}{character: neuroimaging cortical atlas}
#'   \item{hemisphere}{character: right or left hemisphere or midline/whole brain}
#'   \item{secondary}{character: secondary cortical region}
#'   \item{fs_name}{character: freesurfer name}
#'   \item{label}{character: label name for the region}
#'   \item{atlas}{character: freesurfer name for neuroimaging cortical atlas}
#' }
#' @source Bledsoe, X. (2024) A transcriptomic atlas of the human brain reveals genetically determined aspects of neuropsychiatric health
#' @returns This script has no return. This is a documentation file for the
#' annotation dataset for all neuroimaging derived phenotypes in the fsbrain package.
NULL
