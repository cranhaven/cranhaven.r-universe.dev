#'@title Example Input Data for Compare Mode Analysis
#'@description A list containing example single-cell multi-omics data used in "compare" mode of the `scPOEM` package.
#'
#'@format A named list of length 2. Each element is itself a named list with the following components:
#'\describe{
#'    \item{\code{X}}{The scATAC-seq data, sparse matrix.}
#'    \item{\code{Y}}{The scRNA-seq data, sparse matrix.}
#'    \item{\code{peak_data}}{A data.frame containing peak information.}
#'    \item{\code{gene_data}}{A data.frame containing gene information (must contain column "gene_name").}
#'    \item{\code{cell_data}}{A data.frame containing cell metadata.}
#'    \item{\code{neibor_peak}}{The peak IDs within a certain range of each gene, must have cols c("gene_name", "start_use", "end_use"). The id numbers in "start_use" and "end_use" are start from 0.}
#'    \item{\code{genome}}{The genome length for the species.}
#'}
#'
#'@usage data(example_data_compare)
#'
#'@keywords datasets
#'
#'@examples
#'data(example_data_compare)
#'
#'@name example_data_compare
"example_data_compare"
