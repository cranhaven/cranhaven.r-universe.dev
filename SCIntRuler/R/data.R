#' My Example Dataset
#'
#' An example PBMC data with SingleCellExperiment format, including 3000 cells and 800 genes.
#'
#' @format An example PBMC data with SingleCellExperiment format
#' \describe{
#'   \item{int_elementMetadata}{A DataFrame with 3000 rows and 1 column, storing simulated gene information.}
#'   \item{int_colData}{A DataFrame with 800 rows and 3 columns, representing metadata for each cell.}
#'   \item{int_metadata}{A list containing two elements that provide additional global metadata about the experiment.}
#'   \item{rowRanges}{A CompressedGRangesList object providing genomic range data associated with each row/gene.}
#'   \item{colData}{A DataFrame with 800 rows and 8 columns, detailing cell-level metadata.}
#'   \item{assays}{A SimpleAssay object with matrix dimensions 3000x800, representing the gene expression matrix.}
#'   \item{elementMetadata}{A DataFrame linked with assays, providing gene-level metadata.}
#' }
#' @details
#' The "sim_data_sce" object is designed to serve as a teaching and development aid for methods that require complex
#' single-cell expression data. It includes several typical features found in single-cell datasets, such as varied levels of
#' gene expression and metadata describing both cells and genes.
#'
#' The data within this object are entirely synthetic and should not be used for real analysis. The main use case is for
#' testing and development of single-cell analysis methodologies.
#'
#' @references
#' The data were generated using a combination of random number generation for expression values and curated sources for
#' metadata to simulate realistic experimental scenarios.
#' @return Simulation data to exemplify the usage of the method.
#' @examples
#' data("sim_data_sce")
#' @name sim_data_sce
"sim_data_sce"


