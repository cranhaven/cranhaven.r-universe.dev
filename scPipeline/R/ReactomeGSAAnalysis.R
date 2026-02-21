#' Reactome Data Analysis for Seurat Object
#'
#' This function performs pathway analysis using ReactomeGSA on a Seurat object
#' with cluster information.
#'
#' @param lowdim_seurat_object Seurat object that has clusters information
#' @return A list containing:
#'         - GSVA result (`gsva_result`)
#'         - Pathway expression data (`pathway_expression`)
#'         - Max difference between pathway expression values (`max_difference`)
#' @export
#' @importFrom ReactomeGSA analyse_sc_clusters pathways
#' @examples
#' \donttest{
#' library(Seurat)
#' # Read 10X counts data from matrix.mtx, barcodes.tsv and genes.tsv
#' counts <- Read10X(data.dir = "../inst/extdata", gene.column = 1)
#'
#' # Create Seurat object without batch correction
#' seurat_obj <- SeuratPreprocess(counts)
#' seurat_obj <- SeuratLowDim(seurat_obj)
#' # Reactome Analysis
#' seurat_reactome <- ReactomeData(seurat_obj)
#' }
ReactomeData <- function(lowdim_seurat_object) {
  # Step 1: Perform GSVA pathway analysis
  gsva_result <- ReactomeGSA::analyse_sc_clusters(lowdim_seurat_object)

  # Step 2: Extract pathway expression data
  pathway_expression <- ReactomeGSA::pathways(gsva_result)

  # Step 3: Clean pathway expression column names
  colnames(pathway_expression) <- gsub("\\.Seurat", "", colnames(pathway_expression))

  # Step 4: Calculate max differences in pathway expression
  max_difference <- do.call(rbind, apply(pathway_expression, 1, function(row) {
    values <- as.numeric(row[2:length(row)])
    return(data.frame(name = row[1], min = min(values), max = max(values)))
  }))

  # Step 5: Add difference column and sort
  max_difference$diff <- max_difference$max - max_difference$min
  max_difference <- max_difference[order(max_difference$diff, decreasing = TRUE), ]

  # Return the results
  return(list(gsva_result = gsva_result, pathway_expression = pathway_expression, max_difference = max_difference))
}
