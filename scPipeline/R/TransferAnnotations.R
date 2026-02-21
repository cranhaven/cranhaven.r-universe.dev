#' Transfer annotations to Seurat clusters
#'
#' This function assigns cluster-level annotations in a Seurat object based on the majority
#' annotation of cells within each cluster.
#'
#' @param seurat_object Seurat object containing cluster and annotation information.
#' @param annotation_col The name of the metadata column with annotations (character string).
#' @param cluster_col The name of the metadata column with cluster information (character string).
#' @param output_col The name of the output column to store cluster annotations (character string).
#' @importFrom dplyr group_by summarise
#' @importFrom Seurat DimPlot
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @return The Seurat object with an additional column in its metadata, specified by `output_col`.
#' @export
TransferAnnotations <- function(seurat_object, annotation_col, cluster_col, output_col) {
  # Step 1: Validate input columns
  if (!annotation_col %in% colnames(seurat_object@meta.data)) {
    stop(paste0("Annotation column '", annotation_col, "' not found in metadata."))
  }
  if (!cluster_col %in% colnames(seurat_object@meta.data)) {
    stop(paste0("Cluster column '", cluster_col, "' not found in metadata."))
  }

  # Step 2: Calculate majority annotations
  metadata <- seurat_object@meta.data
  majority_annotations <- metadata %>%
    group_by(!!sym(cluster_col)) %>%
    summarise(
      majority_annotation = names(sort(table(!!sym(annotation_col)), decreasing = TRUE)[1])
    )

  # Step 3: Map annotations to all cells
  seurat_object@meta.data[[output_col]] <- majority_annotations$majority_annotation[
    match(seurat_object@meta.data[[cluster_col]], majority_annotations[[cluster_col]])
  ]

  # Return the updated Seurat object
  return(seurat_object)
}
