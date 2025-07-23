#' Input and Split SingleCellExperiment Data
#'
#' This function takes a SingleCellExperiment object and a variable by which to split it,
#' converts it to a Seurat object, and then splits it according to the specified variable.
#'
#' @param sce A SingleCellExperiment object.
#'
#' @return A Seurat objects.
#' @export
#'
#' @examples
#' data(sim_data_sce)
#' # seuratlist <- InputData(sim_data_sce,"Study")
#' seuratobj <- SCEtoSeurat(sim_data_sce)



# InputData <- function(sce,split_var){
#
#   if(class(sce)[1] == "SingleCellExperiment"){
#     seuratobj <- SeuratObject::as.Seurat(sce)
#
#   }
#   seuratlist <- Seurat::SplitObject(seuratobj, split.by = split_var)
#   return(seuratlist)
#
# }


SCEtoSeurat <- function(sce) {


  # Extract count data (assuming counts are stored in the 'counts' assay)
  counts <- SummarizedExperiment::assay(sce, "counts")

  # Create a Seurat object from the count data
  seurat <- Seurat::CreateSeuratObject(counts = counts)

  # Add cell metadata from colData if not empty
  if (!is.null(SummarizedExperiment::colData(sce))) {
    cell_metadata <- as.data.frame(SummarizedExperiment::colData(sce))
    # Add cell metadata to the Seurat object
    for (col_name in colnames(cell_metadata)) {
      seurat <- Seurat::AddMetaData(seurat, metadata = cell_metadata[, col_name, drop = FALSE], col.name = col_name)
    }
  }

  # Add gene metadata from rowData if not empty
  if (!is.null(SummarizedExperiment::rowData(sce))) {
    gene_metadata <- as.data.frame(SummarizedExperiment::rowData(sce))
    # Ensure the row names match
    rownames(gene_metadata) <- rownames(seurat[["RNA"]])
    if (ncol(gene_metadata) > 0) {
      # Check if the meta.features slot exists
      # if ("meta.features" %in% slotNames(seurat[["RNA"]])) {
      #   seurat[["RNA"]]@meta.features <- cbind(seurat[["RNA"]]@meta.features, gene_metadata)
      # } else {
        # If meta.features does not exist, store gene metadata differently
        for (col_name in colnames(gene_metadata)) {
          seurat[["RNA"]] <- Seurat::AddMetaData(seurat[["RNA"]], gene_metadata[, col_name, drop = FALSE], col.name = col_name)
        }
     # }
    } else {
      warning("No gene metadata columns found to add to Seurat object.")
    }
  }

  # Return the Seurat object
  return(seurat)
}

