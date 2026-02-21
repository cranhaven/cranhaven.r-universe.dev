#' Annotate cells in a Seurat object using SingleR with Celldex
#'
#' This function annotates the cells in a Seurat object using the SingleR package
#' with reference data obtained from the Celldex package.
#'
#' @export
#' @importFrom SingleR SingleR
#' @importFrom celldex HumanPrimaryCellAtlasData
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom Seurat as.SingleCellExperiment AddMetaData
#' @param seurat_object A Seurat object to be annotated.
#' @param reference_data A reference dataset to use for annotation (e.g., HumanPrimaryCellAtlasData from Celldex).
#'        If NULL, HumanPrimaryCellAtlasData is used by default.
#' @param assay The assay in the Seurat object to use for annotation. Default is "RNA".
#' @return The Seurat object with cell annotations added to the metadata.
AnnotateCellsWithSingleR <- function(seurat_object, reference_data = NULL, assay = "RNA") {
  # Step 1: Load default reference data from Celldex if not provided
  if (is.null(reference_data)) {
    reference_data <- celldex::HumanPrimaryCellAtlasData()
  }

  # Step 2: Convert Seurat object to SingleCellExperiment
  sce <- Seurat::as.SingleCellExperiment(seurat_object, assay = assay)

  # Ensure it contains log-normalized counts
  if (!"logcounts" %in% names(SummarizedExperiment::assays(sce))) {
    SummarizedExperiment::assay(sce, "logcounts") <- log1p(SummarizedExperiment::assay(sce, "counts"))
  }

  # Step 3: Run SingleR to annotate cells
  singleR_results <- SingleR::SingleR(
    test = SummarizedExperiment::SummarizedExperiment(list(counts = sce)),
    ref = reference_data,
    labels = reference_data$label.main
  )

  # Step 4: Add SingleR annotations to Seurat metadata
  seurat_object <- Seurat::AddMetaData(
    object = seurat_object,
    metadata = singleR_results$labels,
    col.name = "SingleR_Labels"
  )

  return(seurat_object)
}
