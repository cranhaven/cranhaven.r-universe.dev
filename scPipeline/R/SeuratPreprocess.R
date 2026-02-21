#' Preprocess count data and create a Seurat object
#'
#' This function preprocesses count data, optionally applying batch correction using batchelor::fastMNN,
#' and creates a Seurat object.
#'
#' @export
#' @importFrom Seurat CreateSeuratObject
#' @importFrom Seurat NormalizeData
#' @importFrom Seurat FindVariableFeatures
#' @importFrom Seurat PercentageFeatureSet
#' @importFrom Seurat ScaleData
#' @importFrom Seurat as.Seurat
#' @importFrom Seurat SplitObject
#' @importFrom batchelor fastMNN
#' @param counts_data A matrix or data frame of count data.
#' @param meta.data A data frame containing metadata to include in the Seurat object. Default is NULL.
#' @param batch_column A vector or factor specifying batch assignments for each cell. Default is NULL.
#' @param use_fastMNN Logical. Whether to apply batch correction using fastMNN. Default is FALSE.
#' @param ... Additional arguments to be passed to Seurat::CreateSeuratObject.
#' @return A Seurat object.
#' @examples
#' \donttest{
#' library(Seurat)
#' # Read 10X counts data from matrix.mtx, barcodes.tsv and genes.tsv
#' counts <- Read10X(data.dir = "../inst/extdata", gene.column = 1)
#'
#' # Create Seurat object without batch correction
#' seurat_obj <- SeuratPreprocess(counts)
#' }
SeuratPreprocess <- function(counts_data, meta.data = NULL, batch_column = NULL, use_fastMNN = FALSE, ...) {
  n_size <- dim(counts_data)[2]

  # Step 1: Create Seurat object
  seurat_object <- Seurat::CreateSeuratObject(
    counts = counts_data,
    project = "project_title",
    min.cells = 3,
    min.features = 200,
    ...
  )

  # Step 2: Normalize data
  seurat_object <- NormalizeData(seurat_object, normalization.method = "LogNormalize", scale.factor = n_size)

  # Step 3: Find variable features
  seurat_object <- FindVariableFeatures(seurat_object)

  # Step 4: Calculate mitochondrial percentage
  seurat_object[["percent.mt"]] <- PercentageFeatureSet(seurat_object, pattern = "^mt-")

  # Step 5: Apply batch correction (if requested)
  if (use_fastMNN) {
    if (is.null(batch_column)) {
      stop("Batch correction requires a 'batch_column' to specify batch assignments.")
    }

    # Add batch information to metadata
    seurat_object$batch <- batch_column

    # Split object by batch
    batches <- SplitObject(seurat_object, split.by = "batch")

    # Convert to SingleCellExperiment objects
    sce_list <- lapply(batches, Seurat::as.SingleCellExperiment)

    # Perform batch correction using fastMNN
    corrected <- batchelor::fastMNN(sce_list)

    # Convert back to Seurat object
    seurat_object <- Seurat::as.Seurat(corrected)
  }

  # Step 6: Scale data, regressing out mitochondrial percentage
  seurat_object <- ScaleData(seurat_object, vars.to.regress = "percent.mt")

  return(seurat_object)
}
