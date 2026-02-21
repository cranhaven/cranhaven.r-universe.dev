#' Create a Low dimensional Seurat object from scaled seurat object
#'
#' This function converts the transformed data to low-dimensional data for downstream analysis.
#'
#' @export
#' @importFrom Seurat RunPCA
#' @importFrom Seurat FindNeighbors
#' @importFrom Seurat FindClusters
#' @importFrom Seurat RunTSNE
#' @importFrom Seurat RunUMAP
#' @param scaled_seurat_object A scaled Seurat object.
#' @param ... Additional arguments to be passed for downstream analyses.
#' @return A Seurat object.
#' @examples
#' \donttest{
#' library(Seurat)
#' # Read 10X counts data from matrix.mtx, barcodes.tsv and genes.tsv
#' counts <- Read10X(data.dir = "../inst/extdata", gene.column = 1)
#'
#' # Create Seurat object without batch correction
#' seurat_obj <- SeuratPreprocess(counts)
#' seurat_obj <- SeuratLowDim(seurat_obj)
#' }
SeuratLowDim <- function(scaled_seurat_object, ...) {
  # Compute number of PCs and dimensions
  n_size <- dim(scaled_seurat_object@meta.data)[1]
  n_pcs <- round(n_size / 100)
  n_dims <- round(n_pcs / 1.25)

  # Run PCA
  seurat_object <- RunPCA(scaled_seurat_object, npcs = n_pcs, ndims.print = 1:5, nfeatures.print = 5)

  # Find Neighbors
  seurat_object <- FindNeighbors(seurat_object, reduction = "pca", dims = 1:n_dims, nn.eps = 0.5)

  # Find Clusters
  seurat_object <- FindClusters(seurat_object, resolution = 3, n.start = 10)

  # Run t-SNE
  seurat_object <- RunTSNE(seurat_object, dims = 1:n_dims)

  # Run UMAP
  seurat_object <- RunUMAP(seurat_object, dims = 1:n_dims, min.dist = 0.75)

  return(seurat_object)
}
