#' A thresholded markers list for better calculation of DE genes
#'
#' This function calculates differentially expressed genes using Seurat::FindAllMarkers.
#'
#' @param lowdim_seurat_object Seurat object with cluster information
#' @importFrom Seurat FindAllMarkers
#' @return A list containing two marker lists:
#'         - Full markers list
#'         - Thresholded markers list with `min.pct = 0.1`
#' @export
#' @examples
#' \donttest{
#' library(Seurat)
#' # Read 10X counts data from matrix.mtx, barcodes.tsv and genes.tsv
#' counts <- Read10X(data.dir = "../inst/extdata", gene.column = 1)
#'
#' # Create Seurat object without batch correction
#' seurat_obj <- SeuratPreprocess(counts)
#' seurat_obj <- SeuratLowDim(seurat_obj)
#' # Create Markers list
#' seurat_markers <- SeuratMarkers(seurat_obj)
#' }
SeuratMarkers <- function(lowdim_seurat_object) {
  # Calculate the full markers list
  markers_list <- Seurat::FindAllMarkers(lowdim_seurat_object)

  # Calculate the thresholded markers list
  markers_list_threshold <- Seurat::FindAllMarkers(lowdim_seurat_object, min.pct = 0.1)

  # Return results as a list
  return(list(
    full_markers_list = markers_list,
    thresholded_markers_list = markers_list_threshold
  ))
}
