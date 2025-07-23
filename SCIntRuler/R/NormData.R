#' Normalized RNA data matrix
#'
#' @param seuratlist A list of Seurat objects, usually can be got by SplitObject().
#'
#' @return A list of matrix.
#' @export
#'
#' @examples
#' data(sim_data_sce)
#' sim_data <- SCEtoSeurat(sim_data_sce)
#' seuratlist <- Seurat::SplitObject(sim_data, split.by = "Study")
#' normCount <- NormData(seuratlist)

NormData <- function(seuratlist) {
  stopifnot(exprs = {
    is.list(seuratlist)
  })

  genelist <- c()
  for(i in seq_along(seuratlist)) {
    onecount <- Seurat::GetAssayData(seuratlist[[i]], slot = "counts")
    if(i == 1) {
      genelist <- rownames(onecount[(MatrixGenerics::rowSums(onecount>0) >= 3),])
    } else {
      genelist <- base::intersect(genelist, rownames(onecount[which(MatrixGenerics::rowSums(onecount>0) >= 3),]))
    }
  }
  normCount <- list()
  for(i in seq_along(seuratlist)) {
    onecount <- Seurat::GetAssayData(seuratlist[[i]], slot = "counts")[genelist, ]
    normCount[[i]] <- batchelor::cosineNorm(onecount, mode = "matrix")
  }
  return(normCount)
}
