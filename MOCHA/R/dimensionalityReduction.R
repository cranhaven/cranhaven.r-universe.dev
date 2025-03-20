#' @title \code{bulkDimReduction}
#'
#' @description \code{bulkDimReduction} runs dimensionality reduction (either PCA or LSI). We adapt Signac's
#'
#' @param SampleTileObj The SummarizedExperiment object output from getSampleTileMatrix
#' @param cellType vector of strings. Cell subsets for which to call
#'   peaks. This list of group names must be identical to names that appear in
#'   the SampleTileObj.  Optional, if cellPopulations='ALL', then peak
#'   calling is done on all cell populations. Default is 'ALL'.
#' @param componentNumber integer. Number of components to include in LSI, or PCA This must be strictly less than
#' @param method a string. Represents the method to use. Includes LSI or PCA, but we do not recommend PCA for scATAC pseudobulk.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return SEObj a SummarizedExperiment containing PC components from dimensionality reduction and metadata from the SampleTileObj
#'
#' @references LSI method adapted from Andrew Hill: http://andrewjohnhill.com/blog/2019/05/06/dimensionality-reduction-for-scatac-data/
#'
#' @examples
#' \dontrun{
#' LSIObj <- MOCHA::bulkDimReduction(SampleTileObj, cellType = "CD16_Mono")
#' }
#' @export
#'
#'
bulkDimReduction <- function(SampleTileObj, cellType = "All", componentNumber = 30, method = "LSI", verbose = FALSE) {
  if (!requireNamespace("irlba", quietly = TRUE)) {
    stop(
      "Package 'irlba' is required for bulkDimReduction. ",
      "Please install 'irlba' to proceed."
    )
  }
  allCellTypes <- names(SummarizedExperiment::assays(SampleTileObj))
  if (all(tolower(cellType) == "all")) {
    fullObj <- combineSampleTileMatrix(SampleTileObj)
    countMat <- SummarizedExperiment::assays(fullObj)[[1]]
  } else if (all(cellType %in% allCellTypes)) {
    newTSAM <- subsetMOCHAObject(SampleTileObj,
      subsetBy = "celltype",
      groupList = cellType, subsetPeaks = TRUE,
      verbose = verbose
    )
    fullObj <- combineSampleTileMatrix(newTSAM)
    countMat <- SummarizedExperiment::assays(fullObj)[[1]]
  } else {
    stop("cellType not found. SampleTileObj must contain the given cellType.")
  }

  countMat[is.na(countMat)] <- 0

  if (tolower(method) == "lsi") {
    if (!requireNamespace("irlba", quietly = TRUE)) {
      stop(
        "Package 'Matrix' is required for bulkDimReduction with method = 'lsi'. ",
        "Please install 'Matrix' to proceed."
      )
    }
    # TF-IDF step
    freqs <- t(t(countMat) / Matrix::colSums(countMat))
    idf <- log(1 + ncol(countMat) / Matrix::rowSums(countMat))
    tfidf <- Matrix::Diagonal(x = as.vector(idf)) %*% freqs

    # SVD step, using componentNumber
    tryCatch(
      {
        svd <- irlba::irlba(tfidf, componentNumber, componentNumber)
      },
      error = function(cond) {
        message(cond)
        stop("Columns containing all NAs may be present in SampleTileObj")
      }
    )
    svdDiag <- matrix(0, nrow = componentNumber, ncol = componentNumber)
    diag(svdDiag) <- svd$d
    matSVD <- t(svdDiag %*% t(svd$v))
    rownames(matSVD) <- colnames(countMat)
    colnames(matSVD) <- paste0("LSI", seq_len(ncol(matSVD)))

    assayList1 <- list(t(matSVD))
    names(assayList1) <- "LSI"

    newMetadata <- fullObj@metadata
    newMetadata$History <- append(newMetadata$History, paste("bulkDimReduction", utils::packageVersion("MOCHA")))

    DimReducObj <- SummarizedExperiment::SummarizedExperiment(
      assayList1,
      metadata = newMetadata,
      colData = SummarizedExperiment::colData(fullObj)
    )
  } else if (tolower(method) == "pca") {

    # SVD step, using 30 components
    tryCatch(
      {
        pca <- irlba::prcomp_irlba(countMat, componentNumber)
      },
      error = function(cond) {
        message(cond)
        stop("Columns containing all NAs may be present in SampleTileObj")
      }
    )

    pc_rotation <- pca$rotation
    rownames(pc_rotation) <- colnames(countMat)
    loadings <- pca$x
    rownames(loadings) <- rownames(countMat)

    newMetadata <- fullObj@metadata
    newMetadata$History <- append(newMetadata$History, paste("bulkDimReduction", utils::packageVersion("MOCHA")))
    newMetadata <- append(pca[c("scale", "totalvar", "sdev", "center")], newMetadata)

    assayList1 <- list(t(pc_rotation))
    names(assayList1) <- c("PCA")

    DimReducObj <- SummarizedExperiment::SummarizedExperiment(
      assayList1,
      rowData = t(loadings),
      metadata = newMetadata,
      colData = SummarizedExperiment::colData(fullObj)
    )
  } else {
    stop("Method not recognized. Must be LSI or PCA.")
  }

  return(DimReducObj)
}

#' @title \code{bulkUMAP}
#'
#' @description \code{bulkUMAP} generates UMAP from pseudobulk LSIObj object, and merges in metadata.
#'
#' @param SEObj The SummarizedExperiment object output from bulkDimReduction, or an STM, subsetted down to just one cell type.
#' @param assay A string, describing the name of the assay within SEObj to run UMAP ('PCA', 'LSI', or 'counts').
#' @param components A vector of integers. Number of components to include in LSI (1:30 typically).
#' @param seed an integer. Represents the random seed to pass to the UMAP. Default seed is 1.
#' @param returnModel A boolean. Default is FALSE. If set to true, it will return a list, where the first is the UMAP coordinates with metadata for plotting, and the second is the full UMAP model so further projection can occur.
#' @param nNeighbors See  \link[uwot]{umap}. The size of local neighborhood (in terms of number of
#'           neighboring sample points) used for manifold approximation. Default is 15.
#' @param ... Additional arguments to be passed to \link[uwot]{umap}.
#'
#' @return fullUMAP data.frame of UMAP values with metadata attached.
#'
#' @examples
#' \dontrun{
#' UMAPvalues <- MOCHA::bulkUMAP(LSIObj)
#' }
#' @export
#'
bulkUMAP <- function(SEObj,
                     assay = "LSI",
                     components = c(1:30),
                     nNeighbors = 15,
                     returnModel = FALSE,
                     seed = 1,
                     ...) {
  if (!requireNamespace("uwot", quietly = TRUE)) {
    stop(
      "Package 'uwot' is required for bulkUMAP. ",
      "Please install 'uwot' to proceed."
    )
  }

  set.seed(seed)
  if (!any(names(SummarizedExperiment::assays(SEObj)) == assay)) {
    stop("Assay was not represented in the SEObj. ")
  }
  countMat <- t(SummarizedExperiment::assays(SEObj)[[assay]])

  if (any(is.na(countMat))) {
    stop("The given matrix contains NA. Remove and try again.")
  }

  if (!all(components %in% seq_along(colnames(countMat)))) {
    stop("Component list does not align with number of LSI components.")
  }

  if (!returnModel) {
    subUMAP <- as.data.frame(
      uwot::umap(countMat[, components], n_neighbors = nNeighbors, batch = TRUE, ...)
    )

    colnames(subUMAP) <- c("UMAP1", "UMAP2")
    subUMAP$Sample <- rownames(subUMAP)

    fullUMAP <- dplyr::left_join(
      subUMAP,
      as.data.frame(SummarizedExperiment::colData(SEObj)),
      by = "Sample"
    )

    return(fullUMAP)
  } else {
    umapOut <- uwot::umap(countMat[, components], n_neighbors = nNeighbors, ret_model = TRUE, batch = TRUE, ...)
    subUMAP <- as.data.frame(umapOut$embedding)
    colnames(subUMAP) <- c("UMAP1", "UMAP2")
    subUMAP$Sample <- rownames(subUMAP)

    fullUMAP <- dplyr::left_join(
      subUMAP,
      as.data.frame(SummarizedExperiment::colData(SEObj)),
      by = "Sample"
    )

    outList <- list(fullUMAP, umapOut)
    names(outList) <- c("UMAP", "modelOutput")
    return(outList)
  }
}
