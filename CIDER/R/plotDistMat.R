#' Plot Similarity Matrix with pheatmap
#'
#' This function creates a heatmap of the similarity matrix computed by \code{getDistMat()}.
#'
#' @param dist.list A list representing the similarity matrix output by \code{getDistMat()}. Required.
#' @param use Character string specifying the similarity measure to use. Default is "coef". 
#' No other option is currently available.
#'
#' @return A \code{pheatmap} object displaying the similarity matrix.
#'
#' @seealso \code{\link{getDistMat}}
#'
#' @export
#'
#' @import pheatmap
plotDistMat <- function(dist.list, use = "coef") {
  if (use == "coef") {
    dist_coef <- dist.list[[1]]
  } else if (use == "p") {
    dist_coef <- dist.list[[2]]
  }

  hm <- list()
  for (i in which(vapply(dist_coef, function(x) {
    return(!is.null(x))
  }))) {
    tmp <- dist_coef[[i]] + t(dist_coef[[i]])
    diag(tmp) <- 1
    hm[i] <- pheatmap::pheatmap(tmp, display_numbers = TRUE)
  }
  return(hm)
}

#' Plot Heatmap for the IDER-Based Similarity Matrix
#'
#' This function generates a heatmap that visualises the similarity between shared 
#' groups across batches, as computed by \code{getIDEr}.
#'
#' @param seu A Seurat object.
#' @param ider The output list from the \code{getIDEr} function.
#' @param batch.var Character string specifying the metadata column that contains batch information. Default is "Batch".
#'
#' @return A heatmap displaying the similarity between shared groups across batches.
#'
#' @seealso \code{\link{getIDEr}}
#'
#' @export
#'
#' @import pheatmap viridis
plotHeatmap <- function(seu, ider, batch.var = "Batch") {
  idx <- getSharedGroups(seu, ider[[1]], batch.var)
  shared_g <- idx[[1]] # shared groups
  if(length(shared_g) == 1){
    stop("No shared groups.")
  }
  idx1 <- idx[[2]] # rownames
  idx2 <- idx[[3]] # colnames

  pheatmap::pheatmap(
    ider[[1]][idx1, idx2],
    border_color = "grey20",
    color = viridis::inferno(50),
    display_numbers = TRUE,
    cluster_rows = FALSE,
    cluster_cols = FALSE
  )
}
