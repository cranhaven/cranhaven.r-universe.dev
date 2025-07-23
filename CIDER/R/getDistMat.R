#' Calculate the Similarity Matrix
#'
#' Compute the IDER-based similarity matrix for a list of Seurat objects. This 
#' function does not regress out batch effects and is designed for use during the initial clustering step.
#'
#' @param seu_list A list containing Seurat objects. Required.
#' @param verbose Logical. If \code{TRUE}, progress messages and a progress bar are displayed. Default is \code{TRUE}.
#' @param tmp.initial.clusters Character string specifying one of the column names from \code{Seurat@meta.data} 
#' that denotes groups, e.g., initial clusters. 
#' Default is "seurat_clusters".
#' @param method Character string specifying the method for differential expression analysis.
#'  Options are "voom" or "trend" (default is "trend").
#' @param batch.var Character string specifying the metadata column containing batch information. Default is "Batch".
#' @param additional.variate Character vector of additional variates to include in the linear model for regression.
#' @param downsampling.size Numeric value indicating the number of cells to use per group. Default is 35.
#' @param downsampling.include Logical. Whether to include groups with fewer cells than 
#' \code{downsampling.size}. Default is \code{TRUE}.
#' @param downsampling.replace Logical. Whether to sample with replacement for groups 
#' smaller than \code{downsampling.size}. Default is \code{TRUE}.
#'
#' @return A list of similarity matrices.
#'
#' @seealso \code{\link{calculateDistMatOneModel}}
#'
#' @export
#'
#' @import Seurat utils limma
#' @importFrom edgeR cpm
getDistMat <- function(seu_list,
                       verbose = TRUE,
                       tmp.initial.clusters = "seurat_clusters",
                       method = "trend",
                       batch.var = "Batch",
                       additional.variate = NULL,
                       downsampling.size = 35,
                       downsampling.include = TRUE,
                       downsampling.replace = TRUE) {
  dist_coef <- list()

  if (verbose == TRUE) { # create progress bar
    pb <- txtProgressBar(min = 0, max = length(seu_list), style = 3)
    k <- 1
  }

  for (seu_itor in seq_along(seu_list)) {
    df_info <- data.frame(
      label = seu_list[[seu_itor]][[tmp.initial.clusters]],
      batch = seu_list[[seu_itor]][[batch.var]]
      # donor = seu_list[[seu_itor]]$Tissue
    )
    colnames(df_info) <- c("label","batch")

    idx <- downsampling(
      metadata = df_info, n.size = downsampling.size,
      include = downsampling.include, replace = downsampling.replace
    )
    idx <- sort(idx)

    to_add <- idx[duplicated(idx)]
    idx <- idx[!duplicated(idx)]
    matrix <- .getCountsMatrix(seu_list[[seu_itor]])[, idx] # version-aware layer/slot handling

    if (length(to_add) > 0) {
      matrix2 <- data.frame(.getCountsMatrix(seu_list[[seu_itor]])[, to_add])
      colnames(matrix2) <- paste0(colnames(matrix2), seq_len(ncol(matrix2)))
      matrix2 <- as.matrix(matrix2)
      matrix <- cbind(matrix, matrix2)
      rm(matrix2)
    }

    if (length(unique(df_info$label[idx])) > 2) {
      dist_coef[[seu_itor]] <- calculateDistMatOneModel(
        matrix = matrix, metadata = df_info[c(idx, to_add), ],
        # matrix = matrix, metadata = df_info[idx, ],
        verbose = verbose, method = method,
        additional.variate = additional.variate
      )
    }

    if (verbose == TRUE) {
      setTxtProgressBar(pb, k) # progress bar
      k <- k + 1
    }
  }
  if (verbose == TRUE) {
    close(pb) # close progress bar
  }

  return(dist_coef)
}

#' Calculate Distance Matrix Using a Single Model
#'
#' This function computes a similarity matrix by utilising 
#' a single linear model for differential expression analysis.
#'
#' @param matrix A count matrix with rows representing genes or features and columns representing samples or cells.
#' @param metadata A data frame containing metadata corresponding to the samples or cells. 
#' Each row should match a column in \code{matrix}.
#' @param verbose Logical. If \code{TRUE}, the function displays progress messages 
#' and a progress bar. The default is \code{TRUE}.
#' @param method A character string specifying the method for differential 
#' expression analysis. Options are "voom" or "trend", with "trend" as the default.
#' @param additional.variate A character vector of additional variates to 
#' include in the linear model for regression.
#'
#' @return A similarity matrix.
#'
#' @seealso \code{\link{getDistMat}}
#'
#' @export
#'
#' @import limma edgeR
#' @importFrom stats model.matrix cor coef
calculateDistMatOneModel <- function(matrix, metadata,
                                     verbose = TRUE,
                                     method = "voom",
                                     additional.variate = NULL)
  {
  keep <- rowSums(matrix > 0.5) > 5
  dge <- edgeR::DGEList(counts = matrix[keep,,drop=FALSE])
  # make a edgeR object
  dge <- dge[!grepl("ERCC-", rownames(dge)),] # remove ERCC
  dge <- dge[!grepl("MT-", rownames(dge)),]
  dge <- dge[!grepl("mt-", rownames(dge)),]

  df <- data.frame(g = paste(metadata$label, metadata$batch, sep = "_"),
                   b = metadata$batch, ## batch
                   c = metadata$label, ## label
                   stringsAsFactors = FALSE)
  df$detrate <- scale(colMeans(matrix > 0))[,1] # gene detection rate
  rownames(df) <- colnames(matrix)

  N <- length(unique(df$g)) # number of initial groups
  combinations <- data.frame(g1 = rep(unique(df$g), each = N),
                             g2 = rep(unique(df$g), N),
                             stringsAsFactors = FALSE)
  combinations <- combinations[combinations$g1 != combinations$g2, ]

  idx <- c()
  for(i in 2:nrow(combinations)){
    if(!combinations$g2[i] %in% combinations$g1[seq_len(i-1)]) {
      idx <- c(idx, i)
    }
  }

  combinations <- combinations[c(1,idx),]
  rownames(combinations) <- seq_len(nrow(combinations))

  dist_coef <- matrix(0, nrow = N, ncol = N)
  colnames(dist_coef) <- rownames(dist_coef) <- sort(unique(df$g))

  if("donor" %in% colnames(metadata)){
    df$subb <- metadata$donor
    design <- model.matrix(~  0 + g + subb + detrate, data = df)
  } else {
    design <- model.matrix(~  0 + g + detrate, data = df)
  }

  groups <- sort(unique(paste0("g", df$g)))
  n_groups <- length(groups) # number of groups

  df_contrasts <- data.frame(target_group = groups, contrast = NA)
  # prepare contrast matrix
  for(i in seq_len(n_groups)){
    df_contrasts$contrast[i] <- paste0(groups[i],
                                       "-(",
                                       paste(groups[-i], collapse = "+"),
                                       ")/", (n_groups-1))
  }
  # contrast matrix
  contrast_m <- makeContrasts(contrasts = df_contrasts$contrast,
                              levels = design)
  colnames(contrast_m) <- groups

  if (method == "voom") {
    v <- voom(dge, design, plot = FALSE)
    fit <- lmFit(v, design)
    group_fit <- contrasts.fit(fit, contrast_m)
  } else if (method == "trend") {
    logCPM <- edgeR::cpm(dge, log=TRUE, prior.count=3)
    fit <- lmFit(logCPM, design)
    group_fit <- contrasts.fit(fit, contrast_m)
  }
  # pairwise comparison
  for(i in seq_len(nrow(combinations))){
    idx1 <- rownames(dist_coef) == combinations$g1[i]
    idx2 <- colnames(dist_coef) == combinations$g2[i]
    pos1 <- df_contrasts$target_group == paste0("g", combinations$g1[i])
    pos2 <- df_contrasts$target_group == paste0("g", combinations$g2[i])
    dist_coef[idx1, idx2] <- cor(coef(group_fit)[, pos1],
                                 coef(group_fit)[, pos2])
  }
  return(dist_coef)
}

