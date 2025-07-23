#' Downsampling Cells
#'
#' Downsamples cells from each group for IDER-based similarity calculation.
#'
#' @param metadata A data frame containing at least two columns: one for group labels
#' and one for batch information. Each row corresponds to a single cell. Required.
#' @param n.size Numeric value specifying the number of cells to use in each group.
#' Default is \code{35}.
#' @param seed Numeric value to set the random seed for sampling. Default is \code{12345}.
#' @param include Logical value indicating whether to include groups that have fewer cells than \code{n.size}.
#' Default is \code{FALSE}.
#' @param replace Logical value specifying whether to sample with replacement if a group
#' is smaller than \code{n.size}. Default is \code{FALSE}.
#' @param lower.cutoff Numeric value indicating the minimum group size required for inclusion.
#' Default is \code{3}.
#'
#' @return A list of numeric indices (or cell names) for cells to be kept for downstream computation.
#'
#' @export
#'
#' @examples
#'   # 'meta' is a data frame with columns 'label' and 'batch'
#'   meta <- data.frame(
#'     label = c(rep("A", 40), rep("A", 35), rep("B", 20)),
#'     batch = c(rep("X", 40), rep("Y", 35), rep("X", 20))
#'   )
#'   keep_cells <- downsampling(meta, n.size = 35, seed = 12345)
#'   
#'   # Display the selected indices
#'   print(keep_cells)
#' 
downsampling <- function(metadata, n.size = 35, seed = NULL, include = FALSE,
                         replace = FALSE, lower.cutoff = 3) {
  if(!"label" %in% colnames(metadata)) {
    stop("metadata error. cannot downsample.")
  }
  if(!"batch" %in% colnames(metadata)) {
    stop("metadata error. cannot downsample.")
  }
  cluster <- unique(metadata$label)
  tech <- unique(metadata$batch)
  select <- c()
  for (i in cluster) {
    for (j in tech) {
      idx <- which(metadata$label %in% i & metadata$batch %in% j)
      if (length(idx) > n.size) {
        if(!is.null(seed)){
          set.seed(seed)
        }
        select <- c(select, sample(idx, size = n.size, replace = FALSE))
      } else if (length(idx) == n.size) {
        select <- idx
      } else if (length(idx) < n.size & length(idx) >= lower.cutoff) {
        if (include & !replace) {
          select <- c(select, idx)
        } else if (include & replace) {
          if(!is.null(seed)){
            set.seed(seed)
          }
          select <- c(select, sample(idx, size = n.size, replace = TRUE))
        }
      }
    }
  }
  return(select)
}

dist2similarity <- function(dist){
  sml <- dist[[1]] + t(dist[[1]])
  sml <- 1 - sml
  diag(sml) <- 1
}

getSharedGroups <- function(seu, dist, batch.var="Batch"){

  batches <- unique(seu@meta.data[[batch.var]])
  groups <- colnames(dist)
  idx1 <- colnames(dist)[grep(paste0("_", batches[1],"$"), colnames(dist))]
  # end with _Batch1
  idx2 <- colnames(dist)[grep(paste0("_", batches[2],"$"), colnames(dist))]
  # end with _Batch2

  g1 <- gsub(paste0("_", batches[1]), "",idx1)
  g2 <- gsub(paste0("_", batches[2]), "",idx2)
  shared_g <- intersect(g1,g2)

  idx1_shared <- paste0(shared_g, paste0("_", batches[1]))
  idx2_shared <- paste0(shared_g, paste0("_", batches[2]))

  return(list(shared_g, idx1_shared, idx2_shared))
}

#' @importFrom stats cor
measureSimilarity <- function(x1, x2, method = "pearson"){
  # Measure similarity between two vectors
  # 
  # Measure similarity between two vectors
  # 
  # param x1 x1
  # param x2 x2
  # param method method
  # return similarity matrix
  if (!is.null(x1) & !is.null(x2)) {
    if(length(x1) != length(x2)) {
      warning("x1 or x2 don't have the same length for similarity measures")
      return(NA)
    }
    if (method %in% c("pearson", "spearman","kendall")) {
      return(cor(x1, x2, method = method))
    } else if (method == "cosine") {
      x <- matrix(cbind(x1, x2))
      return(1-cosineSimilarityR(x)[1,2])
    }
  } else {
    warning("x1 or x2 are not valid for similarity measures")
    return(NA)
  }
}

cosineSimilarityR <- function(x) {
  # Calculate Cosine Similarity in R
  # 
  # This function computes the cosine similarity between all rows of a numeric matrix.
  # 
  # Cosine similarity is defined as the dot product of two vectors divided by the product of
  # their Euclidean norms.
  # 
  # param x A numeric matrix. Each row represents an observation for which the cosine similarity is calculated.
  # 
  # return A numeric similarity matrix. The entry in the \emph{i}-th row and \emph{j}-th
  # column corresponds to the cosine similarity between the \emph{i}-th and \emph{j}-th rows of \code{x}.
  # 
  # details The cosine similarity ranges from -1 to 1, where 1 indicates identical orientation,
  # 0 indicates orthogonality, and -1 indicates opposite orientation.
  y <- t(x) %*% x
  res <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  return(res)
}

#' @references Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W, Smyth GK
#' (2015). limma powers differential expression analyses for RNA-sequencing
#' and microarray studies. Nucleic Acids Research, 43(7), e47.
#' doi: 10.1093/nar/gkv007.
.zeroDominantMatrixMult <- function(A,B)
{
  # Computes A %*% B, except that a zero in B will always produce
  # zero even when multiplied by an NA in A, instead of NA as usually
  # produced by R arithmetic.
  # A and B are numeric matrices and B does not contain NAs.
  # In the limma usage, A usually has far more rows than columns
  # and B is relatively small.
  # AUTHOR: Gordon Smyth
  # Created 16 Feb 2018. Modified 2 Feb 2020.
  # THIS IS AN UNEXPORTED FUNCTION FROM R PACKAGE LIMMA
  # https://bioconductor.org/packages/release/bioc/html/limma.html

  # Proportion of zeros in B
  Z <- (B==0)
  MeanZ <- mean(Z)

  # Decide whether to run guarded or ordinary matrix multiplication
  # MeanZ can only be NA if B has 0 elements
  if(!is.na(MeanZ) && (MeanZ > 0)) {
    if(MeanZ >= 0.8)
      # Full algorithm is quick if there are lots of zeros
      Guard <- TRUE
    else {
      RowBHasZero <- (rowSums(Z) > 0)
      if(mean(RowBHasZero) > 0.4) {
        # If the matrix is big, it's much quicker to check the whole matrix
        # than to subset it
        Guard <- anyNA(A)
      } else {
        Guard <- anyNA(A[,RowBHasZero])
      }
    }
  } else {
    Guard <- FALSE
  }

  if(Guard) {
    dn <- list()
    dn[[1]] <- rownames(A)
    dn[[2]] <- colnames(B)
    D <- matrix(0,nrow(A),ncol(B),dimnames=dn)
    for (j in seq_len(ncol(B))) {
      z <- B[,j]==0
      if(any(z))
        D[,j] <- A[,!z,drop=FALSE] %*% B[!z,j,drop=FALSE]
      else
        D[,j] <- A %*% B[,j]
    }
    return(D)
  } else {
    return(A %*% B)
  }
}


.checkSeuratObjectVersion <- function(obj) {
  ## check the version of seurat object; chatgpt code; verified 31 jan 2025
  # First confirm it's a Seurat object
  if (!inherits(obj, "Seurat")) {
    stop("Not a valid Seurat object.")
  }
  # In very old Seurat objects (v2 or older), @version might be missing
  if (is.null(obj@version$major)) {
    return("v2 or older (no @version slot)")
  }
  # Otherwise, retrieve the major version
  major_ver <- obj@version$major
  if (major_ver == 5) {
    return("v5")
  } else if (major_ver == 4) {
    return("v4")
  } else if (major_ver == 3) {
    return("v3")
  } else {
    return("v2 or older")
  }
}

.getCountsMatrix <- function(seu, assay = "RNA") {
  ## get the seurat count matrix; gpt assisted function
  obj_version <- .checkSeuratObjectVersion(seu)
  
  # If version is NOT in c("v1","v2-or-earlier","v3","v4"),
  # we treat it as "v5 or newer" and use `layer`.
  # Adjust the condition as you see fit for your own logic.
  if (!obj_version %in% c("v2 or older", "v3", "v4")) {
    # v5 or newer
    layer_names <- names(seu@assays[[assay]]@layers)
    counts_layers <- grep("^counts", layer_names, value = TRUE)
    if (length(counts_layers) == 0) {
      stop("No layer(s) named 'counts' found in assay ", assay, ".")
    }
    # Retrieve each counts layer as a matrix and combine via cbind
    if("counts" %in% counts_layers){
      mat <- as.matrix(GetAssayData(seu, assay = "RNA", layer = "counts"))
      return(mat)
      
    } else {
      mat_list <- lapply(counts_layers, function(ln) {
        GetAssayData(seu, assay = assay, layer = ln)
      })
      # cbind all counts.* layers together
      # (They should have the same number of rows but different columns)
      merged_mat <- do.call(cbind, mat_list)
      merged_mat <- as.matrix(merged_mat)
      return(merged_mat)
    }

  } else {
    # v4 or older
    mat <- as.matrix(GetAssayData(seu, assay = "RNA", slot = "counts"))
    return(mat)
  }
  
}
