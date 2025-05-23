################################################################################
# CAESAR coembedding without image
################################################################################
#' Add Gene Embedding to Seurat Object
#'
#' @description
#' This function computes and adds gene embeddings to a Seurat object based on a provided adjacency matrix of spatial information and an existing cell embedding. It allows for the integration of gene-level information into the dimensional reduction of the Seurat object.
#'
#' @param seu A Seurat object. The Seurat object should contain pre-computed cell embeddings for dimensional reduction and expression data for genes.
#' @param adjm A spatial adjacency matrix that represents relationships between cells or spots in the spatial transcriptomic data. This matrix is used to calculate the gene embeddings.
#' @param reduction.name A character string specifying the name of the dimensional reduction method used for the cell embeddings (e.g., "ncfm", "caesar", etc.). The computed gene embeddings will be added to this slot. Default is "caesar".
#' @param assay A character string specifying which assay to use from the Seurat object. If NULL, the function will use the default assay set in the Seurat object.
#'
#' @return The modified Seurat object with the computed gene embeddings added to the specified dimensional reduction (`reduction.name`).
#'
#' @importFrom Seurat GetAssayData Embeddings DefaultAssay CreateDimReducObject
#' @importFrom Matrix as.matrix
#' @importFrom methods is
#' @import Rcpp
#' @useDynLib CAESAR.Suite, .registration = TRUE
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' pos <- toydata$pos
#' 
#' adjm <- ProFAST::AddAdj(as.matrix(pos), radius.upper = 200)
#' seu <- add.gene.embedding(
#'     seu = seu,
#'     adjm = adjm,
#'     reduction.name = "caesar",
#'     assay = "RNA"
#' )
#' print(seu)
#'
#' @export
add.gene.embedding <- function(
    seu, adjm, reduction.name = "caesar", assay = NULL) {
    if (is.null(assay)) {
        assay <- Seurat::DefaultAssay(seu)
    }

    if (!inherits(seu, "Seurat")) {
        stop("Input 'seu' must be a Seurat object.")
    }

    if (!is(adjm, "sparseMatrix")) {
        stop("Input 'adjm' must be a sparse matrix.")
    }

    X_data <- as.matrix(Seurat::GetAssayData(
        object = seu, slot = "data", assay = assay
    ))

    cellsCoordinates <- Seurat::Embeddings(seu, reduction.name)
    featuresCoordinates <- gene_embed_weight_cpp(as.matrix(X_data), cellsCoordinates, adjm)
    row.names(featuresCoordinates) <- row.names(X_data)
    feature_names <- intersect(row.names(featuresCoordinates), rownames(seu))

    seu@reductions[[reduction.name]] <- Seurat::CreateDimReducObject(
        embeddings = cellsCoordinates[colnames(seu), ],
        loadings = featuresCoordinates[feature_names, ],
        key = paste0(gsub("_", "", reduction.name), "_"),
        assay = assay
    )

    return(seu)
}




## co-embedding without image information



#' Compute Spatial-Aware Cell Embeddings
#' @description
#' This function computes low-dimensional cell embeddings from a gene-by-cell matrix. The method initializes cell embeddings using approximate PCA and refines them through a linear factor model nested a intrinsical conditional autoregressive model.
#'
#' @param X A gene-by-cell matrix (e.g., the `data` slot from a Seurat object) that serves as the input data for dimensional reduction.
#' @param adjm A spatial adjacency matrix representing the relationships between cells or spots in spatial transcriptomic data.
#' @param q An integer specifying the number of dimensions to reduce to. Default is 50.
#' @param reduction.name A character string specifying the name of the dimensional reduction method. Default is 'caesar'.
#' @param ... Additional parameters passed to `ProFAST::FAST_run`.
#'
#' @return A matrix containing the computed cell embeddings. The number of rows corresponds to the number of cells, and the number of columns corresponds to the specified number of dimensions (`q`).
#'
#' @seealso
#' \code{\link[ProFAST]{FAST_run}} for the main FAST dimensionality reduction algorithm.
#'
#' @importFrom Matrix t
#' @importFrom ProFAST FAST_run
#' @useDynLib CAESAR.Suite, .registration = TRUE
#' 
#' @export 
#' @examples 
#' data(toydata)
#' 
#' seu <- toydata$seu
#' pos <- toydata$pos
#' 
#' adjm <- ProFAST::AddAdj(as.matrix(pos), radius.upper = 200)
#' X <- Seurat::GetAssayData(object = seu, slot = "data", assay = "RNA")
#' cellembedding <- cellembedding_matrix(
#'     X = X,
#'     adjm = adjm
#' )
#' print(cellembedding[1:3, 1:3])
cellembedding_matrix <- function(
    X, adjm, q = 50, reduction.name = "caesar", ...) {
    reslist <- ProFAST::FAST_run(
        XList = list(Matrix::t(X)),
        AdjList = list(adjm),
        q = q, fit.model = "gaussian", ...
    )

    ce_cell <- reslist$hV[[1]]
    component <- paste0(reduction.name, "_", seq_len(ncol(ce_cell)))
    colnames(ce_cell) <- component
    row.names(ce_cell) <- colnames(X)

    return(ce_cell)
}








#' Perform CAESAR embedding of Cells Using FAST with Spatial Weights
#'
#' @description
#' This function computes cell embedding using the CAESAR framework with FAST for dimensionality reduction and spatial adjacency weights. It integrates variable feature selection and spatial adjacency information to generate low-dimensional representations for cells.
#'
#' @param seu A Seurat object. The Seurat object should contain gene expression data and be preprocessed with variable features identified.
#' @param adjm A spatial adjacency matrix representing the relationships between cells or spots in spatial transcriptomic data.
#' @param assay A character string specifying which assay to use from the Seurat object. If NULL, the function will use the default assay.
#' @param slot The data slot to use for feature extraction (e.g., "data", "counts"). Default is "data".
#' @param nfeatures The number of features to select for analysis. Default is 2000.
#' @param q An integer specifying the number of dimensions for the reduced embeddings. Default is 50.
#' @param reduction.name A character string specifying the name for the dimensional reduction. Default is "caesar".
#' @param var.features A vector of variable features (genes) to use for the embedding. If NULL, the function will use variable features stored in the Seurat object.
#' @param ... Additional arguments passed to `FAST_run`.
#'
#' @return The modified Seurat object with the co-embedding results (cell and gene embeddings) stored in the specified dimensional reduction slot.
#'
#' @seealso
#' \code{\link[ProFAST]{FAST_run}} for the main FAST dimensionality reduction algorithm.
#'
#' @importFrom Seurat GetAssayData DefaultAssay CreateDimReducObject
#' @importFrom Matrix as.matrix
#' @useDynLib CAESAR.Suite, .registration = TRUE
#' 
#' @export 
#' @examples 
#' data(toydata)
#' 
#' seu <- toydata$seu
#' pos <- toydata$pos
#' 
#' adjm <- ProFAST::AddAdj(as.matrix(pos), radius.upper = 200)
#' seu <- cellembedding_seurat(
#'     seu = seu,
#'     adjm = adjm
#' )
#' print(seu)
cellembedding_seurat <- function(
    seu, adjm, assay = NULL, slot = "data", nfeatures = 2000, q = 50,
    reduction.name = "caesar", var.features = NULL, ...) {
    # Use default assay if not provided
    if (is.null(assay)) {
        assay <- Seurat::DefaultAssay(seu)
    }

    # Start timing the process
    tstart <- Sys.time()

    X_all <- Seurat::GetAssayData(object = seu, slot = slot, assay = assay)

    var.fe.tmp <- get_varfeature_fromSeurat(seu, assay = assay)
    if (is.null(var.features)) {
        if (length(var.fe.tmp) == 0) {
            stop("NCFM: please find the variable features using Seurat::FindVariableFeatures or DR.SC::FindSVGs before running this function!")
        }
        var.features <- var.fe.tmp
    } else {
        var.features <- intersect(var.features, rownames(X_all))
    }

    cellsCoordinates <- cellembedding_matrix(
        X = X_all[var.features, ], adjm = adjm, q = q,
        reduction.name = reduction.name, ...
    )

    seu@reductions[[reduction.name]] <- Seurat::CreateDimReducObject(
        embeddings = cellsCoordinates,
        key = paste0(reduction.name, "_"), assay = assay
    )

    .logDiffTime(
        sprintf(paste0("%s Finish calculate CAESAR embedding"), "*****"),
        t1 = tstart, verbose = TRUE
    )

    return(seu)
}




#' Compute Co-embedding Using CAESAR
#'
#' @description
#' This function performs co-embedding of both cells and genes using the CAESAR method. It integrates spatial transcriptomics data from a Seurat object (`seu`) with a spatial adjacency matrix to compute the low-dimensional co-embedding.
#'
#' @param seu A Seurat object containing spatial transcriptomics data.
#' @param pos A matrix of spatial coordinates for the spots (e.g., spatial positions of cells or pixels in the image). The row names of `pos` should match the column names of `seu`.
#' @param reduction.name A character string specifying the name of the dimensional reduction method to store in the Seurat object. Default is "caesar".
#' @param q An integer specifying the number of dimensions for the reduced co-embeddings. Default is 50.
#' @param radius.upper A numeric value specifying the upper limit of the search radius for the spatial adjacency matrix. Default is 400.
#' @param ... Additional arguments passed to `cellembedding_image_seurat`.
#'
#' @return The modified Seurat object with the computed cell and gene embeddings stored in the specified reduction slot.
#'
#' @seealso
#' \code{\link{cellembedding_seurat}} for computing cell embeddings.
#' \code{\link{add.gene.embedding}} for adding gene embeddings to a Seurat object.
#'
#' @importFrom stats dist median
#' @importFrom ProFAST AddAdj
#' @export
#' 
#' @examples 
#' data(toydata)
#' 
#' seu <- toydata$seu
#' pos <- toydata$pos
#' 
#' seu <- CAESAR.coembedding(
#'     seu = seu,
#'     pos = pos
#' )
#' print(seu)
CAESAR.coembedding <- function(
    seu, pos, reduction.name = "caesar", q = 50, radius.upper = 400, ...) {
    if (!inherits(seu, "Seurat")) {
        stop("Input 'seu' must be a Seurat object.")
    }

    if (!is.matrix(pos) && !is.data.frame(pos)) {
        stop("Input 'pos' must be a matrix or a data.frame.")
    }

    if (!all(rownames(pos) %in% colnames(seu))) {
        stop("Row names of 'pos' must match the column names of 'seu'.")
    }

    if (!is.numeric(q) || q <= 0 || q %% 1 != 0) {
        stop("'q' must be a positive integer value.")
    }

    if (!is.numeric(radius.upper) || radius.upper <= 0) {
        stop("'radius.upper' must be a positive numeric value.")
    }

    pos <- as.matrix(pos)

    adjm <- ProFAST::AddAdj(pos, radius.upper = radius.upper)
    seu <- cellembedding_seurat(
        seu,
        adjm = adjm, q = q, reduction.name = reduction.name, ...
    )
    seu <- add.gene.embedding(seu, adjm, reduction.name)

    return(seu)
}
