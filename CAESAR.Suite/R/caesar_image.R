################################################################################
# CAESAR coembedding with image
################################################################################
## Auxiliary functions

# Normalize a Matrix by Row Sums
scaleMatrix_byrow <- function(adj) {
    s <- Matrix::rowSums(adj) + 1e-08
    n <- length(s)
    adj_norm <- adj / matrix(s, nrow = n, ncol = n, byrow = FALSE)
    return(adj_norm)
}


# Search for a Radius that Satisfies Requirements
#' @importFrom Matrix rowSums
#' @importFrom stats dist
searchRadius <- function(pos, lower.med = 8, upper.med = 10, radius.upper = NULL, seed = 1) {
    if (!inherits(pos, "matrix")) {
        stop("method is only for matrix object!")
    }

    set.seed(seed)
    n_spots <- nrow(pos)
    idx <- sample(n_spots, min(1000, n_spots))
    dis <- dist(pos[idx, ])
    if (is.null(radius.upper)) {
        radius.upper <- sort(dis)[50]
    }

    radius.lower <- min(dis[dis > 0])
    Adj_sp <- getneighborhood_fastcpp(pos, radius = radius.upper)
    Med <- summary(Matrix::rowSums(Adj_sp))["Median"]

    if (Med < lower.med) {
        stop("The radius.upper is too small and cannot find median neighbors greater than ", lower.med)
    }

    message("Find the adjacency matrix by bisection method...")
    start.radius <- 1
    Med <- 0
    maxIter <- 30
    k <- 1

    # Use bisection method to adjust the radius
    while (!(Med >= lower.med && Med <= upper.med)) {
        Adj_sp <- getneighborhood_fastcpp(pos, radius = start.radius)
        Med <- summary(Matrix::rowSums(Adj_sp))["Median"]

        if (Med < lower.med) {
            radius.lower <- start.radius
            start.radius <- (radius.lower + radius.upper) / 2
        } else if (Med > upper.med) {
            radius.upper <- start.radius
            start.radius <- (radius.lower + radius.upper) / 2
        }

        message("Current radius is ", round(start.radius, 2))
        message("Median of neighborhoods is ", Med)

        if (k > maxIter) {
            message("Reached the maximum iteration but could not find a proper radius!")
            break
        }

        k <- k + 1
    }

    return(start.radius)
}




## co-embedding with image information

#' Compute Spatial-Aware Cell Embeddings with Image Information
#'
#' @description
#' This function computes low-dimensional cell embeddings from a gene-by-cell matrix. The method initializes cell embeddings using approximate PCA and refines them through a linear factor model nested a intrinsical conditional autoregressive model.
#'
#' @param X A gene-by-cell matrix (e.g., the `data` slot from a Seurat object) that serves as the input data for dimensional reduction.
#' @param adjm A spatial adjacency matrix representing relationships between cells or spots.
#' @param q An integer specifying the number of dimensions for the reduced embeddings. Default is 50.
#' @param reduction.name A character string specifying the name of the dimensional reduction method. Default is 'caesar'.
#' @param maxIter Maximum number of iterations for the optimization algorithm. Default is 30.
#' @param epsELBO A small number specifying the convergence threshold for the optimization algorithm. Default is 1e-6.
#' @param approx_Phi Logical, indicating whether to use the approximate method for Phi matrix estimation. Default is FALSE.
#' @param verbose Logical, indicating whether to print progress messages. Default is TRUE.
#' @param Phi_diag Logical, indicating whether to constrain the Phi matrix to be diagonal. Default is TRUE.
#' @param seed An integer used to set the random seed for reproducibility. Default is 1.
#'
#' @return A matrix containing the computed cell embeddings. The number of rows corresponds to the number of cells, and the number of columns corresponds to the specified number of dimensions (`q`).
#'
#' @importFrom Matrix rowSums t colMeans
#' @importFrom irlba irlba
cellembedding_image_matrix <- function(
    X, adjm, q = 50, reduction.name = "caesar", maxIter = 30,
    epsELBO = 1e-6, approx_Phi = FALSE, verbose = TRUE, Phi_diag = TRUE,
    seed = 1) {
    X <- Matrix::t(X)
    message("Step into function")

    Diag <- function(vec) {
        q <- length(vec)
        if (q > 1) {
            y <- diag(vec)
        } else {
            y <- matrix(vec, 1, 1)
        }
        return(y)
    }

    approxPCA <- function(X, q) {
        n <- nrow(X)
        svdX <- irlba::irlba(A = X, nv = q)
        PCs <- svdX$u %*% Diag(svdX$d[1:q])
        loadings <- svdX$v
        dX <- PCs %*% t(loadings) - X
        Lam_vec <- colSums(dX^2) / n
        return(list(PCs = PCs, loadings = loadings, Lam_vec = Lam_vec))
    }

    n <- nrow(X)
    mu_int <- colMeans(X)
    message("Centering X")
    X_center <- scale(X, scale = FALSE)
    message("Calculate initial values using PCA")

    set.seed(seed)
    princ <- approxPCA(X_center, q = q)
    rm(X_center)

    M_int <- princ$PCs
    B_int <- princ$loadings
    Lam_int <- princ$Lam_vec
    rm(princ)

    Phi_int <- diag(rep(1, q))
    if (!approx_Phi) {
        w_plus <- Matrix::rowSums(adjm)
        w_plus[w_plus == 0] <- 1
        R_int <- array(dim = c(q, q, n))
        for (i in 1:n) R_int[, , i] <- diag(rep(1, q))
        res <- imFactorCpp(X, adjm, w_plus, mu_int, B_int, Lam_int, Phi_int,
            M_int, R_int, maxIter, epsELBO, verbose,
            Phi_diag = TRUE
        )
    } else {
        R_int <- Phi_int
        res <- approxPhi_imFactorCpp(X, adjm, mu_int, B_int, Lam_int, Phi_int,
            M_int, R_int, maxIter, epsELBO, verbose,
            Phi_diag = TRUE
        )
    }

    ce_cell <- as.matrix(scale(res$M, scale = FALSE))

    component <- paste0(reduction.name, "_", seq_len(ncol(ce_cell)))
    colnames(ce_cell) <- component
    row.names(ce_cell) <- row.names(X)

    return(ce_cell)
}




#' Compute Spatial-Aware Cell Embeddings with Image Information
#'
#' @description
#' This function computes low-dimensional cell embeddings from a seyrat object. The method initializes cell embeddings using approximate PCA and refines them through a linear factor model nested a intrinsical conditional autoregressive model.
#'
#' @param seu A Seurat object containing gene expression data. The object should have variable features identified prior to running this function.
#' @param adjm A spatial adjacency matrix representing relationships between cells or spots.
#' @param assay A character string specifying which assay to use from the Seurat object. If NULL, the function will use the default assay set in the Seurat object.
#' @param slot The data slot to use for feature extraction (e.g., "data", "scale.data"). Default is "data".
#' @param q An integer specifying the number of dimensions for the reduced embeddings. Default is 10.
#' @param approx_Phi Logical, indicating whether to use an approximate method for estimating the Phi matrix. Default is FALSE.
#' @param reduction.name A character string specifying the name for the dimensional reduction result. Default is "caesar".
#' @param var.features A vector of variable features (genes) to use for the analysis. If NULL, the function will automatically use the variable features stored in the Seurat object.
#' @param ... Additional arguments passed to `cellembedding_image_matrix`.
#'
#' @return The modified Seurat object with the cell embedding results stored in the specified dimensional reduction slot.
#'
#' @seealso
#' \code{\link{cellembedding_image_matrix}} for additional arguments used to compute cell embeddings.
#'
#' @importFrom Seurat GetAssayData DefaultAssay CreateDimReducObject
cellembedding_image_seurat <- function(
    seu, adjm, assay = NULL, slot = "data", q = 10, approx_Phi = FALSE,
    reduction.name = "caesar", var.features = NULL, ...) {
    if (is.null(assay)) {
        assay <- Seurat::DefaultAssay(seu)
    }

    # Start timing the process
    tstart <- Sys.time()

    X_all <- as.matrix(Seurat::GetAssayData(object = seu, slot = slot, assay = assay))

    var.fe.tmp <- get_varfeature_fromSeurat(seu, assay = assay)
    if (is.null(var.features)) {
        if (length(var.fe.tmp) == 0) {
            stop("cellembedding_image_seurat: please find the variable features using Seurat::FindVariableFeatures or DR.SC::FindSVGs before running this function!")
        }
        var.features <- var.fe.tmp
        rm(var.fe.tmp)
    } else {
        var.features <- intersect(var.features, rownames(X_all))
    }

    cellsCoordinates <- cellembedding_image_matrix(
        X = X_all[var.features, ], adjm = adjm,
        reduction.name = reduction.name, q = q, approx_Phi = approx_Phi, ...
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

#' Compute Co-embedding with Image Information Using CAESAR
#'
#' @description
#' This function performs co-embedding of both cells and genes using the CAESAR method. It integrates spatial transcriptomics data from a Seurat object (`seu`) with image features (`feature_img`) and a spatial adjacency matrix to compute the low-dimensional co-embedding.
#'
#' @param seu A Seurat object containing spatial transcriptomics data.
#' @param feature_img A matrix representing features extracted from a histology image using a Visual Transformer. Rows correspond to spots and columns represent image features. The row names of `feature_img` should match the column names of `seu`.
#' @param pos A matrix of spatial coordinates for the spots (e.g., spatial positions of cells or pixels in the image). The row names of `pos` should match the column names of `seu`.
#' @param reduction.name A character string specifying the name of the dimensional reduction method to store in the Seurat object. Default is "caesar".
#' @param q An integer specifying the number of dimensions for the reduced co-embeddings. Default is 50.
#' @param lower.med A numeric value specifying the lower bound for the desired median number of neighbors in the spatial adjacency matrix. Default is 3.5.
#' @param upper.med A numeric value specifying the upper bound for the desired median number of neighbors in the spatial adjacency matrix. Default is 5.5.
#' @param radius.upper A numeric value specifying the upper limit of the search radius for the spatial adjacency matrix. Default is 400.
#' @param q.image An integer specifying the number of dimensions for the reduced image embeddings. Default is 10.
#' @param weighted Logical, indicating whether to apply weighted PCA on the image features. Default is FALSE.
#' @param approx_Phi Logical, indicating whether to use an approximate method for Phi matrix estimation. Default is TRUE.
#' @param seed An integer used to set the random seed for reproducibility. Default is 1.
#' @param ... Additional arguments passed to `cellembedding_image_seurat`.
#'
#' @return The modified Seurat object with the computed cell and gene embeddings stored in the specified reduction slot.
#'
#' @seealso
#' \code{\link{cellembedding_image_seurat}} for computing cell embeddings.
#' \code{\link{add.gene.embedding}} for adding gene embeddings to a Seurat object.
#'
#' @importFrom stats dist median
#' @export
#' @examples 
#' data(toydata)
#' 
#' seu <- toydata$seu
#' pos <- toydata$pos
#' imgf <- toydata$imgf
#' 
#' seu <- CAESAR.coembedding.image(seu, imgf, pos)
#' print(seu)
CAESAR.coembedding.image <- function(
    seu, feature_img, pos, reduction.name = "caesar", q = 50, lower.med = 3.5,
    upper.med = 5.5, radius.upper = 400, q.image = 10, weighted = FALSE,
    approx_Phi = TRUE, seed = 1, ...) {
    
    if (!inherits(seu, "Seurat")) {
        stop("Input 'seu' must be a Seurat object.")
    }

    if (!is.matrix(feature_img) && !is.data.frame(feature_img)) {
        stop("Input 'feature_img' must be a matrix or a data.frame.")
    }

    if (nrow(feature_img) != ncol(seu)) {
        stop("The number of rows of 'feature_img' must the same as the number of columns of 'seu'.")
    }

    if (!all(rownames(feature_img) %in% colnames(seu))) {
        stop("Row names of 'feature_img' must match the column names of 'seu'.")
    }

    if (!is.matrix(pos) && !is.data.frame(pos)) {
        stop("Input 'pos' must be a matrix or a data.frame.")
    }

    if (nrow(pos) != nrow(feature_img)) {
        stop("'pos' and 'feature_img' must have the same number of rows.")
    }

    if (!all(rownames(pos) %in% colnames(seu))) {
        stop("Row names of 'pos' must match the column names of 'seu'.")
    }

    if (!is.numeric(q) || q <= 0 || q %% 1 != 0) {
        stop("'q' must be a positive integer value.")
    }
    if (!is.numeric(q.image) || q.image <= 0 || q.image %% 1 != 0) {
        stop("'q.image' must be a positive integer value.")
    }
    if (!is.numeric(lower.med) || lower.med <= 0) {
        stop("'lower.med' must be a positive numeric value.")
    }
    if (!is.numeric(upper.med) || upper.med <= 0) {
        stop("'upper.med' must be a positive numeric value.")
    }
    if (!is.numeric(radius.upper) || radius.upper <= 0) {
        stop("'radius.upper' must be a positive numeric value.")
    }

    if (!is.logical(weighted)) {
        stop("'weighted' must be a logical value (TRUE or FALSE).")
    }
    if (!is.logical(approx_Phi)) {
        stop("'approx_Phi' must be a logical value (TRUE or FALSE).")
    }

    feature_img <- as.matrix(feature_img)
    pos <- as.matrix(pos)

    prin <- wpca(feature_img, q = q.image, weighted = weighted)
    embed_img <- prin$PCs
    
    radius_use <- searchRadius(
        pos,
        lower.med = lower.med, upper.med = upper.med,
        radius.upper = radius.upper, seed = seed
    )

    set.seed(seed)
    n_spots <- ncol(seu)
    idx <- sample(n_spots, min(100, n_spots))
    dis <- dist(embed_img[idx, ])
    sigma <- median(dis)^2

    adjm <- weightAdj(pos, img_embed = embed_img, radius = radius_use, width = sigma)
    seu <- cellembedding_image_seurat(
        seu, adjm = adjm, q = q, approx_Phi = approx_Phi,
        reduction.name = reduction.name, ...
    )

    adj <- ProFAST::AddAdj(pos, radius.upper = radius.upper)
    seu <- add.gene.embedding(seu, adj, reduction.name)

    return(seu)
}

