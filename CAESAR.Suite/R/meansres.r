



#' Calculate Area Under the Curve (AUC) for Pathway Scores
#'
#' @description
#' This function calculates the area under the curve (AUC) for pathway scores with respect to a specific cell type. It uses the AUC to evaluate the performance of the pathway scores in distinguishing the target cell type from others.
#'
#' @param celltype A factor or character vector representing the cell type labels for each cell.
#' @param pathway.scores A matrix of pathway scores where rows represent cells and columns represent different pathways.
#' @param return.mean Logical, indicating whether to return the weighted mean AUC across all cell types. If \code{FALSE}, returns the AUC for each pathway. Default is \code{TRUE}.
#' @param seed An integer specifying the random seed for reproducibility. Default is 1.
#'
#' @return If \code{return.mean = TRUE}, a numeric vector containing the weighted mean AUC. If \code{return.mean = FALSE}, a numeric matrix of AUCs where rows represent pathways and columns represent cell types.
#'
#' @seealso \code{\link[DescTools]{AUC}} for the AUC calculation.
#'
#' @examples
#' # Example usage:
#' celltype <- factor(rep(letters[1:5], each = 20))
#' pathway.scores <- matrix(runif(1000), nrow = 100, ncol = 10)
#' colnames(pathway.scores) <- letters[1:10]
#' auc_values <- auc(celltype, pathway.scores, return.mean = TRUE)
#' print(auc_values)
#'
#' @importFrom DescTools AUC
#' @export
auc <- function(celltype, pathway.scores, return.mean = TRUE, seed = 1) {

    # Check if 'celltype' is a factor or character vector
    if (!is.factor(celltype) && !is.character(celltype)) {
        stop("'celltype' must be a factor or character vector representing cell type labels.")
    }

    # Check if 'pathway.scores' is a matrix
    if (!is.matrix(pathway.scores)) {
        stop("'pathway.scores' must be a matrix with cells as rows and pathways as columns.")
    }

    # Ensure the number of rows in 'pathway.scores' matches the length of 'celltype'
    if (nrow(pathway.scores) != length(celltype)) {
        stop("The number of rows in 'pathway.scores' must match the length of 'celltype'.")
    }

    y <- as.character(celltype)
    cts <- colnames(pathway.scores)
    n <- length(y)

    # Shuffle the order of cells randomly
    set.seed(seed)
    id <- sample(seq_len(n), n)
    y <- y[id]
    pathway.scores <- pathway.scores[id, ]

    # Calculate AUC for each pathway
    res <- sapply(cts, function(ct) {
        n_ct <- sum(y == ct)
        if (n_ct == 0) {
            return(0)
        }
        y0 <- seq_len(n) / n
        x <- cumsum(y[order(pathway.scores[, ct], decreasing = TRUE)] == ct) / n_ct
        DescTools::AUC(y0, x)
    })

    if (return.mean) {
        # Calculate weighted mean AUC across all cell types
        weight <- table(y)[cts]
        id <- which((!is.na(weight)) & (weight != 0))
        weight <- weight[id] / sum(weight[id])
        return(as.vector(res[id] %*% weight))
    } else {
        return(res)
    }
}



#' Calculate Signature Score for Cell Clusters
#'
#' @description
#' This function calculates a signature score for cell clusters based on the embeddings of cells and genes in a Seurat object. The score measures how well the genes linked to specific clusters are separated in the embedding space. The function also supports returning a weighted mean score across all clusters.
#'
#' @param seu A Seurat object containing the single-cell RNA-seq data.
#' @param reduction A character string specifying the name of the dimensional reduction to use (e.g., "caesar").
#' @param label A character string specifying the column name in the Seurat object's metadata that contains the cell type or cluster labels.
#' @param gclink A data frame with two columns: `gene` and `cluster`, where `gene` corresponds to gene names and `cluster` corresponds to the associated cell cluster.
#' @param return.mean Logical, indicating whether to return the weighted mean signature score across all clusters. If \code{FALSE}, returns the score for each gene-cluster pair. Default is \code{TRUE}.
#'
#' @details
#' The function computes the distance between gene embeddings and cell embeddings using a custom distance metric. It then calculates a score for each gene-cluster pair by evaluating how well the genes associated with a specific cluster are ordered in proximity to the cells of that cluster. The score is normalized between 0 and 1, where 1 indicates perfect separation.
#'
#' @return If \code{return.mean = TRUE}, a numeric value representing the weighted mean signature score across all clusters. If \code{return.mean = FALSE}, a numeric vector containing the signature score for each gene-cluster pair.
#'
#' @examples
#' library(Seurat)
#' data(toydata)
#' 
#' seu <- toydata$seu
#' 
#' seu$cluster <- Idents(seu)
#' 
#' gclink <- data.frame(
#'     gene = c(
#'         "FBLN1", "CCDC80", "LYPD3", "MLPH", "HOXD9", "EGFL7",
#'         "HAVCR2", "IGSF6", "KRT5", "KRT6B", "CD79A", "DERL3",
#'         "CAV1", "AVPR1A", "CD3G", "CD3D"
#'     ),
#'     cluster = c(
#'         "CAFs", "CAFs", "Cancer Epithelial", "Cancer Epithelial",
#'         "Endothelial", "Endothelial", "Myeloid", "Myeloid",
#'         "Normal Epithelial", "Normal Epithelial", "Plasmablasts",
#'         "Plasmablasts", "PVL", "PVL", "T-cells", "T-cells"
#'     )
#' )
#' 
#' score <- SigScore(
#'     seu, reduction = "caesar", label = "cluster", gclink = gclink,
#'     return.mean = TRUE
#' )
#' print(score)
#' 
#' score <- SigScore(
#'     seu, reduction = "caesar", label = "cluster",
#'     gclink = gclink, return.mean = FALSE
#' )
#' print(score)
#'
#' @importFrom Seurat Embeddings Loadings
#' @export
SigScore <- function(seu, reduction, label, gclink, return.mean = TRUE) {

    # Check if 'seu' is a valid Seurat object
    if (!inherits(seu, "Seurat")) {
        stop("Input 'seu' must be a Seurat object.")
    }

    # Check if 'reduction' is a character string
    if (!is.character(reduction) || length(reduction) != 1) {
        stop("'reduction' must be a single character string specifying the dimensional reduction to use.")
    }

    # Check if 'label' is a valid column in the Seurat object's metadata
    if (!is.element(label, colnames(seu@meta.data))) {
        stop("'label' must be one of the column names in meta.data.")
    }

    # Check if 'gclink' is a data frame with the required columns
    if (!is.data.frame(gclink) || !all(c("gene", "cluster") %in% colnames(gclink))) {
        stop("'gclink' must be a data frame with 'gene' and 'cluster' columns.")
    }

    # Extract cell type labels
    y <- as.character(seu@meta.data[, label])
    names(y) <- colnames(seu)

    # Extract cell embeddings
    cebd <- Seurat::Embeddings(seu, reduction)

    # Extract gene embeddings
    if (!all(gclink$gene %in% rownames(Seurat::Loadings(seu, reduction)))) {
        stop("Some genes in 'gclink' are not present in the loadings of the specified reduction.")
    }
    gebd <- Seurat::Loadings(seu, reduction)[gclink$gene, ]

    # Calculate the distance between gene embeddings and cell embeddings
    dist <- pdistance.matrix(gebd, cebd)
    nc <- nrow(gclink)

    # Calculate signature scores for each gene-cluster pair
    score_ct <- sapply(seq_len(nc), function(i) {
        ct <- gclink$cluster[i]
        y0 <- y[order(dist[i, ])]
        score <- sum(which(y0 == ct))
        nct <- sum(y == ct)
        n <- length(y)
        score.min <- sum(seq_len(nct))
        score.max <- (n + 1) * nct - score.min
        score <- 1 - (score - score.min) / (score.max - score.min)
        return(score)
    })

    # Return either the mean score or all scores
    if (return.mean) {
        weight <- table(y)[as.character(gclink$cluster)]
        id <- which((!is.na(weight)) & (weight != 0))
        weight <- weight[id] / sum(weight[id])
        return(score_ct[id] %*% weight)
    } else {
        return(score_ct)
    }
}




#' Calculate Accuracy of Predicted Cell Types
#'
#' @description
#' This function calculates the accuracy of predicted cell types by comparing the predicted labels to the true labels. It allows for the exclusion of certain cell types from the reference when calculating accuracy.
#'
#' @param y.predict A character vector representing the predicted cell type labels.
#' @param y.truth A character vector representing the true cell type labels.
#' @param cts_notin_reference A character vector specifying the cell types that should be excluded from the accuracy calculation. If \code{NULL}, all cell types are included. Default is \code{NULL}.
#'
#' @details
#' The function compares the predicted labels (`y.predict`) to the true labels (`y.truth`) and calculates the proportion of correct predictions. If `cts_notin_reference` is provided, cell types in this vector are excluded from the accuracy calculation.
#'
#' @return A numeric value representing the accuracy, i.e., the proportion of correctly predicted labels among the considered cell types.
#'
#' @examples
#' # Example usage:
#' y.predict <- c("A", "B", "A", "C", "A", "B")
#' y.truth <- c("A", "B", "C", "C", "A", "A")
#' acc_value <- acc(y.predict, y.truth)
#' print(acc_value)
#'
#' @export
acc <- function(y.predict, y.truth, cts_notin_reference = NULL) {

    # Ensure y.predict and y.truth are character vectors
    y.predict <- as.character(y.predict)
    y.truth <- as.character(y.truth)

    # Ensure y.predict and y.truth have the same length
    if (length(y.predict) != length(y.truth)) {
        stop("y.predict and y.truth must have the same length.")
    }

    # Identify the indices to include in the accuracy calculation
    if (!is.null(cts_notin_reference)) {
        id <- which(!(y.truth %in% cts_notin_reference))
    } else {
        id <- seq_along(y.truth)
    }

    # Calculate and return the accuracy
    return(mean(y.predict[id] == y.truth[id]))
}


