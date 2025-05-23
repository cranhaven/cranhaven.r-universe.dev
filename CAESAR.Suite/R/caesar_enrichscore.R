#' Calculate Spot Level Enrichment Scores for Pathways Using CAESAR
#'
#' @description
#' This function calculates spot level enrichment scores for a list of pathways based on a cell-gene distance matrix in a Seurat object. The function uses a permutation-based approach to determine the significance of the enrichment scores.
#'
#' @param seu A Seurat object containing the gene expression data.
#' @param pathwaylist A list of pathways, where each pathway is represented by a vector of genes.
#' @param assay.dist A character string specifying the assay that contains the distance matrix. Default is "distce".
#' @param reduction.name A character string specifying the reduction method to use if the distance matrix needs to be computed. Default is "caesar".
#' @param gene.use A character vector specifying which genes to use in the analysis. If \code{NULL}, all genes in the distance matrix will be used. Default is \code{NULL}.
#' @param n_fake An integer specifying the number of random permutations to generate for significance testing. Default is 1001.
#' @param seed An integer specifying the random seed for reproducibility. Default is 1.
#'
#' @return A matrix of enrichment scores with cells as rows and pathways as columns.
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' pathway_list <- toydata$pathway_list
#' 
#' enrich.score <- CAESAR.enrich.score(seu, pathway_list)
#' head(enrich.score)
#'
#' @importFrom Seurat GetAssayData Assays
#' @importFrom Matrix colSums
#' @export
CAESAR.enrich.score <- function(
    seu, pathwaylist, assay.dist = "distce", reduction.name = "caesar",
    gene.use = NULL, n_fake = 1001, seed = 1) {

    # Check if 'seu' is a valid Seurat object
    if (!inherits(seu, "Seurat")) {
        stop("Input 'seu' must be a Seurat object.")
    }

    # Check if 'pathwaylist' is a list
    if (!is.list(pathwaylist)) {
        stop("Input 'pathwaylist' must be a list.")
    }

    # Ensure that the assay containing the distance matrix exists or create it
    if (!(assay.dist %in% Seurat::Assays(seu))) {
        seu <- ProFAST::pdistance(
            seu,
            reduction = reduction.name, assay.name = assay.dist
        )
    }

    # Extract the distance matrix
    distce <- Seurat::GetAssayData(
        object = seu, slot = "data", assay = assay.dist
    )

    # Use the specified genes or all genes in the distance matrix
    if (is.null(gene.use)) {
        gene.use <- row.names(distce)
    } else {
        gene.use <- intersect(row.names(distce), gene.use)
        distce <- distce[gene.use, ]
    }

    # Filter pathway genes based on available genes
    pathwaylist <- lapply(pathwaylist, function(pathway) {
        intersect(pathway, gene.use)
    })

    # Number of genes per pathway
    n_pathway <- sapply(pathwaylist, length)
    n_max <- max(n_pathway)
    n_min <- min(n_pathway)
    n_path <- length(n_pathway)
    n_p <- nrow(distce)

    # Calculate pathway enrichment scores
    dat_anno <- sapply(pathwaylist, function(pathway) {
        colSums(distce[pathway, , drop = FALSE])
    })
    pathwayscore <- matrix(NA, nrow(dat_anno), ncol(dat_anno), dimnames = list(
        colnames(seu), names(pathwaylist)
    ))

    # Generate random permutations for significance testing
    id_n_fake <- sapply(seq_len(n_fake), function(i) {
        set.seed(i + seed)
        sample(seq_len(n_p), n_max)
    })

    message("There are ", n_path, " pathways. The largest pathway has ", n_max, " genes.")
    for (ii in n_min:n_max) {
        tic <- proc.time()
        if (ii == n_min) {
            dis.mat.sim <- t(Reduce(`+`, lapply(seq_len(n_min), function(i) {
                id <- id_n_fake[i, ]
                distce[id, , drop = FALSE]
            })))
        } else {
            dis.mat.sim <- dis.mat.sim + t(distce[id_n_fake[ii, ], , drop = FALSE])
        }
        id_pathway <- which(n_pathway == ii)
        if (length(id_pathway) == 0) next
        for (jj in id_pathway) {
            pathwayscore[, jj] <- rowMeans(dis.mat.sim - dat_anno[, jj] > 0)
        }
        toc <- proc.time()
        message("Pathways with ", ii, " genes finished, which includes ", length(id_pathway), " pathways, elapsed time is ", round(toc[3] - tic[3], 3), "s.")
    }

    return(pathwayscore)
}


#' Combine p-values Using the Cauchy Combination Method
#'
#' @description
#' This function combines multiple p-values using the Cauchy combination method. The method is particularly useful for combining dependent p-values and is known for being robust and powerful, especially in the context of small p-values. For details, see Liu and Xie (2020).
#' 
#' @references Liu, Y., & Xie, J. (2020). Cauchy combination test: a powerful test with analytic p-value calculation under arbitrary dependency structures. Journal of the American Statistical Association, 115(529), 393-402.
#'
#' @param pvals A numeric vector of p-values to be combined. The p-values should be between 0 and 1.
#' @param weight A numeric vector of weights corresponding to the p-values. If \code{NULL}, equal weights are assigned to all p-values. Default is \code{NULL}.
#'
#' @return A single combined p-value.
#'
#' @examples
#' # Example usage:
#' pvals <- c(0.01, 0.03, 0.05)
#' combined_pval <- Cauchy.Combination(pvals)
#' print(combined_pval)
#'
#' @export
Cauchy.Combination <- function(pvals, weight = NULL) {
    if (!is.numeric(pvals) || any(pvals < 0 | pvals > 1)) {
        stop("'pvals' must be a numeric vector with values between 0 and 1.")
    }

    if (is.null(weight)) {
        weight <- rep(1, length(pvals))
    }

    if (!is.numeric(weight) || length(weight) != length(pvals)) {
        stop("'weight' must be a numeric vector of the same length as 'pvals'.")
    }

    weight <- weight / sum(weight)
    combine_ts <- tan((0.5 - pvals) * pi) %*% weight
    combine_pvals <- 0.5 - atan(combine_ts) / pi

    return(combine_pvals)
}




#' Test Cell Type Differentially Enriched Pathways
#'
#' @description
#' This function tests whether specific pathways are differentially enriched in particular cell types with cell types stored in a Seurat object. The function applies the Wilcoxon rank-sum test to compare the pathway scores between cells of a given cell type and all other cells. It supports parallel computation to speed up the testing process.
#'
#' @param seu A Seurat object containing the single-cell RNA-seq data.
#' @param pathway_scores A matrix of pathway scores where rows represent cells and columns represent different pathways.
#' @param ident A character string specifying the column name in the Seurat object's metadata to use as the cell type labels. If \code{NULL}, the default identities (\code{Idents(seu)}) will be used. Default is \code{NULL}.
#' @param cts A character vector specifying the cell types to test. If \code{NULL}, the function will test all unique cell types present in \code{ident} or \code{Idents(seu)}. Default is \code{NULL}.
#' @param parallel Logical, indicating whether to run the computation in parallel. Default is \code{TRUE}.
#' @param ncores An integer specifying the number of cores to use for parallel computation. Default is 10.
#' @param seed An integer specifying the random seed for reproducibility in parallel computation. Default is 1.
#'
#' @return A matrix of p-values where rows represent the pathways and columns represent the tested cell types.
#'
#' @seealso
#' \code{\link[stats]{wilcox.test}} for the Wilcoxon rank-sum test.
#' \code{\link{CAESAR.enrich.score}} for spot level pathway enrich scores.
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' pathway_list <- toydata$pathway_list
#' cts <- levels(seu)[1:2]
#' 
#' enrich.score <- CAESAR.enrich.score(seu, pathway_list)
#' dep.pvals <- CAESAR.CTDEP(seu, enrich.score, cts = cts)
#' print(dep.pvals)
#'
#' @importFrom Seurat Idents
#' @importFrom stats wilcox.test
#' @importFrom future plan multicore
#' @importFrom furrr future_map furrr_options
#' @importFrom pbapply pbsapply
#' @export
CAESAR.CTDEP <- function(
    seu, pathway_scores, ident = NULL, cts = NULL,
    parallel = TRUE, ncores = 10, seed = 1) {

    # Check if 'seu' is a valid Seurat object
    if (!inherits(seu, "Seurat")) {
        stop("Input 'seu' must be a Seurat object.")
    }

    # Check if 'pathway_scores' is a matrix
    if (!is.matrix(pathway_scores)) {
        stop("'pathway_scores' must be a matrix with cells as rows and pathways as columns.")
    }

    # Ensure the number of rows in 'pathway_scores' matches the number of cells in 'seu'
    if (nrow(pathway_scores) != ncol(seu)) {
        stop("The number of rows in 'pathway_scores' must match the number of cells in 'seu'.")
    }

    # Retrieve the cell type labels from the Seurat object
    if (is.null(ident)) {
        y <- as.character(Seurat::Idents(seu))
    } else {
        # Ensure 'ident' is a valid column in the Seurat object's metadata
        if (!is.element(ident, colnames(seu@meta.data))) {
            stop("ident must be NULL or one of the column names in meta.data!")
        }
        y <- as.character(seu@meta.data[, ident])
    }

    # If 'cts' is NULL, use all unique cell types in 'y', sorted by name
    if (is.null(cts)) {
        cts <- sort(unique(y))
    }

    # Function to apply the Wilcoxon test
    wtest_fun <- function(i, ps, id1, id2) {
        x <- ps[, i]
        wilcox.test(x[id1], x[id2], alternative = "greater")$p.value
    }

    p <- ncol(pathway_scores)
    pvals_mat <- matrix(NA, p, length(cts), dimnames = list(
        colnames(pathway_scores), cts
    ))

    # Run the Wilcoxon test for each cell type
    for (ct in cts) {
        id1 <- which(y == ct)
        id2 <- which(y != ct)
        if (parallel) {
            # Set up parallel computation
            future::plan("multicore", workers = ncores)
            pvals <- furrr::future_map(
                1:p, wtest_fun,
                ps = pathway_scores, id1 = id1, id2 = id2,
                .progress = TRUE, .options = furrr::furrr_options(seed = seed)
            )
            pvals_mat[, ct] <- Reduce(c, pvals)
        } else {
            # Serial computation
            # pvals_mat[, ct] <- pbapply::pbsapply(
            #     1:p, wtest_fun,
            #     ps = pathway_scores, id1 = id1, id2 = id2
            # )

            if (is_interactive()) {
                pvals_mat[, ct] <- pbapply::pbsapply(
                    1:p, wtest_fun,
                    ps = pathway_scores, id1 = id1, id2 = id2
                )
            } else {
                pvals_mat[, ct] <- sapply(
                    1:p, wtest_fun,
                    ps = pathway_scores, id1 = id1, id2 = id2
                )
            }
        }
    }

    return(pvals_mat)
}







