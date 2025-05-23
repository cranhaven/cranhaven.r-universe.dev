#' Identify Signature Genes for Each Cell Type
#'
#' @description
#' This function identifies signature genes for each cell type or cell group in a Seurat object using a co-embedding distance-based approach. It computes the average expression and distance metrics for each gene across different groups, while also considering expression proportions.
#'
#' @param seu A Seurat object containing gene expression data.
#' @param distce.assay A character string specifying the assay that contains the distance matrix or distance-related data. Default is "distce".
#' @param ident A character string specifying the column name in the `meta.data` slot of the Seurat object used to define the identities (clusters or cell groups). If `NULL`, the default identities (`Idents(seu)`) will be used. Default is `NULL`.
#' @param expr.prop.cutoff A numeric value specifying the minimum proportion of cells that must express a gene for it to be considered. Default is 0.1.
#' @param assay A character string specifying the assay to use for expression data. If `NULL`, the default assay of the Seurat object will be used. Default is `NULL`.
#' @param genes.use A character vector specifying the genes to use for the analysis. If `NULL`, all genes in the `distce.assay` assay will be used. Default is `NULL`.
#'
#' @return A list where each element corresponds to a cell group and contains a data frame with the following columns:
#' \item{distance}{The mean distance of the gene across the cells in the group.}
#' \item{expr.prop}{The proportion of cells in the group expressing the gene.}
#' \item{expr.prop.others}{The proportion of cells in other groups expressing the gene.}
#' \item{label}{The identity label of the cell group.}
#' \item{gene}{The gene name.}
#'
#' @seealso None
#' 
#' @examples 
#' data(toydata)
#' 
#' seu <- toydata$seu
#' 
#' seu <- ProFAST::pdistance(seu, reduction = "caesar")
#' sglist <- find.sig.genes(
#'     seu = seu
#' )
#' str(sglist)
#'
#' @importFrom Seurat Idents DefaultAssay GetAssayData
#' @importFrom pbapply pblapply
#' @importFrom Matrix rowMeans rowSums
#' @export
find.sig.genes <- function(
    seu, distce.assay = "distce", ident = NULL, expr.prop.cutoff = 0.1,
    assay = NULL, genes.use = NULL) {
    if (is.null(ident)) {
        cell_label_vec <- Seurat::Idents(seu)
    } else {
        if (!is.element(ident, colnames(seu@meta.data))) {
            stop("ident must be NULL or one column name in meta.data!")
        }
        cell_label_vec <- seu@meta.data[, ident]
    }

    if (is.null(assay)) {
        assay <- Seurat::DefaultAssay(seu)
    }

    if (is.null(genes.use)) {
        genes.use <- rownames(seu@assays[[distce.assay]])
    }

    # Get the unique cell identities
    cell_ID <- sort(as.character(unique(cell_label_vec)))

    # Extract distance and expression data
    distce <- Seurat::GetAssayData(seu, assay = distce.assay, slot = "data")[genes.use, ]
    exp_data <- Seurat::GetAssayData(seu, assay = assay, slot = "data")
    expr.all <- Matrix::rowSums(
        exp_data[genes.use, , drop = FALSE] > 0
    )
    n <- ncol(seu)

    # Loop through each cell identity and calculate distance and expression proportions
    df_co_list <- pbapply::pblapply(cell_ID, function(x) {
        idx <- which(cell_label_vec == x) # Cells belonging to current identity
        n_idx <- length(idx)

        # Calculate expression proportion in the group and others
        genes.expr.prop <- Matrix::rowMeans(exp_data[genes.use, idx, drop = FALSE] > 0)
        genes.expr.prop.other <- (expr.all - genes.expr.prop * n_idx) / (n - n_idx)

        # Compute the mean distance for the genes
        cv.vec <- Matrix::rowMeans(distce[, idx, drop = FALSE])

        # Create a data frame of results for the current identity
        df <- data.frame(
            distance = cv.vec,
            expr.prop = genes.expr.prop,
            expr.prop.others = genes.expr.prop.other,
            label = x,
            gene = genes.use
        )
        row.names(df) <- genes.use
        df[order(df$distance, decreasing = FALSE), ] # Sort by distance (ascending)
    })

    names(df_co_list) <- cell_ID
    return(df_co_list)
}




#' Select Marker Genes from a signature gene list Based on Expression Proportion and Overlap Criteria
#'
#' @description
#' This function selects marker genes for each cluster or cell type based on expression proportion, with options to remove mitochondrial and ribosomal genes, limit the maximum number of top marker genes, and control the overlap between markers across clusters.
#'
#' @param ref_sig_list A list where each element corresponds to a cluster or cell type. Each element should be a data frame containing at least two columns: \code{gene} (the gene names) and \code{expr.prop} (the proportion of cells expressing each gene). Generally, it is the output of function \code{find.sig.genes}.
#' @param expr.prop.cutoff A numeric value specifying the minimum proportion of cells that must express a gene for it to be considered. Default is 0.1.
#' @param ntop.max An integer specifying the maximum number of top marker genes to be selected. Default is 200.
#' @param overlap.max An integer specifying the maximum allowable overlap of marker genes across clusters. If a gene appears in more than \code{overlap.max} clusters, it will be excluded. Default is 1.
#' @param rm_mito_ribo Logical, indicating whether to remove mitochondrial and ribosomal genes from the marker gene list. Default is \code{FALSE}.
#' @param species A character string specifying the species for mitochondrial and ribosomal gene detection. Options are "ms" for mouse or "hs" for human. Default is "ms".
#'
#' @return A list where each element corresponds to a cluster and contains the selected marker genes. If no markers are found, a message is printed and \code{NULL} is returned.
#'
#' @seealso
#' \code{\link{find.sig.genes}} for signature gene list.
#'
#' @examples 
#' data(toydata)
#' 
#' seu <- toydata$seu
#' 
#' seu <- ProFAST::pdistance(seu, reduction = "caesar")
#' sglist <- find.sig.genes(seu = seu)
#' 
#' markers <- marker.select(sglist, expr.prop.cutoff = 0.1, overlap.max = 1)
#' print(markers)
#'
#' @export
marker.select <- function(
    ref_sig_list, expr.prop.cutoff = 0.1, ntop.max = 200, overlap.max = 1,
    rm_mito_ribo = FALSE, species = "ms") {
    # Check for valid overlap.max value
    if (overlap.max < 1) {
        stop("overlap.max >= 1 is required!")
    }
    if (overlap.max >= length(ref_sig_list)) {
        stop("overlap.max must be less than the number of clusters!")
    }

    # Filter genes based on expression proportion and remove mitochondrial/ribosomal genes if needed
    ref_sig_list <- lapply(ref_sig_list, function(sig) {
        sig <- sig[sig$expr.prop > expr.prop.cutoff, ]
        if (rm_mito_ribo) {
            gene_vector <- sig$gene
            if (species == "ms") {
                mitochondrial_pattern <- "^(mt-|Mt-)"
                ribosomal_pattern <- "^(Rps|Rpl)"
            } else {
                mitochondrial_pattern <- "^MT-"
                ribosomal_pattern <- "^(RPS|RPL)"
            }

            # Identify mitochondrial and ribosomal genes
            mitochondrial_genes <- grep(mitochondrial_pattern, gene_vector, value = TRUE)
            ribosomal_genes <- grep(ribosomal_pattern, gene_vector, value = TRUE)

            # Remove mitochondrial and ribosomal genes
            filtered_gene_vector <- setdiff(gene_vector, union(mitochondrial_genes, ribosomal_genes))
            return(filtered_gene_vector)
        } else {
            return(sig$gene)
        }
    })

    # Initialize marker list
    markers.determined <- NULL

    # Iteratively select top markers with overlap control
    for (ntop in 1:ntop.max) {
        markers <- lapply(ref_sig_list, function(sig) {
            sig[seq_len(ntop)]
        })
        # Check for overlap in selected markers
        if (max(table(unlist(markers))) > overlap.max) break
        markers.determined <- markers
    }

    # Handle cases where no markers were found or maximum reached
    if (is.null(markers.determined)) {
        message("No marker was found under the given conditions, please relax the conditions.")
        return(NULL)
    }
    if (length(markers.determined[[1]]) == ntop.max) {
        message("The maximum number of marker genes has been reached. Increasing this upper bound may discover more markers.")
    }

    return(markers.determined)
}




#' Convert Marker List to a Weighted Matrix
#'
#' @description
#' This function converts a list of marker genes for different cell types or clusters into a matrix, where rows represent cell types and columns represent marker genes. The matrix contains the frequency of each marker gene across the different cell types.
#'
#' @param markerList A list where each element contains marker genes for different cell types or clusters. Each element of the list is a named list, where the names are the cell types and the values are the marker genes for that cell type. Generally, it is a list of the output of function \code{marker.select}.
#'
#' @return A matrix with rows representing cell types and columns representing unique marker genes. The values in the matrix represent the frequency of each gene as a marker in the corresponding cell type.
#'
#' @seealso
#' \code{\link{marker.select}} for select markers.
#' \code{\link{find.sig.genes}} for signature gene list.
#'
#' @examples
#' data(toydata)
#' 
#' markers <- toydata$markers
#' 
#' marker.freq <- markerList2mat(list(markers))
#' print(marker.freq)
#'
#' @export
markerList2mat <- function(markerList) {
    # Extract unique cell types from the marker list
    celltypes <- unique(unlist(lapply(markerList, names)))

    # Create a list that counts the frequency of each marker gene per cell type
    wtlist <- lapply(celltypes, function(celltype) {
        table(unlist(lapply(markerList, function(sig_list) {
            sig_list[[celltype]]
        })))
    })

    # Assign names to the weighted list
    names(wtlist) <- celltypes

    # Extract the names of the marker genes for each cell type
    markerlist <- lapply(wtlist, names)

    # Get the unique marker genes across all cell types
    marker_genes <- unique(unlist(markerlist))

    # Create an empty matrix to store the weights (frequencies)
    wtmat <- matrix(
        data = 0, nrow = length(celltypes), ncol = length(marker_genes),
        dimnames = list(celltypes, marker_genes)
    )

    # Fill the matrix with the frequencies of marker genes per cell type
    for (celltype in celltypes) {
        wtmat[celltype, markerlist[[celltype]]] <- wtlist[[celltype]]
    }

    return(wtmat)
}




#' Annotate Cells Using Distance Matrix and Marker Frequencies
#'
#' @description
#' This function annotates cells based on a cell-gene distance matrix and marker gene frequencies. It computes the average distances, optionally calculates confidence levels for the predictions, and computes cell mixing proportions.
#'
#' @param distce A matrix of distances between spots and genes. Rows represent genes, and columns represent cells. Generally, it is a list of the output of function \code{ProFAST::pdistance} with CAESAR co-embedding as input.
#' @param marker.freq A matrix where rows represent cell types, and columns represent marker genes. The values in the matrix represent the frequency or weight of each marker gene for each cell type. Generally, it is a list of the output of function \code{markerList2mat}.
#' @param gene.use A character vector specifying which genes to use for the annotation. If `NULL`, all genes in `distce` will be used. Default is `NULL`.
#' @param cal.confidence Logical, indicating whether to calculate the confidence of the predictions. Default is `TRUE`.
#' @param cal.proportions Logical, indicating whether to calculate the mixing proportions of cell types for each spot. Default is `TRUE`.
#' @param parallel Logical, indicating whether to run the confidence calculation in parallel. Default is `TRUE`.
#' @param ncores The number of cores to use for parallel computation. Default is 10.
#' @param n_fake The number of fake (randomized) distance matrices to simulate for confidence calculation. Default is 1001.
#' @param seed The random seed for reproducibility. Default is 1.
#' @param threshold A numeric value specifying the confidence threshold below which a cell is labeled as `unassigned`. Default is 0.95.
#' @param unassign A character string representing the label to assign to cells below the confidence threshold. Default is `"unassigned"`.
#'
#' @return A list with the following components:
#' \item{ave.dist}{A matrix of average distances between each cell and each cell type.}
#' \item{confidence}{A numeric vector of confidence values for each cell (if `cal.confidence = TRUE`).}
#' \item{pred}{A character vector of predicted cell types for each cell.}
#' \item{pred_unassign}{A character vector of predicted cell types with cells below the confidence threshold labeled as `unassigned` (if `cal.confidence = TRUE`).}
#' \item{cell_mixing_proportions}{A matrix of mixing proportions for each spot across the different cell types (if `cal.proportions = TRUE`).}
#'
#' @seealso
#' \code{\link{marker.select}} for select markers.
#' \code{\link{find.sig.genes}} for signature gene list.
#' \code{\link{markerList2mat}} for marker frequency matrix.
#' \code{\link[ProFAST]{pdistance}} for obtain cell-gene distance matrix using co-embedding.
#'
#' @importFrom stats pnorm sd 
#' @importFrom future plan multicore
#' @importFrom furrr future_map
#' @importFrom pbapply pbsapply
#' @export
#'
#' @examples 
#' data(toydata)
#' 
#' seu <- toydata$seu
#' markers <- toydata$markers
#' 
#' seu <- ProFAST::pdistance(seu, reduction = "caesar")
#' distce <- Seurat::GetAssayData(object = seu, slot = "data", assay = "distce")
#' 
#' marker.freq <- markerList2mat(list(markers))
#' 
#' anno_res <- annotation_mat(distce, marker.freq, cal.confidence = FALSE, cal.proportions = FALSE)
#' str(anno_res)
annotation_mat <- function(
    distce, marker.freq, gene.use = NULL,
    cal.confidence = TRUE, cal.proportions = TRUE,
    parallel = TRUE, ncores = 10, n_fake = 1001, seed = 1,
    threshold = 0.95, unassign = "unassigned") {
    # Filter genes to be used
    if (is.null(gene.use)) {
        gene.use <- row.names(distce)
        id <- apply(distce, 1, function(x) all(!is.na(x))) # Remove rows with NA values
        gene.use <- gene.use[id]
        distce <- distce[gene.use, ]
    }

    # Identify marker genes common to both marker.freq and distce
    marker_genes <- intersect(colnames(marker.freq), gene.use)

    # Calculate weight matrix by normalizing marker frequencies
    weight.mat <- t(apply(marker.freq[, marker_genes], 1, function(x) x / sum(x)))

    # Compute the average distance between each cell and each cell type
    ave.dist <- t(weight.mat %*% distce[marker_genes, , drop = FALSE])

    # Predict cell types based on minimum average distance
    pred <- colnames(ave.dist)[apply(ave.dist, 1, which.min)]

    # Confidence calculation using randomized distance matrices
    if (cal.confidence) {
        ave.dist_min <- apply(ave.dist, 1, min)

        fake_dis_fun <- function(i, distce, weight.mat) {
            genes <- sample(row.names(distce), ncol(weight.mat))
            apply(weight.mat %*% distce[genes, , drop = FALSE], 2, min)
        }

        if (parallel) {
            future::plan("multicore", workers = ncores)
            dis.mat.sim <- furrr::future_map(
                1:n_fake, fake_dis_fun,
                distce = distce, weight.mat = weight.mat,
                .progress = TRUE, .options = furrr::furrr_options(seed = seed)
            )
            dis.mat.sim <- Reduce(cbind, dis.mat.sim)
        } else {
            # dis.mat.sim <- pbapply::pbsapply(
            #     1:n_fake, fake_dis_fun,
            #     distce = distce, weight.mat = weight.mat
            # )
            
            if (is_interactive()) {
                dis.mat.sim <- pbapply::pbsapply(
                    1:n_fake, fake_dis_fun,
                    distce = distce, weight.mat = weight.mat
                )
            } else {
                dis.mat.sim <- sapply(
                    1:n_fake, fake_dis_fun,
                    distce = distce, weight.mat = weight.mat
                )
            }
        }

        # Calculate confidence as the proportion of randomized distances greater than the real distances
        confidence <- rowMeans(dis.mat.sim - ave.dist_min > 0)

        # Assign 'unassigned' label to cells with confidence below the threshold
        pred_unassign <- pred
        pred_unassign[confidence < threshold] <- unassign
    } else {
        confidence <- NULL
        pred_unassign <- NULL
    }

    # Compute cell mixing proportions based on the distance matrix
    if (cal.proportions) {
        cell_mixing_proportions <- t(apply(ave.dist, 1, function(x) {
            x <- 1 - pnorm((x - mean(x)) / sd(x))
            x / sum(x)
        }))
    } else {
        cell_mixing_proportions <- NULL
    }

    # Return results as a list
    return(
        list(
            ave.dist = ave.dist,
            confidence = confidence,
            pred = pred,
            pred_unassign = pred_unassign,
            cell_mixing_proportions = cell_mixing_proportions
        )
    )
}




#' Perform Cell Annotation Using CAESAR with Confidence and Proportion Calculation
#'
#' @description
#' This function annotates cells in a Seurat object using marker gene frequencies and a distance matrix. It calculates average distances between cells and cell types, confidence scores, and mixing proportions. Optionally, it can add the annotations and related metrics to the Seurat object metadata.
#'
#' @param seu A Seurat object containing cell expression data.
#' @param marker.freq A matrix where rows represent cell types and columns represent marker genes. The values in the matrix represent the frequency or weight of each marker gene for each cell type. Generally, it is a list of the output of function \code{markerList2mat}.
#' @param reduction.name A character string specifying the name of the dimensional reduction to use when calculating distances. Default is "caesar".
#' @param assay.dist A character string specifying the name of the assay to store the distance matrix. If not present in the Seurat object, the function will calculate the distances using \code{ProFAST::pdistance}. Default is "distce".
#' @param gene.use A character vector specifying which genes to use for the annotation. If \code{NULL}, all genes in the distance matrix will be used. Default is \code{NULL}.
#' @param cal.confidence Logical, indicating whether to calculate the confidence of the predictions. Default is \code{TRUE}.
#' @param cal.proportions Logical, indicating whether to calculate the mixing proportions of cell types for each cell. Default is \code{TRUE}.
#' @param parallel Logical, indicating whether to run the confidence calculation in parallel. Default is \code{TRUE}.
#' @param ncores The number of cores to use for parallel computation. Default is 10.
#' @param n_fake The number of fake (randomized) distance matrices to simulate for confidence calculation. Default is 1001.
#' @param seed The random seed for reproducibility. Default is 1.
#' @param threshold A numeric value specifying the confidence threshold below which a cell is labeled as \code{unassigned}. Default is 0.95.
#' @param unassign A character string representing the label to assign to cells below the confidence threshold. Default is "unassigned".
#' @param add.to.meta Logical, indicating whether to return the annotation results directly or add them to the Seurat object metadata. If \code{TRUE}, the function will return the results directly. Default is \code{FALSE}.
#'
#' @return If \code{add.to.meta = FALSE}, the Seurat object with the added metadata for predicted cell types (\code{CAESAR}), predictions with unassigned (\code{CAESARunasg}), confidence scores (\code{CAESARconf}), average distances, and mixing proportions. If \code{add.to.meta = TRUE}, a list containing the above annotation results is returned.
#'
#' @seealso
#' \code{\link{marker.select}} for select markers.
#' \code{\link{find.sig.genes}} for signature gene list.
#' \code{\link{markerList2mat}} for marker frequency matrix.
#' \code{\link[ProFAST]{pdistance}} for obtain cell-gene distance matrix using co-embedding.
#' \code{\link{annotation_mat}} for annotation procedure.
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' markers <- toydata$markers
#' 
#' marker.freq <- markerList2mat(list(markers))
#' anno_res <- CAESAR.annotation(seu, marker.freq, cal.confidence = FALSE, cal.proportions = FALSE)
#' str(anno_res)
#'
#' @importFrom Seurat AddMetaData GetAssayData Assays
#' @importFrom ProFAST pdistance
#' @export
CAESAR.annotation <- function(
    seu, marker.freq, reduction.name = "caesar", assay.dist = "distce",
    gene.use = NULL, cal.confidence = TRUE, cal.proportions = TRUE,
    parallel = TRUE, ncores = 10, n_fake = 1001, seed = 1,
    threshold = 0.95, unassign = "unassigned", add.to.meta = FALSE) {
    
    if (!inherits(seu, "Seurat")) {
        stop("Input 'seu' must be a Seurat object.")
    }

    if (!is.matrix(marker.freq)) {
        stop("Input 'marker.freq' must be a matrix.")
    }

    if (!is.character(reduction.name) || length(reduction.name) != 1) {
        stop("'reduction.name' must be a single character string.")
    }

    if (!is.character(assay.dist) || length(assay.dist) != 1) {
        stop("'assay.dist' must be a single character string.")
    }

    if (!is.null(gene.use) && !is.character(gene.use)) {
        stop("'gene.use' must be a character vector or NULL.")
    }

    if (!is.logical(cal.confidence) || length(cal.confidence) != 1) {
        stop("'cal.confidence' must be a single logical value (TRUE or FALSE).")
    }
    if (!is.logical(cal.proportions) || length(cal.proportions) != 1) {
        stop("'cal.proportions' must be a single logical value (TRUE or FALSE).")
    }

    if (!is.logical(parallel) || length(parallel) != 1) {
        stop("'parallel' must be a single logical value (TRUE or FALSE).")
    }

    if (!is.numeric(ncores) || length(ncores) != 1 || ncores <= 0 || ncores %% 1 != 0) {
        stop("'ncores' must be a positive integer.")
    }

    if (!is.numeric(n_fake) || length(n_fake) != 1 || n_fake <= 0 || n_fake %% 1 != 0) {
        stop("'n_fake' must be a positive integer.")
    }

    if (!is.numeric(seed) || length(seed) != 1 || seed <= 0 || seed %% 1 != 0) {
        stop("'seed' must be a positive integer.")
    }

    if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
        stop("'threshold' must be a numeric value between 0 and 1.")
    }

    if (!is.character(unassign) || length(unassign) != 1) {
        stop("'unassign' must be a single character string.")
    }

    if (!is.logical(add.to.meta) || length(add.to.meta) != 1) {
        stop("'add.to.meta' must be a single logical value (TRUE or FALSE).")
    }
    
    # Check if the distance matrix assay exists, if not, calculate the distance
    if (!(assay.dist %in% Seurat::Assays(seu))) {
        seu <- ProFAST::pdistance(
            seu,
            reduction = reduction.name, assay.name = assay.dist
        )
    }

    # Extract the distance matrix from the Seurat object
    distce <- Seurat::GetAssayData(
        object = seu, slot = "data", assay = assay.dist
    )

    # Run the annotation process using the distance matrix and marker frequencies
    res_list <- annotation_mat(
        distce, marker.freq,
        gene.use = gene.use,
        cal.confidence = cal.confidence, cal.proportions = cal.proportions,
        parallel = parallel, ncores = ncores, n_fake = n_fake, seed = seed,
        threshold = threshold, unassign = unassign
    )

    # If add.to.meta is TRUE, return the annotation results list directly
    if (!add.to.meta) {
        return(res_list)
    } else {
        # Otherwise, add the annotations to the Seurat object's metadata
        anno.df <- data.frame(
            res_list$pred, res_list$pred_unassign, res_list$confidence
        )
        colnames(anno.df) <- c("CAESAR", "CAESARunasg", "CAESARconf")
        seu <- Seurat::AddMetaData(seu, anno.df, col.name = colnames(anno.df))

        # Add the average distance matrix to the Seurat object's metadata
        caesar_dist <- res_list$ave.dist
        celltype_names <- gsub("-|/| ", ".", colnames(caesar_dist))
        colnames(caesar_dist) <- paste0("dist_", celltype_names)
        seu <- Seurat::AddMetaData(seu, caesar_dist, colnames(caesar_dist))

        # Add the cell mixing proportions to the Seurat object's metadata
        caesar_prob <- res_list$cell_mixing_proportions
        colnames(caesar_prob) <- paste0("prob_", celltype_names)
        seu <- Seurat::AddMetaData(seu, caesar_prob, colnames(caesar_prob))

        # Return the updated Seurat object
        return(seu)
    }
}

