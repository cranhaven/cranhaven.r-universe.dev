#' Integrate Signature Genes Across Datasets
#'
#' @description
#' This function integrates signature genes across different datasets by identifying common genes that meet specific criteria. It filters out mitochondrial and ribosomal genes, allows for the exclusion of genes based on expression proportion, and supports weighting gene selection by cell type ratios.
#'
#' @param sg_List A list of signature gene lists for different datasets. Each element in the list should be a named list where the names correspond to cell types, and each cell type contains a data frame with gene information.
#' @param ntop An integer specifying the maximum number of top genes to retain for each cell type.
#' @param ct_ratio A list of numeric vectors specifying the ratio of cells for each cell type in the datasets. If \code{NULL}, no weighting is applied. Default is \code{NULL}.
#' @param expr.prop.cutoff A numeric value specifying the minimum expression proportion required for a gene to be considered. Default is 0.1.
#' @param species A character string specifying the species, either "hm" (human) or "ms" (mouse). Default is "hm".
#' @param rm_mito_ribo Logical, indicating whether to remove mitochondrial and ribosomal genes from the signature gene list. Default is \code{TRUE}.
#' @param ratio_lower_bound A numeric value specifying the lower bound for the cell type ratio. Only cell types with a ratio above this bound are considered. Default is 0.0.
#'
#' @return A named list where each element corresponds to a cell type and contains the integrated list of top signature genes.
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' 
#' seu <- ProFAST::pdistance(seu, reduction = "caesar")
#' sglist <- find.sig.genes(seu = seu)
#' top2sgs <- Intsg(list(sglist), ntop = 2)
#' print(top2sgs)
#' 
#' top2intsgs <- Intsg(list(sglist, sglist), ntop = 2)
#'
#' @export
#' @importFrom stats setNames
Intsg <- function(sg_List, ntop, ct_ratio = NULL, expr.prop.cutoff = 0.1, species = "hm", rm_mito_ribo = TRUE, ratio_lower_bound = 0.0) {

    # Ensure sg_List is a list of lists
    if (!is.list(sg_List) || !all(sapply(sg_List, is.list))) {
        stop("'sg_List' must be a list of lists, where each sublist corresponds to different datasets.")
    }

    # Ensure ntop is a positive integer
    if (!is.numeric(ntop) || length(ntop) != 1 || ntop <= 0 || ntop != round(ntop)) {
        stop("'ntop' must be a positive integer specifying the number of top genes to retain.")
    }

    # Ensure expr.prop.cutoff is a numeric value between 0 and 1
    if (!is.numeric(expr.prop.cutoff) || expr.prop.cutoff < 0 || expr.prop.cutoff > 1) {
        stop("'expr.prop.cutoff' must be a numeric value between 0 and 1.")
    }

    # Ensure species is either "hm" or "ms"
    if (!species %in% c("hm", "ms")) {
        stop("'species' must be either 'hm' (human) or 'ms' (mouse).")
    }

    # Ensure ratio_lower_bound is a numeric value between 0 and 1
    if (!is.numeric(ratio_lower_bound) || ratio_lower_bound < 0 || ratio_lower_bound > 1) {
        stop("'ratio_lower_bound' must be a numeric value between 0 and 1.")
    }

    # Ensure ct_ratio is a list of numeric vectors if provided
    if (!is.null(ct_ratio) && (!is.list(ct_ratio) || !all(sapply(ct_ratio, is.numeric)))) {
        stop("'ct_ratio' must be a list of numeric vectors if provided.")
    }

    # Identify unique cell types across all datasets
    cts <- unique(unlist(lapply(sg_List, names)))

    # Integrate signature genes for each cell type
    sg <- setNames(
        lapply(cts, function(ct) {
            # Find datasets that contain the current cell type
            id1 <- which(sapply(sg_List, function(sig_i) {
                ct %in% names(sig_i)
            }))

            # Apply cell type ratio filter if provided
            if (!is.null(ct_ratio)) {
                id1 <- intersect(id1, which(sapply(ct_ratio, function(ratio) {
                    ratio[ct] > ratio_lower_bound
                })))
            }

            # Extract and filter genes by expression proportion
            sig <- lapply(sg_List[id1], function(sig_i) {
                sig_i_ct <- sig_i[[ct]]
                sig_i_ct <- sig_i_ct[sig_i_ct$expr.prop > expr.prop.cutoff, ]
                sig_i_ct$gene
            })

            # Identify common genes across datasets
            gene_here <- Reduce(intersect, sig)

            # Remove mitochondrial and ribosomal genes if specified
            if (rm_mito_ribo) {
                if (species == "ms") {
                    mitochondrial_pattern <- "^(mt-|Mt-)"
                    ribosomal_pattern <- "^(Rps|Rpl)"
                } else {
                    mitochondrial_pattern <- "^MT-"
                    ribosomal_pattern <- "^(RPS|RPL)"
                }

                mitochondrial_genes <- grep(mitochondrial_pattern, gene_here, value = TRUE)
                ribosomal_genes <- grep(ribosomal_pattern, gene_here, value = TRUE)

                gene_here <- setdiff(gene_here, union(mitochondrial_genes, ribosomal_genes))
            }

            # Return NULL if no genes remain after filtering
            if (length(gene_here) == 0) {
                return(NULL)
            }

            # Further filter genes by their presence in each dataset
            sig <- lapply(sig, function(sig_i_ct) {
                sig_i_ct[sig_i_ct %in% gene_here]
            })

            # Weight and rank genes
            if (is.null(ct_ratio)) {
                id2 <- Reduce(`+`, lapply(seq_along(sig), function(i) {
                    match(gene_here, sig[[i]])
                }))
            } else {
                id2 <- Reduce(`+`, lapply(seq_along(sig), function(i) {
                    match(gene_here, sig[[i]]) * ct_ratio[[id1[i]]][ct]
                }))
            }

            # Select the top genes
            sig_overlap <- gene_here[order(id2)[seq_len(min(ntop, length(id2)))]]
            sig_overlap
        }),
        cts
    )

    return(sg)
}




#' Co-embedding UMAP for Genes and Cells in a Seurat Object
#'
#' @description
#' This function performs a co-embedding UMAP of both gene and cell embeddings from a Seurat object. It integrates the dimensionality reduction results for genes and cells into a shared UMAP space.
#'
#' @param seu A Seurat object containing the single-cell RNA-seq data.
#' @param reduction A character string specifying the name of the dimensional reduction to use (e.g., "caesar"). Default is "caesar".
#' @param reduction.name A character string specifying the name of the new dimensional reduction slot in the Seurat object where the co-embedding UMAP will be stored. Default is "caesarUMAP".
#' @param gene.set A character vector specifying the set of genes to include in the co-embedding. If \code{NULL}, all genes are used. Default is \code{NULL}.
#' @param slot A character string specifying the slot in the Seurat object to use for gene expression data. Default is "data".
#' @param assay A character string specifying the assay to use. Default is "RNA".
#' @param seed An integer specifying the random seed for reproducibility. Default is 1.
#' @param ... Additional arguments passed to \code{scater::calculateUMAP}.
#'
#' @details
#' The function extracts the embeddings for both genes and cells from the specified dimensional reduction, combines them, and computes a UMAP embedding. The resulting co-embedding UMAP is stored in a new dimensional reduction slot in the Seurat object.
#'
#' @return A modified Seurat object with the co-embedding UMAP stored in the specified \code{reduction.name} slot.
#'
#' @seealso \code{\link[scater]{calculateUMAP}} for UMAP calculation.
#'
#' @importFrom scater calculateUMAP
#' @importFrom Seurat Loadings Embeddings CreateDimReducObject
#' @export
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' 
#' seu <- CoUMAP(seu, gene.set = rownames(seu))
#' print(seu)
CoUMAP <- function(
    seu, reduction = "caesar", reduction.name = "caesarUMAP", gene.set = NULL,
    slot = "data", assay = "RNA", seed = 1, ...) {

    # Internal function to calculate UMAP using scater
    calculateUMAP <- function(...) {
        if (requireNamespace("scater", quietly = TRUE)) {
            x <- scater::calculateUMAP(...)
            return(x)
        } else {
            stop("CoUMAP: scater is not available. Install scater to use this functionality.")
        }
    }

    # Ensure the Seurat object is valid
    if (!inherits(seu, "Seurat")) {
        stop("CoUMAP: 'seu' must be a Seurat object.")
    }

    # Ensure reduction and reduction.name are valid character strings
    if (!is.character(reduction) || length(reduction) != 1) {
        stop("CoUMAP: 'reduction' must be a single character string specifying the dimensional reduction to use.")
    }
    if (!is.character(reduction.name) || length(reduction.name) != 1) {
        stop("CoUMAP: 'reduction.name' must be a single character string specifying the name of the new reduction slot.")
    }

    # Default gene.set to all genes if not provided
    if (is.null(gene.set)) gene.set <- rownames(seu)
    gene.set <- unique(gene.set)

    # Ensure gene.set is a character vector
    if (!is.character(gene.set)) {
        stop("CoUMAP: 'gene.set' must be a character vector.")
    }

    # Extract gene and cell embeddings from the specified reduction
    if (!all(gene.set %in% rownames(Seurat::Loadings(seu, reduction)))) {
        stop("CoUMAP: Some genes in 'gene.set' are not present in the loadings of the specified reduction.")
    }
    febd <- Seurat::Loadings(seu, reduction)[gene.set, ]
    cebd <- Seurat::Embeddings(seu, reduction)

    # Set random seed for reproducibility
    set.seed(seed)

    # Combine gene and cell embeddings and compute UMAP
    umap_all <- calculateUMAP(t(rbind(febd, cebd)), ...)

    # Rename UMAP columns
    colnames(umap_all) <- paste0(gsub("_", "", reduction.name), "_", 1:2)
    n_gene <- length(gene.set)

    # Store the UMAP co-embedding in the Seurat object
    seu@reductions[[reduction.name]] <- Seurat::CreateDimReducObject(
        embeddings = umap_all[-c(1:n_gene), ],
        loadings = umap_all[1:n_gene, ],
        key = paste0(gsub("_", "", reduction.name), "_"), assay = assay
    )
    
    return(seu)
}





#' Plot Co-embedding UMAP for Genes and Cells
#'
#' @description
#' This function generates a UMAP plot for co-embedding genes and cells in a Seurat object. It allows for customization of point colors, shapes, and text labels, and can display both gene and cell embeddings in the same plot.
#'
#' @param seu A Seurat object containing the co-embedding data.
#' @param reduction A character string specifying the name of the dimensional reduction to use for the UMAP plot. Default is "caesarUMAP".
#' @param gene_txtdata A data frame containing gene names and labels to display as text on the plot. If \code{NULL}, no text labels are shown. Default is \code{NULL}.
#' @param ident A character string specifying the column name in the Seurat object's metadata that contains cell type or cluster labels. If \code{NULL}, the default identities (\code{Idents(seu)}) are used. Default is \code{NULL}.
#' @param xy_name A character string specifying the prefix for the UMAP axes. Default is the value of \code{reduction}.
#' @param dims A numeric vector of length 2 specifying which dimensions to plot. Default is \code{c(1, 2)}.
#' @param cols A named vector of colors for clusters. If \code{NULL}, default colors are generated. Default is \code{NULL}.
#' @param shape_cg A numeric vector of length 2 specifying the shapes for cells and genes. Default is \code{c(1, 5)}.
#' @param pt_size Numeric, specifying the size of the points for cells and genes. Default is 1.
#' @param pt_text_size Numeric, specifying the size of the text labels for genes. Default is 5.
#' @param base_size Numeric, specifying the base font size for the plot. Default is 16.
#' @param base_family Character string specifying the font family for the plot. Default is "serif".
#' @param legend.point.size Numeric, specifying the size of the points in the legend. Default is 5.
#' @param legend.key.size Numeric, specifying the size of the legend keys. Default is 1.5.
#' @param alpha Numeric, specifying the transparency level for cell points. Default is 0.8.
#'
#' @details
#' The function creates a UMAP plot that shows both gene and cell embeddings. Gene embeddings can be optionally labeled with text, and the plot can be customized with different colors, shapes, and sizes.
#'
#' @return A ggplot object representing the UMAP plot.
#'
#' @seealso 
#' \code{\link{CoUMAP}} for obtain co-embedding UMAP.
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' 
#' seu <- CoUMAP(seu, gene.set = rownames(seu))
#' CoUMAP.plot(seu)
#'
#' @importFrom ggplot2 ggplot aes_string geom_point scale_colour_manual scale_shape_manual theme_classic xlim ylim guides guide_legend theme unit
#' @importFrom ggrepel geom_text_repel
#' @importFrom Seurat Embeddings Loadings Idents
#' @importFrom grDevices hcl
#' @export
CoUMAP.plot <- function(
    seu, reduction = "caesarUMAP", gene_txtdata = NULL, ident = NULL,
    xy_name = reduction, dims = c(1, 2), cols = NULL, shape_cg = c(1, 5),
    pt_size = 1, pt_text_size = 5, base_size = 16, base_family = "serif",
    legend.point.size = 5, legend.key.size = 1.5, alpha = 0.8) {

    # Function to generate color hues
    gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        if (requireNamespace("grDevices", quietly = TRUE)) {
            x <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
            return(x)
        } else {
            stop("CoUMAP.plot: grDevices is not available. Install grDevices to use plotting functionalities.")
        }
    }

    # Function to add text labels with repelling
    geom_text_repel_here <- function(...) {
        if (requireNamespace("ggrepel", quietly = TRUE)) {
            x <- ggrepel::geom_text_repel(...)
            return(x)
        } else {
            stop("CoUMAP.plot: ggrepel is not available. Install ggrepel to use plotting functionalities.")
        }
    }

    # Ensure the Seurat object is valid
    if (!inherits(seu, "Seurat")) {
        stop("CoUMAP.plot: 'seu' must be a Seurat object.")
    }

    # Extract gene embeddings (loadings)
    if (is.null(gene_txtdata)) {
        umap_febd <- Seurat::Loadings(seu, reduction)[, dims]
    } else {
        if (!all(c("gene", "label") %in% colnames(gene_txtdata))) {
            stop("CoUMAP.plot: 'gene_txtdata' must contain 'gene' and 'label' columns!")
        }
        umap_febd <- Seurat::Loadings(seu, reduction)[gene_txtdata$gene, dims]
    }

    # Extract cell embeddings
    umap_cebd <- Seurat::Embeddings(seu, reduction)[, dims]

    # Combine gene and cell embeddings
    umap_co_ebd <- as.data.frame(rbind(umap_febd, umap_cebd))
    colnames(umap_co_ebd) <- paste0(xy_name, dims)
    n_gene <- nrow(umap_febd)
    type <- factor(c(rep("gene", n_gene), rep("cell", nrow(umap_cebd))))
    umap_co_ebd$type <- type

    # Determine cell labels
    if (is.null(ident)) {
        y <- as.character(Seurat::Idents(seu))
    } else {
        y <- as.character(seu@meta.data[, ident])
    }

    # Generate or use provided colors for clusters
    if (is.null(cols)) {
        col_clusters <- gg_color_hue(length(unique(y)) + 1)
        col_clusters[1] <- "#808080"
    } else {
        col_clusters <- cols
    }

    # Set names for colors if not provided
    if (is.null(names(col_clusters))) {
        legend_ord <- c("gene", sort(unique(y)))
        names(col_clusters) <- legend_ord
    } else {
        legend_ord <- names(col_clusters)
    }

    # Assign cluster information
    umap_co_ebd$cluster <- factor(c(rep("gene", n_gene), y), levels = legend_ord)

    # Create the base ggplot
    p1 <- ggplot(data = umap_co_ebd, aes_string(
        x = colnames(umap_co_ebd)[1],
        y = colnames(umap_co_ebd)[2],
        shape = "type", color = "cluster"
    )) +
        geom_point(data = subset(umap_co_ebd, type != "gene"), size = pt_size, alpha = alpha) + # Cells
        geom_point(data = subset(umap_co_ebd, type == "gene"), size = pt_size + 1.5, stroke = 0.8) + # Genes
        scale_colour_manual(values = col_clusters) +
        scale_shape_manual(values = c("cell" = 16, "gene" = 4)) +
        theme_classic(base_size = base_size, base_family = base_family) +
        xlim(min(umap_co_ebd[, 1]) - 0.5, max(umap_co_ebd[, 1]) + 0.5) +
        ylim(min(umap_co_ebd[, 2]) - 0.2, max(umap_co_ebd[, 2]) + 0.2)

    # Return plot if no gene text data is provided
    if (is.null(gene_txtdata)) {
        return(p1)
    }

    # Prepare data for gene text labels
    df11 <- data.frame(
        UMAP1 = umap_febd[, 1], UMAP2 = umap_febd[, 2],
        label = row.names(umap_febd),
        type = rep("gene", nrow(umap_febd)),
        cluster = rep("gene", nrow(umap_febd)), color = col_clusters[gene_txtdata$label]
    )
    colnames(df11)[1:2] <- paste0("xy_name", dims)

    # Add gene text labels to the plot
    p12 <- p1 + geom_text_repel_here(
        data = df11, aes_string(
            x = colnames(df11)[1],
            y = colnames(df11)[2],
            label = "label"
        ), size = pt_text_size,
        color = df11$color, max.overlaps = n_gene, force = 700
    ) +
        theme(legend.key.size = unit(legend.key.size, "lines")) +
        guides(
            shape = guide_legend(override.aes = list(size = legend.point.size)),
            color = guide_legend(override.aes = list(size = legend.point.size))
        )

    return(p12)
}




