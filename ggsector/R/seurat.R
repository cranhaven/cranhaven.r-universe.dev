
#' Draw sector for seurat object
#'
#' A better alternative to [Seurat::DotPlot()].
#' For more details, please type `vignette("ggsector")`.
#'
#' @rdname seurat
#' @param object Seurat object
#' @param features Input vector of genes list.
#' @param features.level Levels of genes list.
#' @param assay Specific assay to get data from or set data for; defaults to the default assay.
#' @param slot Specific assay data to get or set.
#' @param group.by Column of metadata to group the cells by, default is Idents().
#' @param group.level Levels of group.
#' @param split.by Column of metadata to split the cells by, default is NULL.
#' @param split.level Levels of split vars.
#' @param col_low Colours for low ends of the gradient.
#' @param col_mid Colour for mid point.
#' @param col_high Colours for high ends of the gradient.
#' @param col_midpoint The midpoint (in data value) of the diverging scale.
#' @param ... Other arguments for [ggplot2::facet_wrap()].
#' Defaults to quantile(exp, 0.5)
#'
#' @return ggplot
#'
#' @examples
#' \donttest{
#' ## Download pbmc data from
#' # https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz
#' library(Seurat)
#' path <- paste0(tempdir(), "/pbmc3k.tar.gz")
#' file <- paste0(tempdir(), "/filtered_gene_bc_matrices/hg19")
#' download.file(
#'     "https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz",
#'     path
#' )
#' untar(path, exdir = tempdir())
#' pbmc.data <- Read10X(data.dir = file)
#' pbmc <- CreateSeuratObject(
#'     counts = pbmc.data,
#'     project = "pbmc3k",
#'     min.cells = 3,
#'     min.features = 200
#' )
#' pbmc <- NormalizeData(pbmc)
#' pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)
#' pbmc <- ScaleData(pbmc, features = rownames(pbmc))
#' pbmc <- RunPCA(pbmc)
#' pbmc <- RunUMAP(pbmc, dim = 1:10)
#' pbmc <- FindNeighbors(pbmc, dims = 1:10)
#' pbmc <- FindClusters(pbmc, resolution = 1)
#' pbmc <- FindClusters(pbmc, resolution = 0.5)
#' markers <- tibble::tribble(
#'     ~type, ~marker,
#'     "Naive CD4+ T", "IL7R,CCR7",
#'     "CD14+ Mono", "CD14,LYZ",
#'     "Memory CD4+", "IL7R,S100A4",
#'     "B", "MS4A1",
#'     "CD8+ T", "CD8A",
#'     "FCGR3A+ Mono", "FCGR3A,MS4A7",
#'     "NK", "GNLY,NKG7",
#'     "DC", "FCER1A,CST3",
#'     "Platelet", "PPBP",
#' ) %>%
#'     tidyr::separate_rows(marker, sep = ", *") %>%
#'     dplyr::distinct()
#'
#' # Dotplot
#' DotPlot(pbmc, features = unique(markers$marker)) + coord_flip()
#'
#' # contrast with DotPlot
#' SectorPlot(pbmc, markers$marker, features.level = unique(rev(markers$marker)))
#'
#' SectorPlot(pbmc, markers$marker, group.by = "RNA_snn_res.1")
#'
#' # split plot
#' # Assume a variable 'day', expressed as the number of days of cell development.
#' set.seed(1)
#' pbmc[["day"]] <- sample(1:3, ncol(pbmc), TRUE)
#' SectorPlot(pbmc, markers$marker, group.by = "RNA_snn_res.0.5", split.by = "day")
#' SectorPlot(
#'     pbmc, markers$marker,
#'     group.by = "day", split.by = "RNA_snn_res.0.5", nrow = 1
#' )
#' }
#'
#' @export
SectorPlot <- function(object,
                       features,
                       features.level,
                       assay,
                       slot = c("data", "scale.data", "counts"),
                       group.by,
                       group.level,
                       split.by,
                       split.level,
                       col_low = "blue",
                       col_mid = "white",
                       col_high = "red",
                       col_midpoint, ...) {
    ## define var
    . <- NULL
    ## para
    sob <- object
    if (missing(assay)) {
        assay <- Seurat::DefaultAssay(sob)
    }
    slot <- match.arg(slot)
    if (missing(group.by)) {
        group <- Seurat::Idents(sob)
        group_name <- "Idents"
    } else {
        group <- sob[[group.by]][[1]]
        group_name <- group.by
    }
    if (missing(split.by)) {
        split <- 1
    } else {
        split <- sob[[split.by]][[1]]
    }
    # select gene
    mk_gene <- intersect(features, rownames(sob))
    diff_gene <- setdiff(features, mk_gene)
    if (length(diff_gene)) {
        warning(paste(
            "These genes were not found in the Seurat object:\n",
            paste(diff_gene, collapse = ", ")
        ))
    }

    ## treat data
    df_sob <- Seurat::GetAssayData(sob, slot = slot, assay = assay)[mk_gene, ]

    if (slot == "counts") {
        df_sob_raw <- df_sob
    } else {
        df_sob_raw <- Seurat::GetAssayData(sob, slot = "counts", assay = assay)[rownames(df_sob), ]
    }


    df_sum_exp <- Matrix::t(df_sob) %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(
            group = group,
            split = split, .
        ) %>%
        dplyr::group_by(group, split) %>%
        dplyr::summarise_all(list(exp = mean))

    df_sum_pct <- Matrix::t(df_sob_raw) %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(
            group = group,
            split = split, .
        ) %>%
        dplyr::group_by(group, split) %>%
        dplyr::summarise_all(list(pct = function(x) {
            sum(x >
                0) / length(x)
        }), )
    df_sum_bind <- dplyr::left_join(
        df_sum_exp,
        df_sum_pct,
        by = c("group", "split")
    )

    df_sum <- df_sum_bind %>%
        tidyr::pivot_longer(-c(group, split),
            names_to = c("gene", ".value"), names_pattern = "(^.*)_(.{3}$)"
        )


    ## col
    if (missing(col_midpoint)) {
        col_midpoint <- stats::quantile(df_sum$exp, 0.5)
    }
    if (!missing(group.level)) {
        g_level <- unique(group.level)
        if (length(g_level) != length(group.level)) {
            warning("Duplicate values in group.level")
        }
        df_sum$group <- factor(df_sum$group, levels = g_level)
    } else {
        df_sum$group <- factor(df_sum$group)
    }
    # split level
    if (!missing(split.level)) {
        s_level <- unique(split.level)
        if (length(s_level) != length(split.level)) {
            warning("Duplicate values in split.level")
        }
        df_sum$split <- factor(df_sum$split, levels = s_level)
    } else {
        df_sum$split <- factor(df_sum$split)
    }
    # feature level
    if (missing(features.level)) {
        features.level <- mk_gene
    }
    f_level <- unique(features.level)
    if (length(f_level) != length(features.level)) {
        warning("Duplicate values in features.level")
    }
    df_sum$gene <- factor(df_sum$gene, levels = rev(f_level))
    # ggplot
    if (length(unique(df_sum$split)) == 1) {
        p <- ggplot(df_sum)
    } else {
        p <- ggplot(df_sum) +
            facet_wrap(~split, ...)
    }
    p + aes_string("group", "gene", theta = "pct * 100", fill = "exp") +
        labs(x = group_name, y = paste0("Gene ", slot)) +
        geom_sector(verbose = FALSE) +
        scale_fill_gradient2(
            low = col_low, mid = col_mid, high = col_high,
            midpoint = col_midpoint
        ) + theme_bw() + coord_fixed()
}
