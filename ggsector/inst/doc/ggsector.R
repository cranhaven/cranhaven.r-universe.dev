## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    tidy = FALSE,
    collapse = TRUE,
    comment = "#>",
    fig.width = 5,
    fig.height = 4
)

## ----setup, message = FALSE---------------------------------------------------
##library
library(magrittr)
library(grid)
library(Seurat)
library(ggplot2)
library(ggsector)

## ----fig.width=3, fig.height=3------------------------------------------------
tmp_df <- sector_df(x = 0.5, y = 0.5, theta = 25, r = 0.4, start = 0, r_start = 0)
head(tmp_df)
grid.newpage()
grid.polygon(
    tmp_df$x, tmp_df$y,
    vp = viewport(height = unit(1, "snpc"), width = unit(1, "snpc"))
)

## ----fig.width=3, fig.height=3------------------------------------------------
tmp_df <- sector_df(x = 0.5, y = 0.5, theta = 25, r = 0.4, start = 50, r_start = 0.2)
head(tmp_df)
grid.newpage()
grid.polygon(
    tmp_df$x, tmp_df$y,
    vp = viewport(height = unit(1, "snpc"), width = unit(1, "snpc"))
)

## ----fig.width=3, fig.height=3------------------------------------------------
tmp_df <- sector_df(
    x = 0.5, y = 0.5, theta = 180, r = 0.4,
    start = 90, r_start = 0, type = "degree"
)
head(tmp_df)
grid.newpage()
grid.polygon(
    tmp_df$x, tmp_df$y,
    vp = viewport(height = unit(1, "snpc"), width = unit(1, "snpc"))
)

## ----fig.width=3, fig.height=3------------------------------------------------
tmp_df <- sector_df(
    x = 0.5, y = 0.5, theta = 180, r = 0.4,
    start = 270, r_start = 0.2, type = "degree"
)
head(tmp_df)
grid.newpage()
grid.polygon(
    tmp_df$x, tmp_df$y,
    vp = viewport(height = unit(1, "snpc"), width = unit(1, "snpc"))
)

## ----fig.width=3, fig.height=3------------------------------------------------
tmp_df <- sector_df_multiple(
    x = c(0.2, 0.5, 0.8),
    theta = c(25, 50, 75),
    r = 0.15,
    start = c(75, 50, 100),
    r_start = c(0, 0.05, 0.1),
    type = "percent"
)
head(tmp_df)
grid.newpage()
grid.polygon(
    tmp_df$x,
    tmp_df$y,
    id = tmp_df$group,
    vp = viewport(height = unit(1, "snpc"), width = unit(1, "snpc")),
    gp = gpar(
        fill = 3:1, col = 1:3
    )
)

## -----------------------------------------------------------------------------
grid.newpage()
gp <- sectorGrob(
    x = unit(c(3, 5, 7), "cm"),
    y = unit(c(3, 5, 7), "cm"),
    theta = c(90, 180, 270),
    r = 1,
    start = c(180, 180, 270),
    r_start = c(0.6, 0.3, 0),
    type = "degree",
    group = factor(1:3, levels = c(2, 3, 1)),
    gp = gpar(fill = c("green", "red", "blue"))
)
grid.draw(gp)

## -----------------------------------------------------------------------------
grid.newpage()
grid.sector(
    x = c(0.1, 0.5, 0.9),
    y = c(0.9, 0.6, 0.1),
    theta = c(25, 50, 90),
    r = .1,
    start = c(25, 50, 100),
    r_start = c(0.06, 0.03, 0),
    type = "percent",
    group = factor(1:3, levels = c(2, 3, 1)),
    gp = gpar(col = c("green", "red", "blue"), fill = 2:4),
    default.units = "npc"
)

## ----eval=rlang::is_installed("ComplexHeatmap")-------------------------------
## install ComplexHeatmap
# if (!require("ComplexHeatmap", quietly = TRUE)) {
#     if (!require("BiocManager", quietly = TRUE)) {
#         install.packages("BiocManager")
#     }
#     BiocManager::install("ComplexHeatmap")
# }

## run
library(magrittr)
library(ComplexHeatmap)

t0 <- cor(mtcars) %>%
    set_colnames(paste("y_", colnames(.))) %>%
    set_rownames(paste("x_", rownames(.)))
mat <- abs(t0)
mat[1:5, 1:5]

## ----fig.width=8, fig.height=8, eval=rlang::is_installed("ComplexHeatmap")----
set.seed(1)
Heatmap(
    mat,
    name = "vp",
    rect_gp = gpar(type = "none"),
    cell_fun = function(j, i, x, y, width, height, fill) {
        grid.rect(
            x = x, y = y, width = width, height = height,
            gp = gpar(col = "grey", fill = NA)
        )
        grid.sector(
            theta = mat[i, j] * 100,
            r = 0.5,
            start = mat[i, j] * 100 * runif(1),
            r_start = mat[i, j] * 0.49 * runif(1),
            vp = viewport(x, y, width, height),
            gp = gpar(fill = fill, col = "transparent")
        )
    },
    width = unit(.7, "snpc"),
    height = unit(.7, "snpc")
)

## ----fig.width=8, fig.height=8, eval=rlang::is_installed("ComplexHeatmap")----
# The default viewport locks the horizontal and vertical axes
# so that the sector does not deform, which needs to be removed here.
# The radius 'r' is half the min(length, width).
set.seed(2)
Heatmap(
    mat,
    name = "xy + r",
    rect_gp = gpar(type = "none"),
    cell_fun = function(j, i, x, y, width, height, fill) {
        grid.rect(
            x = x, y = y, width = width, height = height,
            gp = gpar(col = "grey", fill = NA)
        )
        r <- as.numeric(min(width, height)) / 2
        grid.sector(
            x,
            y,
            theta = mat[i, j] * 100,
            r = r,
            start = mat[i, j] * 100 * runif(1),
            r_start = mat[i, j] * r * 0.9 * runif(1),
            vp = NULL,
            gp = gpar(fill = fill, col = "transparent")
        )
    },
    width = unit(.7, "snpc"),
    height = unit(.7, "snpc")
)

## ----fig.width=8, fig.height=8, eval=rlang::is_installed("ComplexHeatmap")----
# The input matrix needs to be extracted with pindex(mat, i, j)
set.seed(3)
Heatmap(
    mat,
    name = "layer",
    rect_gp = gpar(type = "none"),
    layer_fun = function(j, i, x, y, width, height, fill) {
        grid.rect(
            x = x, y = y, width = width, height = height,
            gp = gpar(col = "grey", fill = NA)
        )
        r <- as.numeric(min(width, height)) / 2
        grid.sector(
            x,
            y,
            theta = pindex(mat, i, j) * 100,
            r = r,
            start = pindex(mat, i, j) * 100 * runif(nrow(mat) * ncol(mat)),
            r_start = pindex(mat, i, j) * r * 0.9 * runif(nrow(mat) * ncol(mat)),
            vp = NULL,
            gp = gpar(fill = fill, col = "transparent")
        )
    },
    width = unit(.7, "snpc"),
    height = unit(.7, "snpc")
)

## -----------------------------------------------------------------------------
library(ggsector)
library(reshape2)
df <- cor(mtcars)[1:3, 1:5] %>%
    abs() %>%
    melt(varnames = c("x", "y"))
## Note, for better display effect, please always add coord_fixed()
## Note, for better display effect, please always add coord_fixed()
## Note, for better display effect, please always add coord_fixed()

## ----fig.width=5, fig.height=4------------------------------------------------
ggplot(df) +
    ## type = "percent", theta = 0-100
    geom_sector(
        aes(y, x, theta = value * 100),
        type = "percent",
        color = "blue",
        individual = TRUE
    ) +
    ## type = "degree", theta = 0-360
    geom_sector(
        aes(y, x, theta = value * 360),
        type = "degree",
        color = "red",
        alpha = 0.5,
        individual = TRUE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## -----------------------------------------------------------------------------
ggplot(df) +
    geom_sector(
        aes(y, x, theta = value * 100),
        r = rep(c(0.15, 0.3, 0.45), 5),
        fill = 2,
        individual = TRUE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## -----------------------------------------------------------------------------
ggplot(df) +
    geom_sector(
        aes(y, x, theta = value * 100),
        start = rep(c(60, 40, 20), 5),
        fill = 2,
        individual = TRUE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## -----------------------------------------------------------------------------
ggplot(df) +
    geom_sector(
        aes(y, x, theta = value * 100),
        r_start = rep(c(0.15, 0.25, 0.35), 5),
        fill = 2,
        individual = TRUE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=6, fig.height=6------------------------------------------------
# x = x, y = y
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(x, y),
        theta = 75,
        fill = 2,
        r = 0.5,
        individual = TRUE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=8, fig.height=3------------------------------------------------
# x = y, y =x
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(y, x),
        theta = 75,
        fill = 2,
        r = 0.5,
        individual = TRUE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=6, fig.height=6------------------------------------------------
# x = x, y = y
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(x, y),
        theta = 75,
        fill = 2,
        r = 0.5,
        individual = FALSE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=8, fig.height=3------------------------------------------------
# x = y, y =x
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(y, x),
        theta = 75,
        fill = 2,
        r = 0.5,
        individual = TRUE
    ) +
    coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=6, fig.height=4------------------------------------------------
# x = x, y = y
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(x, y),
        theta = 75,
        fill = 2,
        r = 0.35, ## To reduce the radius, you need to try it manually
        individual = TRUE
    ) +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=6, fig.height=4------------------------------------------------
# x = y, y =x
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(y, x),
        theta = 75,
        fill = 2,
        r = 0.25, ## To reduce the radius, you need to try it manually
        individual = TRUE
    ) +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=6, fig.height=4------------------------------------------------
# x = x, y = y
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(x, y),
        theta = 75,
        fill = 2,
        r = 0.5,
        ## You need to manually adjust the `ratio` value
        ## to prevent sector deformation.
        ratio = 1.6,
        individual = FALSE
    ) +
    theme_bw() +
    theme(axis.title = element_blank())

## ----fig.width=6, fig.height=4------------------------------------------------
# x = y, y =x
ggplot(rbind(
    cbind(df, t1 = 1),
    cbind(df[1:9, ], t1 = 2)
)) +
    facet_wrap(~t1, ncol = 2) +
    geom_sector(
        aes(y, x),
        theta = 75,
        fill = 2,
        r = 0.5,
        ## You need to manually adjust the `ratio` value
        ## to prevent sector deformation.
        ratio = 1.6,
        individual = FALSE
    ) +
    # coord_fixed() +
    theme_bw() +
    theme(axis.title = element_blank())

## ----eval = FALSE-------------------------------------------------------------
#  ## Download pbmc data from
#  # https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz
#  library(Seurat)
#  path <- paste0(tempdir(), "/pbmc3k.tar.gz")
#  file <- paste0(tempdir(), "/filtered_gene_bc_matrices/hg19")
#  download.file(
#      "https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz",
#      path
#  )
#  untar(path, exdir = tempdir())
#  pbmc.data <- Read10X(data.dir = file)
#  pbmc <- CreateSeuratObject(
#      counts = pbmc.data, project = "pbmc3k",
#      min.cells = 3, min.features = 200
#  )
#  pbmc <- NormalizeData(pbmc)
#  pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)
#  pbmc <- ScaleData(pbmc, features = rownames(pbmc))
#  pbmc <- RunPCA(pbmc)
#  pbmc <- RunUMAP(pbmc, dim = 1:10)
#  pbmc <- FindNeighbors(pbmc, dims = 1:10)
#  pbmc <- FindClusters(pbmc, resolution = 1)
#  pbmc <- FindClusters(pbmc, resolution = 0.5)
#  markers <- tibble::tribble(
#      ~type, ~marker,
#      "Naive CD4+ T", "IL7R,CCR7",
#      "CD14+ Mono", "CD14,LYZ",
#      "Memory CD4+", "IL7R,S100A4",
#      "B", "MS4A1",
#      "CD8+ T", "CD8A",
#      "FCGR3A+ Mono", "FCGR3A,MS4A7",
#      "NK", "GNLY,NKG7",
#      "DC", "FCER1A,CST3",
#      "Platelet", "PPBP",
#  ) %>%
#      tidyr::separate_rows(marker, sep = ", *") %>%
#      dplyr::distinct()
#  
#  # Dotplot
#  DotPlot(pbmc, features = unique(markers$marker)) + coord_flip()
#  
#  # contrast with DotPlot
#  SectorPlot(pbmc, markers$marker, features.level = unique(rev(markers$marker)))
#  
#  SectorPlot(pbmc, markers$marker, group.by = "RNA_snn_res.1")
#  SectorPlot(pbmc, markers$marker, group.by = "RNA_snn_res.1", slot = "scale.data")
#  
#  # split plot
#  # Assume a variable 'day', expressed as the number of days of cell development.
#  set.seed(1)
#  pbmc[["day"]] <- sample(1:3, ncol(pbmc), TRUE)
#  SectorPlot(pbmc, markers$marker, group.by = "RNA_snn_res.0.5", split.by = "day")
#  SectorPlot(
#      pbmc, markers$marker,
#      group.by = "day", split.by = "RNA_snn_res.0.5", nrow = 1
#  )

