## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(CIDER)
library(Seurat)
library(pheatmap)
library(ggplot2)
library(cowplot)
library(viridis)

## -----------------------------------------------------------------------------
# Download the data
data_url <- "https://figshare.com/ndownloader/files/31469387"
data_file <- file.path(tempdir(), "pancreas_counts.RData")

if (!file.exists(data_file)) {
  message("Downloading count data to temporary directory...")
  download.file(data_url, destfile = data_file, mode = "wb")
}

## -----------------------------------------------------------------------------
# Load counts matrix and metadata
data_env <- new.env()
loaded_objects <- load(file = data_file, envir = data_env)
pancreas_counts <- data_env[[loaded_objects[1]]]

load("../data/pancreas_meta.RData") # meta data/cell information
seu <- CreateSeuratObject(counts = pancreas_counts, meta.data = pancreas_meta)
table(seu$Batch)

## ----seurat-pipeline----------------------------------------------------------
seu <- NormalizeData(seu, verbose = FALSE)
seu <- FindVariableFeatures(seu, selection.method = "vst", nfeatures = 2000, verbose = FALSE)
seu <- ScaleData(seu, verbose = FALSE)
seu <- RunPCA(seu, npcs = 20, verbose = FALSE)
seu <- RunTSNE(seu, reduction = "pca", dims = 1:12)

## ----scatterPlot-confounded, fig.width=7, fig.height=3------------------------
p1 <- scatterPlot(seu, "pca",colour.by = "Batch", title = "PCA") 
p2 <- scatterPlot(seu, "tsne",colour.by = "Batch", title = "t-SNE") 
plot_grid(p1, p2)

## ----fig.width=5, fig.height=3------------------------------------------------
seu <- FindNeighbors(seu, dims = 1:12)
seu <- FindClusters(seu)

scatterPlot(seu, "tsne",colour.by = "seurat_clusters", title = "t-SNE") 

## ----fig.width=6, fig.height=3------------------------------------------------
res <- data.frame(table(seu$seurat_clusters, seu$Batch))
ggplot(res, aes(fill=Var2, y=Freq, x=Var1)) + 
    geom_bar(position="stack", stat="identity") + xlab("CIDER_cluster") + ylab("Proportions")

## ----initial_cluster----------------------------------------------------------
seu$initial_cluster <- paste(seu$Group, seu$Batch, sep = "_")
table(seu$initial_cluster)

## ----run-getIDER-noparallel---------------------------------------------------
ider <- getIDEr(seu, 
                group.by.var = "initial_cluster",
                batch.by.var = "Batch",
                downsampling.size = 35, 
                use.parallel = FALSE, verbose = FALSE)

## ----heatmap, fig.height=7, fig.width=7---------------------------------------
groups <- c("alpha","beta","delta", "gamma","ductal","endothelial", "activated_stellate", "quiescent_stellate", "macrophage")
idx1 <- paste0(groups, "_human")
idx2 <- paste0(groups, "_mouse")

pheatmap::pheatmap(
  ider[[1]][idx1, idx2],
  color = inferno(10),
  border_color = NA,
  display_numbers = TRUE,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  width = 7,
  height = 5,
  cellwidth = 22,
  cellheight = 22
)

## ----final-clustering---------------------------------------------------------
seu <- finalClustering(seu, ider, cutree.h = 0.45)
head(seu@meta.data)

## ----summary-tsne-plots, fig.width=10, fig.height=4---------------------------
plot_list <- list()
plot_list[[1]] <- scatterPlot(seu, "tsne", colour.by = "CIDER_cluster", title = "asCIDER clustering results") 
plot_list[[2]] <- scatterPlot(seu, "tsne", colour.by = "Group", title = "Ground truth of cell populations") 
plot_grid(plotlist = plot_list, ncol = 2)

## ----fig.height=3, fig.width=5------------------------------------------------
res <- data.frame(table(seu$CIDER_cluster, seu$Batch))
ggplot(res, aes(fill=Var2, y=Freq, x=Var1)) + 
    geom_bar(position="stack", stat="identity") + xlab("CIDER_cluster") + ylab("Proportions")

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

