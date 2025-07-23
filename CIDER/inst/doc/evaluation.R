## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", warning = FALSE, message = FALSE
)

## ----setup--------------------------------------------------------------------
library(CIDER)
library(Seurat)
library(cowplot)
library(ggplot2)

## -----------------------------------------------------------------------------
# Download the data
data_url <- "https://figshare.com/ndownloader/files/52116197"
data_file <- file.path(tempdir(), "dendritic.rds")

if (!file.exists(data_file)) {
  message("Downloading data...")
  download.file(data_url, destfile = data_file, mode = "wb")
}

dendritic_reduced <- load(data_file)

## -----------------------------------------------------------------------------
# load("../data/dendritic.rda")
dendritic <- CreateSeuratObject(counts = dendritic@assays$RNA@counts, meta.data = dendritic@meta.data)

## -----------------------------------------------------------------------------
# Verify batch composition
table(dendritic$Batch)

## ----integration--------------------------------------------------------------
seu.list <- SplitObject(dendritic, split.by = "Batch")
for (i in 1:length(seu.list)) {
  seu.list[[i]] <- NormalizeData(seu.list[[i]], verbose = FALSE)
  seu.list[[i]] <- FindVariableFeatures(seu.list[[i]], 
                                        selection.method = "vst", 
                                        nfeatures = 1000, verbose = FALSE)
}
seu.anchors <- FindIntegrationAnchors(object.list = seu.list, 
                                      dims = 1:15, verbose = FALSE)
seu.integrated <- IntegrateData(anchorset = seu.anchors, 
                                dims = 1:15, verbose = FALSE)

DefaultAssay(seu.integrated) <- "integrated"
seu.integrated <- ScaleData(seu.integrated, verbose = FALSE)
seu.integrated <- RunPCA(seu.integrated, verbose = FALSE)
seu.integrated <- RunTSNE(seu.integrated, reduction = "pca", dims = 1:5)

## -----------------------------------------------------------------------------
rm(seu.list, seu.anchors)
gc()

## -----------------------------------------------------------------------------
seu.integrated <- hdbscan.seurat(seu.integrated)

## -----------------------------------------------------------------------------
ider <- getIDEr(seu.integrated, use.parallel = FALSE, verbose = FALSE)

## -----------------------------------------------------------------------------
seu.integrated <- estimateProb(seu.integrated, ider)

## ----fig.height=3, fig.width=11-----------------------------------------------
p1 <- scatterPlot(seu.integrated, "tsne", "dbscan_cluster")
p2 <- scatterPlot(seu.integrated, "tsne", colour.by = "similarity") + labs(fill = "Similarity")
p3 <- scatterPlot(seu.integrated, "tsne", colour.by = "pvalue") + labs(fill = "Prob of \nrejection")
plot_grid(p1, p2, p3, ncol = 3)

## ----fig.height=5, fig.width=5------------------------------------------------
plotNetwork(seu.integrated, ider, weight.factor = 3)

## ----fig.height=5, fig.width=5------------------------------------------------
plotHeatmap(seu.integrated, ider)

## ----fig.height=3, fig.width=5------------------------------------------------
scatterPlot(seu.integrated, "tsne", colour.by = "Group") + labs(fill = "Group\n (ground truth)")

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

