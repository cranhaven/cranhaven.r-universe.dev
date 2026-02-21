## ----setup, include=FALSE-----------------------------------------------------
library(scPipeline)
library(Seurat)
library(magrittr)
library(ReactomeGSA)

## ----Read10X data-------------------------------------------------------------
counts_data <- Read10X(data.dir = "../inst/extdata", gene.column = 1)

## ----Seurat analysis, echo=FALSE----------------------------------------------
so <- SeuratPreprocess(counts_data)
so <- SeuratLowDim(so)

## ----Violin plot of known genes, echo=FALSE-----------------------------------
VlnPlot(so, features = c("MS4A1", "CD79A"))

## ----Feature plot of known genes, echo=FALSE----------------------------------
FeaturePlot(so, features = c("MS4A1", "GNLY", "CD3E", "CD14", "FCER1A", "FCGR3A", "LYZ", "PPBP",
    "CD8A"))

## ----Marker analysis, echo=FALSE----------------------------------------------
#Compute intense step
#seurat_markers <- SeuratMarkers(so)

## ----Heatmap of markers across clusters, echo=FALSE---------------------------
#Uncomment if you run the above chunk of finding markers
# pbmc.markers <- seurat_markers[[1]]
# pbmc.markers %>%
#     group_by(cluster) %>%
#     dplyr::filter(avg_log2FC > 1) %>%
#     slice_head(n = 10) %>%
#     ungroup() -> top10
# DoHeatmap(so, features = top10$gene) + NoLegend()

## ----Pathway analysis, echo=FALSE---------------------------------------------
# Needs internet connection to access Reactome database
#seurat_reactome <- ReactomeData(so)

## ----Pathways expression------------------------------------------------------
#Uncomment if you run the above chunk of finding markers
#head(seurat_reactome[[2]], n = 3)

## ----Pathways min max---------------------------------------------------------
#Uncomment if you run the above chunk of finding markers
#head(seurat_reactome[[3]], n = 3)

