## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install Depedency, eval = FALSE------------------------------------------
#  
#  # Check if BioManager is installed, install if not
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  
#  # Check if 'batchelor' is installed, install if not
#  if (!requireNamespace("batchelor", quietly = TRUE))
#      BiocManager::install("batchelor")
#  
#  # Check if 'MatrixGenerics' is installed, install if not
#  if (!requireNamespace("MatrixGenerics", quietly = TRUE))
#      BiocManager::install("MatrixGenerics")
#  

## ----install SCIntRuler, eval = FALSE-----------------------------------------
#  BiocManager::install("SCIntRuler")

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(SCIntRuler)
library(Seurat)
library(dplyr)
library(ggplot2)

## ----load the data, message=FALSE, warning=FALSE------------------------------
data("sim_data_sce", package = "SCIntRuler")
sim_data <- as.Seurat(sim_data_sce)

## ----show some data, message=FALSE, warning=FALSE-----------------------------
head(sim_data[[]])

## ----pre-process , message=FALSE, warning=FALSE-------------------------------
# Normalize the data
sim_data <- NormalizeData(sim_data)
# Identify highly variable features
sim_data <- FindVariableFeatures(sim_data, selection.method = "vst", nfeatures = 2000)
# Scale the data
all.genes <- rownames(sim_data)
sim_data <- ScaleData(sim_data, features = all.genes)
# Perform linear dimensional reduction
sim_data <- RunPCA(sim_data, features = VariableFeatures(object = sim_data))
# Cluster the cells
sim_data <- FindNeighbors(sim_data, dims = 1:20)
sim_data <- FindClusters(sim_data, resolution = 0.5)
sim_data <- RunUMAP(sim_data, dims = 1:20)

## ----UMAP , message=FALSE, warning=FALSE--------------------------------------
p1 <- DimPlot(sim_data, reduction = "umap", label = FALSE, pt.size = .5, group.by = "Study", repel = TRUE)
p1

## ----UMAP2 , message=FALSE, warning=FALSE-------------------------------------

p2 <- DimPlot(sim_data, reduction = "umap", label = TRUE, pt.size = .8, group.by = "CellType", repel = TRUE)
p2


## ----CCA, message=FALSE, warning=FALSE----------------------------------------
### CCA
sim.list <- SplitObject(sim_data, split.by = "Study")
sim.anchors <- FindIntegrationAnchors(object.list = sim.list, dims = 1:30, reduction = "cca")

sim.int <- IntegrateData(anchorset = sim.anchors, dims = 1:30, new.assay.name = "CCA")
# run standard analysis workflow
sim.int <- ScaleData(sim.int, verbose = FALSE)
sim.int <- RunPCA(sim.int, npcs = 30, verbose = FALSE)
sim.int <- RunUMAP(sim.int, dims = 1:30, reduction.name = "umap_cca")


## ----harmony, message=FALSE, warning=FALSE------------------------------------
### Harmony
# install.packages("harmony")

sim.harmony <- harmony::RunHarmony(sim_data, group.by.vars = "Study", reduction.use = "pca",
                                   #dims.use = 1:20, assay.use = "RNA"
    )

sim.int[["harmony"]] <- sim.harmony[["harmony"]]
sim.int <- RunUMAP(sim.int, dims = 1:20, reduction = "harmony", reduction.name = "umap_harmony")


## ----integration, fig.height=8, fig.width=14----------------------------------
p5 <- DimPlot(sim_data, reduction = "umap", group.by = "Study") +
  theme(legend.position = "none",
        # axis.line.y = element_line( size = 2, linetype = "solid"),
        # axis.line.x = element_line( size = 2, linetype = "solid"),
        axis.text.y = element_text( color="black", size=20),
        axis.title.y = element_text(color="black", size=20),
        axis.text.x = element_text( color="black", size=20),
        axis.title.x = element_text(color="black", size=20))
p6 <- DimPlot(sim.int, reduction = "umap_cca", group.by = "Study") +
  theme(legend.position = "none",
        # axis.line.y = element_line( size = 2, linetype = "solid"),
        # axis.line.x = element_line( size = 2, linetype = "solid"),
        axis.text.y = element_text( color="black", size=20),
        axis.title.y = element_text(color="black", size=20),
        axis.text.x = element_text( color="black", size=20),
        axis.title.x = element_text(color="black", size=20))
p7 <- DimPlot(sim.int, reduction = "umap_harmony", group.by = "Study") +
  theme(legend.position = "none",
        # axis.line.y = element_line( size = 2, linetype = "solid"),
        # axis.line.x = element_line( size = 2, linetype = "solid"),
        axis.text.y = element_text( color="black", size=20),
        axis.title.y = element_text(color="black", size=20),
        axis.text.x = element_text( color="black", size=20),
        axis.title.x = element_text(color="black", size=20))

leg <- cowplot::get_legend(p5)
gridExtra::grid.arrange(gridExtra::arrangeGrob(p5 + NoLegend() + ggtitle("Unintegrated"), 
                                               p6 + NoLegend() + ggtitle("Seurat CCA") , 
                                               p7 + NoLegend() + ggtitle("Harmony"),
                                               #p8 + NoLegend() + ggtitle("Scanorama"), 
                                               nrow = 2),
    leg, ncol = 2, widths = c(20, 5))


## ----step 1, warning=FALSE,message=FALSE--------------------------------------

sim.list <- SplitObject(sim_data, split.by = "Study")
fullcluster <- GetCluster(sim.list,50,200)

normCount <- NormData(sim.list)

#distmat <- FindNNDist(fullcluster, normCount, 20)
distmat <- FindNNDistC(fullcluster, normCount, 20)


## ----step 2, warning=FALSE,message=FALSE--------------------------------------

testres <- PermTest(fullcluster,distmat,15)
PlotSCIR(fullcluster,sim.list,testres,"")

sim_result <- list(fullcluster,normCount,distmat,testres)


## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

