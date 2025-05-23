## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    eval = requireNamespace("ProFAST", quietly = TRUE),
    collapse = TRUE,
    comment = "#>"
)

## -----------------------------------------------------------------------------
githubURL <- "https://github.com/XiaoZhangryy/CAESAR.Suite/blob/master/vignettes_data/MOB_ST.rda?raw=true"
MOB_ST_file <- file.path(tempdir(), "MOB_ST.rda")
download.file(githubURL, MOB_ST_file, mode='wb')
load(MOB_ST_file)

print(MOB_ST)

githubURL <- "https://github.com/XiaoZhangryy/CAESAR.Suite/blob/master/vignettes_data/MOB_scRNA.rda?raw=true"
MOB_scRNA_file <- file.path(tempdir(), "MOB_scRNA.rda")
download.file(githubURL, MOB_scRNA_file, mode='wb')
load(MOB_scRNA_file)

print(MOB_scRNA)

## -----------------------------------------------------------------------------
set.seed(1) # set a random seed for reproducibility.
library(CAESAR.Suite) # load the package of CAESAR method
library(Seurat)
library(ProFAST)
library(ggplot2)

## -----------------------------------------------------------------------------
MOB_ST <- CreateSeuratObject(
    counts = MOB_ST@assays$RNA@counts,
    meta.data = MOB_ST@meta.data,
    min.features = 5,
    min.cells = 1
)

print(MOB_ST)

MOB_scRNA <- CreateSeuratObject(
    counts = MOB_scRNA@assays$RNA@counts,
    meta.data = MOB_scRNA@meta.data,
    min.features = 5,
    min.cells = 1
)

print(MOB_scRNA)

## -----------------------------------------------------------------------------
# align genes
common_genes <- intersect(rownames(MOB_ST), rownames(MOB_scRNA))
MOB_ST <- MOB_ST[common_genes, ]
MOB_scRNA <- MOB_scRNA[common_genes, ]

print(length(common_genes))

MOB_ST <- NormalizeData(MOB_ST)
MOB_ST <- FindVariableFeatures(MOB_ST, nfeatures = 2000)

MOB_scRNA <- NormalizeData(MOB_scRNA)
MOB_scRNA <- FindVariableFeatures(MOB_scRNA, nfeatures = 2000)

common_vgs <- intersect(VariableFeatures(MOB_ST), VariableFeatures(MOB_scRNA))
VariableFeatures(MOB_ST) <- common_vgs
VariableFeatures(MOB_scRNA) <- common_vgs

print(length(common_vgs))

## -----------------------------------------------------------------------------
MOB_scRNA <- ProFAST::NCFM(MOB_scRNA, q = 50)

## -----------------------------------------------------------------------------
# calculate cell-gene distance
MOB_scRNA <- ProFAST::pdistance(MOB_scRNA, reduction = "ncfm")

# identify signature genes
print(table(MOB_scRNA$CellType))

Idents(MOB_scRNA) <- MOB_scRNA$CellType
sg_sc_List <- find.sig.genes(MOB_scRNA)

str(sg_sc_List)

## -----------------------------------------------------------------------------
marker <- marker.select(sg_sc_List, overlap.max = 1)
print(marker)

## -----------------------------------------------------------------------------
# the spatial coordinates
pos <- MOB_ST@meta.data[, c("x", "y")]
print(head(pos))

MOB_ST <- CAESAR.coembedding(MOB_ST, pos, reduction.name = "caesar", q = 50)
print(MOB_ST)

## -----------------------------------------------------------------------------
# convert marker list to marker frequency matrix
marker.freq <- markerList2mat(list(marker))

# perform annotation using CAESAR and save results to Seurat object
print(colnames(MOB_ST@meta.data))

MOB_ST <- CAESAR.annotation(MOB_ST, marker.freq, reduction.name = "caesar", add.to.meta = TRUE)
print(colnames(MOB_ST@meta.data))

## -----------------------------------------------------------------------------
# set up colors
cols_manual <- setNames(
    c(
        "#4374A5", "#FCDDDE", "#2AB67F", "#F08A21", "#737373"
    ),
    c(
        "GCL", "MCL", "ONL", "GL", "Unknown"
    )
)

celltypes_manual <- c("GCL", "MCL", "ONL", "GL", "Unknown")

cols <- setNames(
    c(
        "#4374A5", "#FCDDDE", "#2AB673", "#F08A21", "#E04D50", "#737373"
    ),
    c(
        "GC", "M/TC", "OSNs", "PGC", "EPL-IN", "unassigned"
    )
)

celltypes <- c("GC", "M/TC", "OSNs", "PGC", "EPL-IN", "unassigned")

colnames(pos) <- paste0("pos", 1:2)
MOB_ST@reductions[["pos"]] <- CreateDimReducObject(
    embeddings = as.matrix(pos),
    key = paste0("pos", "_"), assay = "RNA"
)

## ----fig.width=8.8, fig.height=6----------------------------------------------
Idents(MOB_ST) <- factor(MOB_ST$manual_annotation, levels = celltypes_manual)
DimPlot(MOB_ST, reduction = "pos", cols = cols_manual, pt.size = 8)

## ----fig.width=8.8, fig.height=6----------------------------------------------
Idents(MOB_ST) <- factor(MOB_ST$CAESAR, levels = celltypes)
DimPlot(MOB_ST, reduction = "pos", cols = cols, pt.size = 8)

## ----fig.width=8.8, fig.height=6----------------------------------------------
Idents(MOB_ST) <- factor(MOB_ST$CAESARunasg, levels = celltypes)
DimPlot(MOB_ST, reduction = "pos", cols = cols, pt.size = 8)

## ----fig.width=8.8, fig.height=6----------------------------------------------
FeaturePlot(
    MOB_ST,
    reduction = "pos", features = "CAESARconf", pt.size = 8,
    cols = c("blue", "lightgrey"), min.cutoff = 0.0, max.cutoff = 1.0
)

## ----fig.width=8.8, fig.height=9----------------------------------------------
caesar_prob <- colnames(MOB_ST@meta.data)[15:19]
print(caesar_prob)

plots <- lapply(caesar_prob, function(feature) {
    FeaturePlot(MOB_ST, features = feature, reduction = "pos", pt.size = 3.5) +
        scale_color_gradientn(
            colors = c("#f6eff7", "#feebe2", "#f768a1", "#7a0177", "#6e016b"),
            values = scales::rescale(c(0.0, 0.125, 0.25, 0.375, 0.50)),
            limits = c(0.0, 0.50)
        ) + labs(title = feature)
})

cowplot::plot_grid(plotlist = plots, ncol = 2)

## -----------------------------------------------------------------------------
acc_st <- function(manual_annotation, pred) {
    manual_annotation <- as.character(manual_annotation)
    pred <- as.character(pred)
    manual_annotation[manual_annotation == "GCL"] <- "GC"
    manual_annotation[manual_annotation == "MCL"] <- "M/TC"
    manual_annotation[manual_annotation == "ONL"] <- "OSNs"
    manual_annotation[manual_annotation == "GL"] <- "PGC"
    return(mean(manual_annotation == pred))
}

print(paste0(
    "The ACC of CAESAR annotation is ",
    acc_st(MOB_ST$manual_annotation, MOB_ST$CAESARunasg)
))

## -----------------------------------------------------------------------------
Idents(MOB_ST) <- factor(MOB_ST$CAESARunasg, celltypes)
sg_List <- find.sig.genes(MOB_ST)

str(sg_List)

## ----fig.width=8.8, fig.height=5----------------------------------------------
# obtain the top three signature genes
celltypes_plot <- setdiff(names(sg_List), "unassigned")
top3sgs <- Intsg(list(sg_List), 3)[celltypes_plot]
print(top3sgs)

sg_features <- unname(unlist(top3sgs))

DotPlot(
    MOB_ST,
    idents = celltypes_plot, col.min = -1, col.max = 2, dot.scale = 7,
    features = sg_features, scale.min = 0, scale.max = 30
) + theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = 1, hjust = 1))

## ----fig.width=8.8, fig.height=6----------------------------------------------
# calculate coumap
MOB_ST <- CoUMAP(
    MOB_ST, reduction = "caesar", reduction.name = "caesarUMAP",
    gene.set = sg_features
)

df_gene_label <- data.frame(
    gene = unlist(top3sgs),
    label = rep(names(top3sgs), each = 3)
)

CoUMAP.plot(
    MOB_ST, reduction = "caesarUMAP", gene_txtdata = df_gene_label,
    cols = c("gene" = "#000000", cols)
)

## -----------------------------------------------------------------------------
sessionInfo()

