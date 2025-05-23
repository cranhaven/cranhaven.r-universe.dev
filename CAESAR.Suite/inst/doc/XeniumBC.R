## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    eval = requireNamespace("ProFAST", quietly = TRUE),
    collapse = TRUE,
    comment = "#>"
)

## -----------------------------------------------------------------------------
set.seed(1) # set a random seed for reproducibility.
library(CAESAR.Suite) # load the package of CAESAR.Suite method
library(Seurat)
library(ProFAST)
library(ggplot2)
library(msigdbr)
library(dplyr)

## -----------------------------------------------------------------------------
githubURL <- "https://github.com/XiaoZhangryy/CAESAR.Suite/blob/master/vignettes_data/BC_scRNAList.rda?raw=true"
BC_scRNAList_file <- file.path(tempdir(), "BC_scRNAList.rda")
download.file(githubURL, BC_scRNAList_file, mode='wb')
load(BC_scRNAList_file)

print(BC_scRNAList)

githubURL <- "https://github.com/XiaoZhangryy/CAESAR.Suite/blob/master/vignettes_data/BC_XeniumList.rda?raw=true"
BC_XeniumList_file <- file.path(tempdir(), "BC_XeniumList.rda")
download.file(githubURL, BC_XeniumList_file, mode='wb')
load(BC_XeniumList_file)

print(BC_XeniumList)

githubURL <- "https://github.com/XiaoZhangryy/CAESAR.Suite/blob/master/vignettes_data/BC_feature_imgList.rda?raw=true"
BC_feature_imgList_file <- file.path(tempdir(), "BC_feature_imgList.rda")
download.file(githubURL, BC_feature_imgList_file, mode='wb')
load(BC_feature_imgList_file)

print(sapply(BC_feature_imgList, dim))

## -----------------------------------------------------------------------------
# BC_scRNAList <- lapply(BC_scRNAList,  function(seu) {
#     CreateSeuratObject(
#         counts = seu@assays$RNA@counts,
#         meta.data = seu@meta.data,
#         min.features = 5,
#         min.cells = 1
#     )
# })
# 
# print(BC_scRNAList)
# 
# 
# BC_XeniumList <- lapply(BC_XeniumList,  function(seu) {
#     CreateSeuratObject(
#         counts = seu@assays$RNA@counts,
#         meta.data = seu@meta.data,
#         min.features = 5,
#         min.cells = 1
#     )
# })
# 
# print(BC_XeniumList)
# 
# BC_feature_imgList <- lapply(1:2, function(i) {
#     BC_feature_imgList[[i]][colnames(BC_XeniumList[[i]]), ]
# })

## -----------------------------------------------------------------------------
# align genes
common_genes <- Reduce(intersect, c(
    lapply(BC_scRNAList, rownames),
    lapply(BC_XeniumList, rownames)
))

print(length(common_genes))

# all common genes are used as variable genes, as only around 300 genes here
BC_scRNAList <- lapply(BC_scRNAList, function(seu) {
    seu <- seu[common_genes, ]
    seu <- NormalizeData(seu)
    VariableFeatures(seu) <- common_genes
    seu
})

BC_XeniumList <- lapply(BC_XeniumList, function(seu) {
    seu <- seu[common_genes, ]
    seu <- NormalizeData(seu)
    VariableFeatures(seu) <- common_genes
    seu
})

print(BC_scRNAList)
print(BC_XeniumList)

## -----------------------------------------------------------------------------
BC_scRNAList <- lapply(BC_scRNAList, ProFAST::NCFM, q = 50)

## -----------------------------------------------------------------------------
# calculate cell-gene distance
BC_scRNAList <- lapply(BC_scRNAList, ProFAST::pdistance, reduction = "ncfm")

# identify signature genes
sg_sc_List <- lapply(BC_scRNAList, function(seu) {
    print(table(seu$CellType))

    Idents(seu) <- seu$CellType
    find.sig.genes(seu)
})

str(sg_sc_List)

## -----------------------------------------------------------------------------
markerList <- lapply(sg_sc_List, marker.select, overlap.max = 1)

print(markerList)

## -----------------------------------------------------------------------------
BC_XeniumList <- lapply(1:2, function(i) {
    seu <- BC_XeniumList[[i]]

    # the spatial coordinates
    pos <- seu@meta.data[, c("x_centroid", "y_centroid")]
    print(head(pos))

    # the image feature
    feature_img <- BC_feature_imgList[[i]]

    seu <- CAESAR.coembedding.image(
        seu, feature_img, pos, reduction.name = "caesar", q = 50)
    seu
})
names(BC_XeniumList) <- paste0("BC", 1:2)

print(BC_XeniumList)

## -----------------------------------------------------------------------------
# convert marker list to marker frequency matrix
marker.freq <- markerList2mat(markerList)

# perform annotation using CAESAR and save results to Seurat object
BC_XeniumList <- lapply(
    BC_XeniumList, CAESAR.annotation, marker.freq = marker.freq,
    reduction.name = "caesar", add.to.meta = TRUE
)
print(colnames(BC_XeniumList[[1]]@meta.data))

## -----------------------------------------------------------------------------
# set up colors
cols <- setNames(
    c(
        "#fdc086", "#386cb0", "#b30000", "#FBEA2E", "#731A73",
        "#FF8C00", "#F898CB", "#4DAF4A", "#a6cee3", "#737373"
    ),
    c(
        "B-cells", "CAFs", "Cancer Epithelial", "Endothelial", "Myeloid",
        "Normal Epithelial", "Plasmablasts", "PVL", "T-cells", "unassigned"
    )
)
celltypes <- c(
    "B-cells", "CAFs", "Cancer Epithelial", "Endothelial", "Myeloid",
    "Normal Epithelial", "Plasmablasts", "PVL", "T-cells", "unassigned"
)

BC_XeniumList <- lapply(BC_XeniumList, function(seu) {
    Idents(seu) <- factor(seu$CAESARunasg, levels = celltypes)

    pos <- seu@meta.data[, c("x_centroid", "y_centroid")]
    colnames(pos) <- paste0("pos", 1:2)
    seu@reductions[["pos"]] <- CreateDimReducObject(
        embeddings = as.matrix(pos),
        key = paste0("pos", "_"), assay = "RNA"
    )
    seu
})

## ----fig.width=12, fig.height=15.75-------------------------------------------
plots <- lapply(BC_XeniumList, function(seu) {
    DimPlot(seu, reduction = "pos", cols = cols, pt.size = 1)
})

cowplot::plot_grid(plotlist = plots, ncol = 1)

## ----fig.width=12, fig.height=15.75-------------------------------------------
plots <- lapply(BC_XeniumList, function(seu) {
    FeaturePlot(
        seu,
        reduction = "pos", features = "CAESARconf", pt.size = 1,
        cols = c("blue", "lightgrey"), min.cutoff = 0.0, max.cutoff = 1.0
    )
})

cowplot::plot_grid(plotlist = plots, ncol = 1)

## -----------------------------------------------------------------------------
sg_List <- lapply(BC_XeniumList, find.sig.genes)

str(sg_List)

## -----------------------------------------------------------------------------
dist_names <- paste0("dist_", gsub("-|/| ", ".", setdiff(celltypes, "unassigned")))

distList <- lapply(BC_XeniumList, function(seu) {
    as.matrix(seu@meta.data[, dist_names])
})

seuInt <- CAESAR.RUV(BC_XeniumList, distList, verbose = FALSE, species = "human")

metaInt <- Reduce(rbind, lapply(BC_XeniumList, function(seu) {
    as.matrix(seu@meta.data[, "CAESARunasg", drop = FALSE])
})) %>% as.data.frame()
colnames(metaInt) <- "CAESARunasg"
row.names(metaInt) <- colnames(seuInt)
seuInt <- AddMetaData(seuInt, metaInt, col.name = colnames(metaInt))
Idents(seuInt) <- factor(seuInt$CAESARunasg, levels = names(cols))

print(seuInt)

## ----fig.width=12, fig.height=5-----------------------------------------------
# obtain the top three signature genes
celltypes_plot <- setdiff(celltypes, "unassigned")
top3sgs <- Intsg(sg_List, 3)[celltypes_plot]
print(top3sgs)

sg_features <- unname(unlist(top3sgs))

DotPlot(
    seuInt,
    idents = celltypes_plot, col.min = -1, col.max = 2, dot.scale = 7,
    features = sg_features, scale.min = 0, scale.max = 30
) + theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = 1, hjust = 1))

## ----fig.width=12, fig.height=15.75-------------------------------------------
# calculate coumap
BC_XeniumList <- lapply(
    BC_XeniumList, CoUMAP, reduction = "caesar",
    reduction.name = "caesarUMAP", gene.set = sg_features
)

df_gene_label <- data.frame(
    gene = unlist(top3sgs),
    label = rep(names(top3sgs), each = 3)
)

plots <- lapply(BC_XeniumList, function(seu) {
    CoUMAP.plot(
        seu, reduction = "caesarUMAP", gene_txtdata = df_gene_label,
        cols = c("gene" = "#000000", cols)
    )
})

cowplot::plot_grid(plotlist = plots, ncol = 1)

## -----------------------------------------------------------------------------
# pathway_list <- msigdbr(species = "Homo sapiens", category = "C5", subcategory = "GO:BP") %>%
#     group_by(gs_name) %>%
#     summarise(genes = list(intersect(gene_symbol, common_genes))) %>%
#     tibble::deframe()
# n.pathway_list <- sapply(pathway_list, length)
# pathway_list <- pathway_list[n.pathway_list >= 5]

# --------------------------------------------
# To avoid potential issues caused by differences in operating systems,
# R package versions, and other uncontrollable factors across environments,
# we pre-generated the 'pathway_list' object using the code above
# --------------------------------------------

githubURL <- "https://github.com/XiaoZhangryy/CAESAR.Suite/blob/master/vignettes_data/pathway4BC.rda?raw=true"
pathway4BC_file <- file.path(tempdir(), "pathway4BC.rda")
download.file(githubURL, pathway4BC_file, mode='wb')
load(pathway4BC_file)

print(head(pathway_list))

## -----------------------------------------------------------------------------
seuBC1 <- BC_XeniumList[[1]]

df_enrich <- CAESAR.enrich.pathway(
    seuBC1, pathway.list = pathway_list, reduction = "caesar"
)

# obtain significant enriched pathways
pathways <- pathway_list[df_enrich$asy.wei.pval.adj < 0.05]

## -----------------------------------------------------------------------------
enrich.score.BC1 <- CAESAR.enrich.score(seuBC1, pathways)

dep.pvals <- CAESAR.CTDEP(seuBC1, enrich.score.BC1)
head(dep.pvals)

## ----fig.width=12, fig.height=7.5---------------------------------------------
seuBC1 <- AddMetaData(seuBC1, as.data.frame(enrich.score.BC1))

pathway <- "GOBP_VASCULATURE_DEVELOPMENT"
FeaturePlot(seuBC1, features = pathway, reduction = "pos") +
    scale_color_gradientn(
        colors = c("#fff7f3", "#fcc5c0", "#f768a1", "#ae017e", "#49006a"),
        values = scales::rescale(seq(0, 1, 0.25)),
        limits = c(0, 1)
    ) +
    theme(
        legend.position = "right",
        legend.justification = "center",
        legend.box = "vertical"
    )

## -----------------------------------------------------------------------------
sessionInfo()

