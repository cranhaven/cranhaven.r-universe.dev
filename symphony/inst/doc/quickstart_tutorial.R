## ----eval=FALSE---------------------------------------------------------------
# install.packages('symphony')

## -----------------------------------------------------------------------------
library(symphony)

# Other packages for this tutorial
suppressPackageStartupMessages({
    # Analysis
    library(harmony)
    library(irlba)
    library(data.table)
    library(dplyr)

    # Plotting
    library(ggplot2)
    library(ggthemes)
    library(RColorBrewer)
})

## ----echo=FALSE---------------------------------------------------------------
plotBasic = function(umap_labels,                # metadata, with UMAP labels in UMAP1 and UMAP2 slots
                        title = 'Query',         # Plot title
                        color.by = 'cell_type',  # metadata column name for coloring
                        facet.by = NULL,         # (optional) metadata column name for faceting
                        color.mapping = NULL,    # custom color mapping
                        legend.position = 'right') {  # Show cell type legend
    
    p = umap_labels %>%
            dplyr::sample_frac(1L) %>% # permute rows randomly
            ggplot(aes(x = UMAP1, y = UMAP2)) +
            geom_point(aes(col = get(color.by)), size = 1, stroke = 0.4, shape = 16)
        if (!is.null(color.mapping)) { p = p + scale_color_manual(values = color.mapping) }
    
    # Default formatting
    p = p + theme_bw() +
            labs(title = title, color = color.by) + 
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position=legend.position) +
            theme(legend.text = element_text(size=8), legend.title=element_text(size=12)) + 
            guides(colour = guide_legend(override.aes = list(size = 4))) + guides(alpha = 'none')

    if(!is.null(facet.by)) {
        p = p + facet_wrap(~get(facet.by)) +
                theme(strip.text.x = element_text(size = 12)) }    
    return(p)
}

# Colors for PBMCs
pbmc_colors = c("B" = "#66C2A5", "DC" = "#FC8D62", "HSC" = "#8DA0CB", "MK" = "#E78AC3", 
              "Mono_CD14" = "#A6D854", "Mono_CD16" = "#f2ec72", "NK" = "#62AAEA", 
              "T_CD4" = "#D1C656", "T_CD8" = "#968763")

## -----------------------------------------------------------------------------
load('../data/pbmcs_exprs_small.rda')
load('../data/pbmcs_meta_small.rda')

dim(pbmcs_exprs_small)
dim(pbmcs_meta_small)

pbmcs_meta_small %>% head(4)

## -----------------------------------------------------------------------------
idx_query = which(pbmcs_meta_small$donor == "5p") # use 5' dataset as the query
ref_exp_full = pbmcs_exprs_small[, -idx_query]
ref_metadata = pbmcs_meta_small[-idx_query, ]
query_exp = pbmcs_exprs_small[, idx_query]
query_metadata = pbmcs_meta_small[idx_query, ]

## -----------------------------------------------------------------------------
ref_exp_full[1:5, 1:2] # Sparse matrix with the normalized genes x cells matrix

## -----------------------------------------------------------------------------
var_genes = vargenes_vst(ref_exp_full, groups = as.character(ref_metadata[['donor']]), topn = 1000)
ref_exp = ref_exp_full[var_genes, ]
dim(ref_exp)

## -----------------------------------------------------------------------------
vargenes_means_sds = tibble(symbol = var_genes, mean = Matrix::rowMeans(ref_exp))
vargenes_means_sds$stddev = rowSDs(ref_exp, vargenes_means_sds$mean)
head(vargenes_means_sds)

## -----------------------------------------------------------------------------
ref_exp_scaled = scaleDataWithStats(ref_exp, vargenes_means_sds$mean, vargenes_means_sds$stddev, 1)

## -----------------------------------------------------------------------------
set.seed(0)
s = irlba(ref_exp_scaled, nv = 20)
Z_pca_ref = diag(s$d) %*% t(s$v) # [pcs by cells]
loadings = s$u

## -----------------------------------------------------------------------------
set.seed(0)
ref_harmObj = harmony::RunHarmony(
        data_mat = t(Z_pca_ref),  ## PCA embedding matrix of cells
        meta_data = ref_metadata, ## dataframe with cell labels
        theta = c(2),             ## cluster diversity enforcement
        vars_use = c('donor'),    ## variable to integrate out
        nclust = 100,             ## number of clusters in Harmony model
        max_iter = 20,            ## max number of iterations
        return_object = TRUE      ## return the full Harmony model object
)

## -----------------------------------------------------------------------------
# Compress a Harmony object into a Symphony reference
reference = buildReferenceFromHarmonyObj(
                        ref_harmObj,            # output object from RunHarmony()
                        ref_metadata,           # reference cell metadata
                        vargenes_means_sds,     # gene names, means, and std devs for scaling
                        loadings,               # genes x PCs matrix
                        verbose = TRUE,         # verbose output
                        do_umap = TRUE,         # set to TRUE to run UMAP
                        save_uwot_path = './testing_uwot_model_1') # file path to save uwot model

## -----------------------------------------------------------------------------
saveRDS(reference, './testing_reference1.rds')

## -----------------------------------------------------------------------------
str(reference)

## -----------------------------------------------------------------------------
dim(reference$Z_corr)
reference$Z_corr[1:5, 1:5]

## ----fig.width = 5.5, fig.height = 4------------------------------------------
reference = readRDS('./testing_reference1.rds')
umap_labels = cbind(ref_metadata, reference$umap$embedding)
plotBasic(umap_labels, title = 'Reference', color.mapping = pbmc_colors)

## -----------------------------------------------------------------------------
# Build reference
set.seed(0)
reference = symphony::buildReference(
            ref_exp_full,
            ref_metadata,
            vars = c('donor'),         # variables to integrate over
            K = 100,                   # number of Harmony clusters
            verbose = TRUE,            # verbose output
            do_umap = TRUE,            # can set to FALSE if want to run umap separately later
            do_normalize = FALSE,      # set to TRUE if input counts are not normalized yet
            vargenes_method = 'vst',   # method for variable gene selection ('vst' or 'mvp')
            vargenes_groups = 'donor', # metadata column specifying groups for variable gene selection 
            topn = 1000,               # number of variable genes to choose per group
            d = 20,                    # number of PCs
            save_uwot_path = './testing_uwot_model_2' # file path to save uwot model
)

# Save reference (modify with your desired output path)
saveRDS(reference, './testing_reference2.rds')

## ----fig.width = 5.5, fig.height = 4------------------------------------------
reference = readRDS('./testing_reference2.rds')
umap_labels = cbind(ref_metadata, reference$umap$embedding)
plotBasic(umap_labels, title = 'Reference', color.mapping = pbmc_colors)

## -----------------------------------------------------------------------------
# Read in Symphony reference to map to
reference = readRDS('./testing_reference1.rds')
# Map query
query = mapQuery(query_exp,             # query gene expression (genes x cells)
                 query_metadata,        # query metadata (cells x attributes)
                 reference,             # Symphony reference object
                 do_normalize = FALSE,  # perform log(CP10k) normalization on query
                 do_umap = TRUE)        # project query cells into reference UMAP

## -----------------------------------------------------------------------------
str(query)

## -----------------------------------------------------------------------------
query = knnPredict(query,       # query object
                   reference,   # reference object
                   reference$meta_data$cell_type, # reference cell labels for training
                   k = 5,       # number of reference neighbors to use for prediction
                   confidence = TRUE)

## -----------------------------------------------------------------------------
head(query$meta_data)

## ----fig.width = 5.5, fig.height = 4------------------------------------------
# Sync the column names for both data frames
reference$meta_data$cell_type_pred_knn = NA
reference$meta_data$cell_type_pred_knn_prob = NA
reference$meta_data$ref_query = 'reference'
query$meta_data$ref_query = 'query'

# Add the UMAP coordinates to the metadata
meta_data_combined = rbind(query$meta_data, reference$meta_data)
umap_combined = rbind(query$umap, reference$umap$embedding)
umap_combined_labels = cbind(meta_data_combined, umap_combined)

# Plot UMAP visualization of all cells
plotBasic(umap_combined_labels, title = 'Reference and query cells', color.by = 'ref_query')

## ----fig.width = 7, fig.height = 4--------------------------------------------
plotBasic(umap_combined_labels, title = 'Reference and query cells', 
          color.mapping = pbmc_colors, facet.by = 'ref_query')

## -----------------------------------------------------------------------------
sessionInfo()

