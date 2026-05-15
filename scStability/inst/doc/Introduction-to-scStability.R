## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
have_bioc <-
  requireNamespace("scRNAseq", quietly=TRUE) &&
  requireNamespace("SummarizedExperiment", quietly=TRUE)
if (!have_bioc) {
  message("Skipping entire vignette: please install scRNAseq & SummarizedExperiment from Bioconductor")
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(scStability)
  library(scRNAseq)
  library(SummarizedExperiment)
}

## -----------------------------------------------------------------------------
if (have_bioc){
  sce <- scRNAseq::ZeiselBrainData(location = FALSE)
  counts_matrix <- SummarizedExperiment::assay(sce, "counts")
}

## -----------------------------------------------------------------------------
library(Seurat)
seurat_obj <- Seurat::CreateSeuratObject(counts = counts_matrix)
cell_names <- colnames(seurat_obj@assays$RNA)[1:200]
seurat_obj <- subset(seurat_obj, cells = cell_names)

seurat_obj <- Seurat::NormalizeData(seurat_obj)
seurat_obj <- Seurat::FindVariableFeatures(seurat_obj)
seurat_obj <- Seurat::ScaleData(seurat_obj, verbose = FALSE)

# We need a PCA as the input to the functions and so we will create one using Seurat::RunPCA(). Recommended to use as few principal components as is logical to speed up computation. You can check the number of PCs needed using a scree plot.
seurat_obj <- Seurat::RunPCA(seurat_obj, npcs = 10)

# Extract the PCA also for use in embedding functions
input_pca <- Seurat::Embeddings(seurat_obj, reduction = "pca")


## -----------------------------------------------------------------------------
# Run the wrapper function with 100 different embeddings and cluster assignments being created and compared
stability_results <- scStability::scStability(seurat_obj, n_runs = 15, dr_method = 'umap', clust_method = 'louvain', n_cores = 8)


# As the function runs, output statistics will be printed. A density plot of the mean Kendall's tau per embedding and a violin plot of the cluster assignment NMI will also be printed.


# To see what the mean embedding with the mean cluster assignment is, we can print the plot created
print(stability_results$plot)

# For statistics on each stage, we can extract the following:
embedding_statistics <- stability_results$emb_stats
cluster_statistics <- stability_results$clust_stats

# Look at the mean Kendall's tau (correlation between embeddings) for each embedding
print(embedding_statistics$mean_per_embedding)

# Also look at the overall mean of means Kendall's tau
print(embedding_statistics$mean)

# Look at the mean NMI (similar to correlation) for each cluster assignment
print(cluster_statistics$per_index_means)

# Can also see the mean NMI over all the cluster assignments
print(mean(cluster_statistics$per_index_means))

## -----------------------------------------------------------------------------
# Create a list of embeddings using the input pca
emb_list <- scStability::createEmb(input_pca, n_runs = 15, method = 'umap', n_cores = 8)

# Compare the generated embedding list and look at the output statistics. A density plot will be generated which shows the density of the mean Kendall's tau for each embedding. 
embedding_stats <- scStability::compareEmb(emb_list, n_cores = 8)

# Create and compare cluster assignments. A violin plot showing the distribution of mean NMI values
cluster_stats <- scStability::clustStable(n_runs = 15, seurat_obj = seurat_obj, method = 'louvain', n_cores = 8)

