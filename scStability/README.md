
# scStability

<!-- badges: start -->
<!-- badges: end -->

scStability is a user friendly package to analyse the stability of dimension reduction and creating cell clusters on your own dataset.
Handles UMAP or t-SNE for dimension reduction given an input PCA. Handles Louvain or Leiden algorithm for cell cluster assignment.

## Installation

You can install the development version of scStability like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

A basic workflow should follow a simillar structure to the following:

``` r
library(scStability)
filtered_norm_counts <- readRDS(filterednormcounts.csv)

seurat_obj <- CreateSeuratObject(filtered_norm_counts)

seurat_obj <- RunPCA(seurat_obj)

# Run the wrapper function to compute the entire scStability pipeline
stability_results <- scStability(seurat_obj, n_runs = 100, dr_method = 'umap', clust_method = 'louvain', n_cores = 2)

# Print the mean embedding plot with the mean cluster assignments
print(stability_results$plot)

# Look at embedding and cluster stability statistics
print(stability_results$embedding_stats)
print(stability_results$cluster_stats)

```

## Example

A more detailed example workflow which can be ran in sections:

``` r
library(scStability)
filtered_norm_counts <- readRDS(filterednormcounts.csv)

seurat_obj <- CreateSeuratObject(filtered_norm_counts)

seurat_obj <- RunPCA(seurat_obj)

# Create a list of n_runs embeddings (recommonded < 300)
pca_emb <- Seurat::Embeddings(seurat_obj, reduction = "pca")
emb_list <- createEmb(dr_input = pca_emb, n_runs = 200, method = 'umap', n_cores = 2)

# Compare the set of embeddings and look at printed statistics and density plot
emb_stats <- compareEmb(emb_list, n_cores = 2)

# Manually check the Kendall's tau of each embedding
print(emb_stats$mean_per_embedding)

# Create and compare the cluster assignments on the previously made PCA. Look at printed statistics
clust_stats <- clustStable(seurat_obj)

# Manually check the mean NMI of each cluster assignment
clust_stats$per_index_means



```

