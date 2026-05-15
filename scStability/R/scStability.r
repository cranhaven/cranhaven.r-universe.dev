#' A user friendly wrapper function that runs the entire scRNA-seq stability workflow and shows statistics for each step
#'
#' @description A wrapper function that runs all other stability analysis functions in order.
#' Statistics for each step are printed accordingly and a final DR and cluster plot is shown
#' which represents the medoid embeddings and cluster assignments that were generated.
#'
#' @param seurat_obj A Seurat object containing scRNA-seq data and a PCA
#' @param n_runs Number of DR embeddings and number of cluster assignments to be generated (< 250 recommended)
#' @param dr_method Method to use for dimension reduction, either "umap" or "tsne"
#' @param clust_method Algorithm used for clustering, either "louvain" or "leiden"
#' @param n_cores Number of CPU cores to use for parallelising functions
#' @param verbose Whether the function should print summary statistics as it calculates them
#' @param print_plot Whether the final medoid plot should be printed
#' @param seeds A set of seeds of length n_runs used for generating embeddings and clusters
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_violin labs theme_minimal theme element_blank ggtitle
#'
#' @return A list containing:
#'   \item{mean_emb}{Data frame containing the mean embedding coordinates}
#'   \item{mean_clust}{Vector of the mean cluster assignments}
#'   \item{plot}{ggplot2 object with the medoid embedding plot and cluster assignments}
#'   \item{embedding_stats}{List of embedding statistics}
#'   \item{cluster_stats}{List of clustering statistics}
#'   \item{seurat_object}{Seurat object now containing mean embeddings and mean clusters}
#'
#' @export
scStability <- function(
    seurat_obj,
    n_runs       = 100,
    dr_method    = "umap",
    clust_method = "louvain",
    n_cores      = 1,
    verbose      = TRUE,
    print_plot   = TRUE,
    seeds        = NULL
    ) {
  # Safety Checks
  if (!inherits(seurat_obj, "Seurat")) {
    stop("'seurat_obj' must be a Seurat object.", call. = FALSE)
  }

  if (!is.numeric(n_runs) || n_runs <= 0 || n_runs != round(n_runs)) {
    stop("'n_runs' must be a positive integer.", call. = FALSE)
  }

  if (n_runs > 250) {
    warning("Using a large number of runs (>250) may result in long computation times.", call. = FALSE)
  }

  if (!is.numeric(n_cores) || n_cores <= 0 || n_cores != round(n_cores)) {
    stop("'n_cores' must be a positive integer.", call. = FALSE)
  }

  if (!"pca" %in% names(seurat_obj@reductions)) {
    stop("PCA reduction not found in Seurat object. Please run 'RunPCA()' first.", call. = FALSE)
  }

  # Dependency checks
  for (pkg in c("Seurat", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg), call. = FALSE)
    }
  }

  # Increase allocated memory in case of large scRNA-seq dataframes
  options(future.globals.maxSize = 1000 * 1024^2)

  if (is.null(seeds)) {
    seeds <- 100:(100 + n_runs)
  }

  # Extract the pca embedding coordinates from the seurat object
  pca_emb <- Seurat::Embeddings(seurat_obj, reduction = "pca")

  # Generate n embeddings based on user input methods
  emb_list <- scStability::createEmb(dr_input = pca_emb, n_runs = n_runs, method = dr_method, n_cores = n_cores,
                                     seeds = seeds)
  if(verbose == TRUE){
    cat("-----------------------Embedding Statistics----------------------------- \n")
  }


  # Compare embeddings and print statistics
  emb_stats <- tryCatch({
    scStability::compareEmb(emb_list, n_cores = n_cores, verbose = verbose, print_plot = print_plot)
  }, error = function(e) {
    stop(sprintf("Error comparing embeddings: %s", e$message), call. = FALSE)
  })
  embedding_mean <- emb_stats$mean
  if(verbose == TRUE){
    cat("------------------------Cluster Statistics------------------------------ \n")
  }


  # Mean Kendall's tau from the 95% confidence interval means
  emb_mean_estimate <- mean(emb_stats$mean_per_embedding[emb_stats$mean_per_embedding > emb_stats$ci[1]
                                                     & emb_stats$mean_per_embedding < emb_stats$ci[2]])

  # Find the index that is the closest to this mean value
  emb_closest_index <- which.min(abs(emb_stats$mean_per_embedding - emb_mean_estimate))

  # Take the embedding that is the closest to the mean of the set of embeddings
  mean_emb <- as.data.frame(emb_list[emb_closest_index])
  colnames(mean_emb) <- paste0(toupper(dr_method), "_", 1:2)

  # Ensure embeddings have rownames that match cell IDs
  if (is.null(rownames(mean_emb))) {
    rownames(mean_emb) <- colnames(seurat_obj)
  }

  # Generate and compare the clusters using the input seurat object
  clust_stats <- tryCatch({
    scStability::clustStable(seurat_obj, method = clust_method, n_runs = n_runs,
                n_cores = n_cores, verbose = verbose, print_plot = print_plot, seeds = seeds)
  }, error = function(e) {
    stop(sprintf("Error generating cluster assignments: %s", e$message), call. = FALSE)
  })
  cluster_mean <- mean(clust_stats$per_index_means)

  # Find the mean cluster assignment
  clust_mean_estimate <- mean(clust_stats$per_index_means[
    clust_stats$per_index_means >= as.numeric(clust_stats$ci[1]) &
      clust_stats$per_index_means <= as.numeric(clust_stats$ci[2])
  ])


  # Take the clusters labels from the closest to mean clusters
  clust_closest_index <- which.min(abs(clust_stats$per_index_means - clust_mean_estimate))

  mean_clust <- clust_stats$cluster_labels[[clust_closest_index]]


  # Plot the mean clusters on the mean embeddings
  reduction_key <- ifelse(dr_method == "umap", "UMAP_", "tSNE_")

  mean_emb_matrix <- as.matrix(mean_emb)
  if (is.null(rownames(mean_emb_matrix))) {
    rownames(mean_emb_matrix) <- colnames(seurat_obj)
  }

  seurat_obj[[dr_method]] <- Seurat::CreateDimReducObject(
    embeddings = mean_emb_matrix,
    key = reduction_key,
    assay = Seurat::DefaultAssay(seurat_obj)
  )

  # Ensure cell names match
  if (!all(names(mean_clust) == colnames(seurat_obj))) {
    if (length(mean_clust) == ncol(seurat_obj)) {
      names(mean_clust) <- colnames(seurat_obj)
    } else {
      warning("Cell names in clusters don't match Seurat object. Using as-is.", call. = FALSE)
    }
  }

  seurat_obj$mean_clusters <- as.factor(mean_clust)

  subt <- sprintf("Medoid Embedding Correlation: %.4f, Medoid Cluster NMI %.4f", embedding_mean, cluster_mean)
  p <- Seurat::DimPlot(seurat_obj, group.by = "mean_clusters") +
    ggplot2::ggtitle("Medoid Embeddings with Medoid Clusters", subtitle = subt) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  if(print_plot == TRUE){
    print(p)
  }


  return(list(
    mean_emb = mean_emb,
    mean_clust = mean_clust,
    plot = p,
    embedding_stats = emb_stats,
    cluster_stats = clust_stats,
    seurat_object = seurat_obj
  ))

}
