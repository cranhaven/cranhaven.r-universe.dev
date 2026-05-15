#' Create and compare multiple clustering runs on scRNA-seq data
#'
#' @description
#' Generate multiple clustering iterations on a Seurat object containing scRNA-seq data
#' using the provided dimensionality reduction. The function creates a shared nearest
#' neighbor (SNN) graph and assigns clusters using the specified algorithm, then calculates
#' stability metrics across iterations.
#'
#' @param seurat_obj A Seurat object containing scRNA-seq data with a PCA reduction
#' @param method Character string specifying the clustering algorithm to use:
#'        either "louvain" or "leiden"
#' @param n_runs Integer specifying the number of cluster assignments to generate (default: 100)
#' @param resolution Numeric value specifying the clustering resolution parameter (default: 0.8)
#' @param dims Integer vector specifying which PCA dimensions to use (default: 1:10)
#' @param n_cores Integer specifying the number of CPU cores to use for parallelization (default: 1)
#' @param verbose Whether the function should print summary statistics as it calculates them
#' @param print_plot Whether the final violin plot should be automatically printed
#' @param seeds A set of seeds of length n_runs for creating clusters
#'
#' @importFrom ggplot2 ggplot aes geom_violin labs theme_minimal theme element_blank ggtitle
#' @importFrom rlang .data
#'
#' @return A list containing the following components:
#'   \item{per_index_means}{Numeric vector of NMI values for each clustering iteration}
#'   \item{ci}{Numeric vector containing the lower and upper bounds of the 95% confidence interval}
#'   \item{cluster_labels}{List of cluster assignments for each iteration}
#'
#' @export
clustStable <- function(
    n_runs,
    seurat_obj,
    method      = c("louvain", "leiden"),
    resolution  = 0.8,
    dims        = 1:10,
    n_cores     = 1,
    verbose     = TRUE,
    print_plot  = TRUE,
    seeds       = NULL
    ) {
  # Dependency checks
  for (pkg in c("Seurat", "future.apply", "aricode", "stats", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg), call. = FALSE)
    }
  }

  if (is.null(seeds)) {
    seeds <- 100:(100 + n_runs)
  }

  method <- match.arg(method)
  alg    <- if (method == "louvain") 1 else 4

  # Preallocate
  cluster_labels_all <- vector("list", length = n_runs)
  nmi_matrix        <- matrix(NA_real_, nrow = n_runs, ncol = n_runs)

  # Set up parallel plan
  future::plan('multisession', workers = n_cores)

  # Run all iterations in parallel
  cluster_labels_all <- future.apply::future_lapply(seq_len(n_runs), function(i) {
    set.seed(seeds[i])
    # Recompute graph each time
    so <- Seurat::FindNeighbors(
      seurat_obj,
      reduction    = "pca",
      dims         = dims,
      verbose      = FALSE
    )

    so <- Seurat::FindClusters(
      so,
      graph.name  = Seurat::DefaultAssay(so) %>% paste0("_snn"),
      resolution  = resolution,
      algorithm   = alg,
      verbose     = FALSE,
      random.seed = i + 100
    )

    # Return integer cluster IDs
    as.integer(Seurat::Idents(so))
  }, future.seed = TRUE)

  # Compute pairwise NMI
  for (i in seq_len(n_runs - 1)) {
    for (j in (i + 1):n_runs) {
      val <- aricode::NMI(cluster_labels_all[[i]], cluster_labels_all[[j]])
      nmi_matrix[i, j] <- val
      nmi_matrix[j, i] <- val
    }
  }

  # Compute stats per index
  per_index_means <- numeric(n_runs)

  for (i in seq_len(n_runs)) {
    # exclude diagonal
    values <- nmi_matrix[i, -i]
    per_index_means[i] <- mean(values, na.rm = TRUE)
  }
  ci <- stats::quantile(x = per_index_means, probs = c(0.025, 0.975), na.rm = TRUE)

  if(verbose == TRUE){
    cat(sprintf("Mean of Mean NMI per Index: %.4f\n", mean(per_index_means)))
    cat(sprintf("95 Percent CI of Mean NMI per Index: [%.4f, %.4f]\n", ci[1], ci[2]))
  }


  # Plot distribution of all mean NMI values
  df <- data.frame(mean_nmi = per_index_means)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = .data$mean_nmi)) +
    ggplot2::geom_violin(fill = "skyblue", color = "black") +
    ggplot2::labs(y = "Mean NMI per Index", x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle("Distribution of Mean NMI per Index")

  if(print_plot == TRUE){
    print(p)
  }

  return(list(per_index_means = per_index_means,
              ci = ci,
              cluster_labels = cluster_labels_all))
}
