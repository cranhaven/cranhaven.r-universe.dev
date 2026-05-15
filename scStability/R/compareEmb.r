#' Compare dimensional reduction embeddings and calculate stability statistics
#'
#' @description
#' Evaluates the stability of a set of dimension reduction embeddings by performing
#' pairwise Procrustes alignment and calculating Kendall's Tau correlation between
#' each pair. This function quantifies the consistency of embeddings generated with
#' the same algorithm but different random initializations.
#'
#' @param emb_list A list of 2D embeddings (each typically containing coordinates for UMAP or t-SNE)
#'        created by the \code{createEmb} function
#' @param n_cores Integer specifying the number of CPU cores to use for parallelization (default: 1)
#' @param verbose Whether the function should print summary statistics as it calculates them
#' @param print_plot Whether the final violin plot should be automatically printed
#'
#' @importFrom stats quantile density
#' @importFrom utils combn
#'
#' @return A list containing the following components:
#'   \item{mean}{Numeric value representing the overall mean correlation across all pairwise comparisons}
#'   \item{mean_per_embedding}{Numeric vector of mean correlation values for each embedding}
#'   \item{all_pairwise_correlations}{Numeric vector containing all pairwise correlation values}
#'   \item{range}{Numeric vector with minimum and maximum of mean correlation per embedding}
#'   \item{ci}{Numeric vector containing the lower and upper bounds of the 95% confidence interval}
#'
#' @export
compareEmb <- function(
    emb_list,
    n_cores = 1,
    verbose = TRUE,
    print_plot = TRUE
    ) {
  # Input checks
  if (!is.list(emb_list) || length(emb_list) < 2) {
    stop("`emb_list` must be a list of at least two embeddings.", call. = FALSE)
  }
  dims <- unique(lapply(emb_list, dim))
  if (length(dims) != 1) {
    stop("All embeddings must have identical dimensions.", call. = FALSE)
  }

  # Dependency checks
  for (pkg in c("vegan", "pcaPP", "future.apply", "stats")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg), call. = FALSE)
    }
  }
  # Setup
  n <- length(emb_list)
  matList <- lapply(emb_list, as.matrix)
  pairs   <- combn(length(matList), 2, simplify = FALSE)
  cor_mat <- matrix(NA, nrow = n, ncol = n)
  future::plan('multisession', workers = n_cores)

  # Align and compare embeddings
  cor_values <- future.apply::future_sapply(pairs, function(idx_pair) {
    i <- idx_pair[1]; j <- idx_pair[2]
    proc <- tryCatch(vegan::procrustes(matList[[i]], matList[[j]], symmetric = TRUE),
                     error = function(e) {
                       warning(sprintf("Procrustes failed for (%d, %d): %s", i, j, e$message), call. = FALSE)
                       return(NULL)
                     })
    if (is.null(proc)) return(NA_real_)
    vec1 <- as.numeric(proc$X); vec2 <- as.numeric(proc$Yrot)
    tryCatch(pcaPP::cor.fk(vec1, vec2),
             error = function(e) {
               warning(sprintf("Correlation failed for (%d, %d): %s", i, j, e$message), call. = FALSE)
               NA_real_
             })
  }, future.seed = TRUE)
  future::plan(NULL)

  # Use the computed cor_values to fill in the correlation matrix.
  for (k in seq_along(pairs)) {
    i <- pairs[[k]][1]
    j <- pairs[[k]][2]
    cor_mat[i, j] <- cor_values[k]
    cor_mat[j, i] <- cor_values[k]
  }

  # Compute the mean correlation per embedding (row means excluding the diagonal).
  mean_per_embedding <- sapply(1:n, function(i) {
    mean(cor_mat[i, -i], na.rm = TRUE)
  })

  # Compute overall mean correlation.
  cor_mean <- mean(mean_per_embedding)

  # Find CI based on quantile
  ci <- quantile(x = mean_per_embedding, probs = c(0.025, 0.975), na.rm = TRUE)

  range_vals <- range(mean_per_embedding)
  if(verbose == TRUE){
    cat(sprintf("Mean of Mean Correlation per Embedding: %.4f\n", cor_mean))
    cat(sprintf("95 Percent CI of Mean Correlation per Embedding: [%.4f, %.4f]\n", ci[1], ci[2]))
    cat(sprintf("Mean Correlation Range: Min = %.4f Max = %.4f\n", range_vals[1], range_vals[2]))
  }


  df <- data.frame(mean_corr = mean_per_embedding)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = .data$mean_corr)) +
    ggplot2::geom_violin(fill = "skyblue", color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggplot2::labs(y = "Mean Correlation per Index", x = NULL, subtitle = paste0("n = ", n)) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.0), hjust = 0.5)) +
    ggplot2::ggtitle("Distribution of Mean Correlation per Index")
  if(print_plot == TRUE){
    print(p)
  }


  # Return the results.
  return(list(all_pairwise_correlations = cor_mat,
              mean_per_embedding = mean_per_embedding,
              mean = cor_mean,
              ci = ci,
              range = range_vals,
              plot = p))
}
