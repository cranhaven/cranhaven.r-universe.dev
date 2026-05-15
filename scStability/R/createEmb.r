#' Create multiple dimension reduction embeddings
#'
#' @description
#' Generates multiple dimension reduction embeddings using either UMAP or t-SNE algorithms. Each embedding is created with different random initializations to assess stability. The function returns a list of embeddings, each represented as a data frame or matrix.
#'
#' @param dr_input A numeric matrix or data frame containing the input data for dimension
#'        reduction, with rows representing observations (cells) and columns representing
#'        PCA components
#' @param n_runs Integer specifying the number of embeddings to generate (default: 100)
#' @param method Character string specifying the dimension reduction method to use:
#'        either "umap" or "tsne"
#' @param n_cores Integer specifying the number of CPU cores to use for parallelization (default: 1)
#' @param n_neighbors Integer specifying the number of neighbors to consider when constructing the
#'        initial graph (used for UMAP only, default: 30)
#' @param min_dist Numeric value specifying the minimum distance between points in the embedding
#'        (used for UMAP only, default: 0.1)
#' @param perplexity Numeric value controlling the effective number of neighbors
#'        (used for t-SNE only, default: 30)
#' @param theta Numeric value between 0 and 1 controlling the speed/accuracy trade-off
#'        (used for t-SNE only, default: 0.5)
#' @param seeds A set of seeds of length n_runs to be used for each embedding
#'
#' @return A list of dimension reduction embeddings, each represented as a data frame
#'         with rows corresponding to observations (cells) and two columns representing the
#'         x and y coordinates in the reduced space.
#'
#' @export
createEmb <- function(dr_input, n_runs = 100, method = c("umap","tsne"),
                      n_neighbors = 15, min_dist = 0.1,
                      perplexity = 30, theta = 0.5, n_cores = 1, seeds = NULL
                      ) {
  # Input validation
  if (!(is.matrix(dr_input) || is.data.frame(dr_input))) {
    stop("`dr_input` must be a matrix or data frame of numeric values.", call. = FALSE)
  }
  dr_input <- as.matrix(dr_input)
  if (!is.numeric(dr_input)) stop("`dr_input` must be numeric.", call. = FALSE)

  if (!(is.numeric(n_runs) && length(n_runs)==1 && n_runs>=1 && n_runs==as.integer(n_runs))) {
    stop("`n_runs` must be a single integer >= 1.", call. = FALSE)
  }
  method <- match.arg(method)
  if (is.null(seeds)) {
    seeds <- 100:(100 + n_runs)
  }

  # Dependency checks
  for (pkg in c("uwot","Rtsne","future.apply")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required but not installed.", pkg), call. = FALSE)
    }
  }

  # Plan management
  old_plan <- future::plan()
  future::plan('multisession', workers = n_cores)
  on.exit(future::plan(old_plan), add = TRUE)

  # Chunking
  chunks <- if (n_runs >= 100) parallel::detectCores()*4 else parallel::detectCores()*2

  # Parallel embedding with error handling
  results <- future.apply::future_lapply(
    seq_len(n_runs), function(i) {
      set.seed(seeds[i])
      tryCatch({
        if (method == "umap") {
          uwot::umap(dr_input, n_neighbors = n_neighbors,
                     min_dist = min_dist, n_threads = 1, ret_model = FALSE)
        } else {
          Rtsne::Rtsne(dr_input, perplexity = perplexity,
                       theta = theta, pca = FALSE,
                       check_duplicates = FALSE,
                       max_iter = 500, num_threads = 1)$Y
        }
      }, error = function(e) {
        warning(sprintf("%s run %d failed: %s", toupper(method), i, e$message),
                call. = FALSE)
        NULL
      })
    }, future.seed = TRUE, future.chunk.size = chunks
  )
  gc()

  # Post-process results
  failed <- sum(vapply(results, is.null, logical(1)))
  if (failed > 0) {
    warning(sprintf("%d/%d embeddings failed and were omitted.", failed, n_runs),
            call. = FALSE)
  }
  embeddings <- results[!vapply(results, is.null, logical(1))]

  if (length(embeddings) == 0) {
    stop("All embedding runs failed; no results to return.", call. = FALSE)
  }

  return(embeddings)
}
