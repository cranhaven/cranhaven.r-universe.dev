#' Estimates PTRs based on the whole input dataset
#'
#' @param X cov3 dataframe
#' @return est_ptrs dataframe on success, null otherwise
#' \itemize{
#'  \item est_ptr: estimated PTR values
#'  \item coefficient: coefficient of linear regression
#'  \item pValue: p-value of linear regression
#'  \item cor: correlation coefficient
#'  \item correctY: corrected coverage
#' }
#'
#' @examples
#' est_ptrs_001 <- est_ptr_on_all(max_bin_003)
#' est_ptrs_001
#'
#' @export
est_ptr_on_all <- function(X) {
  p <- iterate_pipelines(X)
  est_ptrs <- est_ptrs_subset(p)

  if (length(cor_diff(est_ptrs)) > 0) {
    warning("Not all estimates are correlated in the same direction")
    return(NULL)
  }

  rownames(est_ptrs) <- est_ptrs$sample
  est_ptrs <- est_ptrs[, c("est_ptr", "coefficient", "pValue", "cor", "correctY")]
  est_ptrs
}

#' Tries up to max_attempts times to compare each permutation of removing random
#' subsets of contigs/samples from X, and returns the PTR estimate if a valid
#' one comes back from the comparisons
#'
#' Requires a minimum of 2 * num_subsets contigs/samples
#'
#' @param X cov3 dataframe
#' @param subset_on either "contig" or "sample"
#' @param max_attempts max number of attempts to find a valid ptr estimate
#' @param num_subsets number of subsets to split contigs/samples into
#' @param cor_cutoff minimum correlation coefficient to accept PTR estimate
#' @return est_ptrs dataframe on success, null otherwise
#' \itemize{
#'   \item est_ptr: estimated PTR values
#'   \item coefficient: coefficient of linear regression
#'   \item pValue: p-value of linear regression
#'   \item cor: correlation coefficient
#'   \item correctY: corrected coverage
#' }
#'
#' @examples
#' est_ptrs_001_on_contigs <- est_ptr_on(max_bin_003, "contig", num_subsets = 5)
#' est_ptrs_001_on_contigs
#'
#' est_ptrs_001_on_samples <- est_ptr_on(max_bin_003, "sample")
#' is.null(est_ptrs_001_on_samples)
#'
#' @export
est_ptr_on <- function(X, subset_on, max_attempts = 10, num_subsets = 3, cor_cutoff = 0.98) {
  if (subset_on != "contig" && subset_on != "sample") {
    stop("subset_on must be either 'contig' or 'sample'")
  }
  if (length(unique(X[[subset_on]])) < 2 * num_subsets) {
    warning("Not enough contigs/samples to compare subsampled datasets")
    return(NULL)
  }

  max_cor <- 0

  for (i in 1:max_attempts) {
    unique_values <- sample(unique(X[[subset_on]]))
    subsets <- split(unique_values, cut(seq_along(unique_values), num_subsets, labels = FALSE))

    subset_pipelines <- lapply(subsets, function(z) iterate_pipelines(X[!X[[subset_on]] %in% z, ]))
    subset_est_ptrs <- lapply(subset_pipelines, function(z) est_ptrs_subset(z))

    for (x in 1:(num_subsets - 1)) {
      for (y in (x + 1):num_subsets) {
        if (is.null(subset_est_ptrs[[x]]) || is.null(subset_est_ptrs[[y]])) {
          next
        }

        if (subset_on == "contig") {
          comparison <- compare_contig_subsets(subset_est_ptrs[[x]], subset_est_ptrs[[y]], subset_pipelines[[x]], subset_pipelines[[y]], cor_cutoff, max_cor)
        } else if (subset_on == "sample") {
          comparison <- compare_sample_subsets(subset_est_ptrs[[x]], subset_est_ptrs[[y]], subset_pipelines[[x]], subset_pipelines[[y]], cor_cutoff, max_cor)
        }

        est_ptrs <- comparison$est_ptr
        max_cor <- comparison$max_cor

        if (!is.null(est_ptrs)) {
          rownames(est_ptrs) <- est_ptrs$sample
          est_ptrs <- est_ptrs[, c("est_ptr", "coefficient", "pValue", "cor", "correctY")]
          return(est_ptrs)
        }
      }
    }
  }

  NULL
}
