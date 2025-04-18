#' Estimate PTRs using all input data as well as using subsets of contigs and samples
#'
#' @param X dataframe with coverage matrix
#' (column names: "log_cov", "GC_content", "sample", "contig", "length")
#' @return named list with results from all three methods
#' all_ptr dataframe with the estimated PTRs on success, null otherwise
#' \itemize{
#'   \item est_ptr: estimated PTR values
#'   \item coefficient: coefficient of linear regression
#'   \item pValue: p-value of linear regression
#'   \item cor: correlation coefficient
#'   \item correctY: corrected coverage
#' }
#' contigs_ptr dataframe with the estimated PTRs on success, null otherwise
#' \itemize{
#'   \item est_ptr: estimated PTR values
#'   \item coefficient: coefficient of linear regression
#'   \item pValue: p-value of linear regression
#'   \item cor: correlation coefficient
#'   \item correctY: corrected coverage
#' }
#' samples_ptr dataframe with the estimated PTRs on success, null otherwise
#' \itemize{
#'   \item est_ptr: estimated PTR values
#'   \item coefficient: coefficient of linear regression
#'   \item pValue: p-value of linear regression
#'   \item cor: correlation coefficient
#'   \item correctY: corrected coverage
#' }
#'
#' @examples
#' est_ptrs_001 <- est_ptr(max_bin_003)
#' est_ptrs_001
#'
#' @export
est_ptr <- function(X) {
  verify_input(X)

  tryCatch(
    all_est_ptrs <- est_ptr_on_all(X),
    error = function(e) {
      all_est_ptrs <- NULL
      message("Error in est_ptr_from_all: ", e)
    }
  )

  tryCatch(
    contig_est_ptrs <- est_ptr_on(X, "contig"),
    error = function(e) {
      contig_est_ptrs <- NULL
      message("Error in est_ptr_on_contigs: ", e)
    }
  )

  tryCatch(
    sample_est_ptrs <- est_ptr_on(X, "sample"),
    error = function(e) {
      sample_est_ptrs <- NULL
      message("Error in est_ptr_on_samples: ", e)
    }
  )

  list(all_ptr = all_est_ptrs, contigs_ptr = contig_est_ptrs, samples_ptr = sample_est_ptrs)
}

#' Verify that the input dataframe/matrix is valid
#'
#' @param X dataframe/matrix with cov3 information
verify_input <- function(X) {
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("Input must be a dataframe or matrix")
  }

  if (!("log_cov" %in% colnames(X) && "GC_content" %in% colnames(X) &&
    "sample" %in% colnames(X) && "contig" %in% colnames(X) &&
    "length" %in% colnames(X))) {
    stop("Input must have columns 'log_cov', 'GC_content', 'sample', 'contig', and 'length'")
  }

  if (length(unique(X$sample)) < 2) {
    stop("Input must have at least 2 samples")
  }

  if (length(unique(X$contig)) < 2) {
    stop("Input must have at least 2 contigs")
  }
}
