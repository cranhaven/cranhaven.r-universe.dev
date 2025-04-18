#' Generate a variety of stats on PTR estimates for a given dataset
#'
#' @param X cov3 dataframe
#' @param iterations number of iterations to run
#'
#' @return named list of stats on PTR estimates
#' \itemize{
#'  \item all_sd: standard deviation of PTR estimates from all method
#'  \item all_mean: mean of PTR estimates from all method
#'  \item contigs_sd: standard deviation of PTR estimates from contigs method
#'  \item contigs_mean: mean of PTR estimates from contigs method
#'  \item samples_sd: standard deviation of PTR estimates from samples method
#'  \item samples_mean: mean of PTR estimates from samples method
#' }
#'
#' @importFrom stats sd
#'
#' @examples
#' stats <- get_eptr_stats(max_bin_001[max_bin_001$sample %in% c('Akk0_001', 'Akk1_001'), ], 2)
#' stats
#'
#' @export
get_eptr_stats <- function(X, iterations = 30) {
  all_ptrs <- list()
  contigs_ptrs <- list()
  samples_ptrs <- list()

  for (i in 1:iterations) {
    message(paste("Iteration", i))
    ptrs <- est_ptr(X)

    if (!is.null(ptrs$all_ptr)) {
      all_ptrs[[i]] <- ptrs$all_ptr$est_ptr
    }
    if (!is.null(ptrs$contigs_ptr)) {
      contigs_ptrs[[i]] <- ptrs$contigs_ptr$est_ptr
    }
    if (!is.null(ptrs$samples_ptr)) {
      samples_ptrs[[i]] <- ptrs$samples_ptr$est_ptr
    }
  }

  if (length(all_ptrs) == 0) {
    all_sd <- NULL
    all_mean <- NULL
  } else {
    all_ptrs <- matrix(unlist(all_ptrs), nrow = length(all_ptrs), byrow = TRUE)
    all_sd <- apply(all_ptrs, 2, sd)
    all_mean <- apply(all_ptrs, 2, mean)
  }

  if (length(contigs_ptrs) == 0) {
    contigs_sd <- NULL
    contigs_mean <- NULL
  } else {
    contigs_ptrs <- matrix(unlist(contigs_ptrs), nrow = length(contigs_ptrs), byrow = TRUE)
    contigs_sd <- apply(contigs_ptrs, 2, sd)
    contigs_mean <- apply(contigs_ptrs, 2, mean)
  }

  if (length(samples_ptrs) == 0) {
    samples_sd <- NULL
    samples_mean <- NULL
  } else {
    samples_ptrs <- matrix(unlist(samples_ptrs), nrow = length(samples_ptrs), byrow = TRUE)
    samples_sd <- apply(samples_ptrs, 2, sd)
    samples_mean <- apply(samples_ptrs, 2, mean)
  }

  list(
    all_sd = all_sd,
    all_mean = all_mean,
    contigs_sd = contigs_sd,
    contigs_mean = contigs_mean,
    samples_sd = samples_sd,
    samples_mean = samples_mean
  )
}
