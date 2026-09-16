#' Generate CV run identifiers
#'
#' A utility function used in RAFS to generate cross validation run identifiers, thus useful also for external cross-validation.
#'
#' @param seed a numerical seed
#' @param k number of folds for cross validation
#' @param i current fold number (1 to \code{k})
#' @return A string with the run identifier.
#' @export
get_run_id <- function(seed, k, i) {
  paste0("run_", seed, "_", k, "_", i)
}


#' Create seeded folds
#'
#' A utility function used in RAFS but useful also for external cross-validation.
#'
#' @param decision decision variable as a binary sequence of length equal to number of observations
#' @param k number of folds for cross validation
#' @param seed a numerical seed
#' @importFrom splitTools create_folds
#' @return A vector of folds. Each fold being a vector of selected indices.
#' @export
create_seeded_folds <- function(decision, k, seed) {
  set.seed(seed)
  if (k == 1) {
    # NOTE: a hack to ensure that we don't exclude any objects with the minus indexing
    length(decision) + 1
  } else {
    create_folds(decision, k = k, invert = TRUE)
  }
}
