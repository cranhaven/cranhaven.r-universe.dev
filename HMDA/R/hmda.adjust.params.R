#' @title Adjust Hyperparameter Combinations
#' @description This internal function prunes or expands a list of
#'   hyperparameters so that the total number of model combinations,
#'   computed as the product of the lengths of each parameter vector,
#'   is near the desired target (\code{n_models}). It first prunes the
#'   parameter with the largest number of values until the product is
#'   less than or equal to \code{n_models}. Then, if the product is much
#'   lower than the target (less than half of \code{n_models}), it attempts
#'   to expand the parameter with the smallest number of values by adding
#'   a midpoint value (if numeric).
#'
#' @param params   A list of hyperparameter vectors.
#' @param n_models Integer. The desired target number of model
#'                 combinations.
#'
#' @return A list of hyperparameter vectors that has been pruned or
#'         expanded so that the product of their lengths is near
#'         \code{n_models}.
#'
#' @details The function calculates the current product of the
#'   lengths of the hyperparameter vectors. In a loop, it removes the
#'   last element from the parameter vector with the largest length
#'   until the product is less than or equal to \code{n_models}. If the
#'   resulting product is less than half of \code{n_models}, the function
#'   attempts to expand the parameter with the smallest length by
#'   computing a midpoint between the two closest numeric values. The
#'   expansion stops if no new value can be added, to avoid an infinite
#'   loop.
#'
#' @examples
#'   # Example 1: Adjust a hyperparameter grid for 100 models.
#'   params <- list(
#'     alpha = c(0.1, 0.2, 0.3, 0.4),
#'     beta = c(1, 2, 3, 4, 5),
#'     gamma = c(10, 20, 30)
#'   )
#'   new_params <- hmda.adjust.params(params, n_models = 100)
#'   print(new_params)
#'
#'   # Example 2: The generated hyperparameters range between min and max of each
#'   # vector in the list
#'   params <- list(
#'     alpha = c(0.1, 0.2),
#'     beta = c(1, 2, 3),
#'     gamma = c(10, 20)
#'   )
#'   new_params <- hmda.adjust.params(params, n_models = 1000)
#'   print(new_params)
#'
#' @export
#' @author E. F. Haghish

hmda.adjust.params <- function(params, n_models) {
  current_prod <- prod(sapply(params, length))

  # Simple loop to prune until we're near or below n_models
  while (current_prod > n_models && current_prod > 1) {
    # Find the hyperparameter with the largest length
    max_len_param <- which.max(sapply(params, length))
    if (length(params[[max_len_param]]) > 1) {
      # Drop the last value
      params[[max_len_param]] <- params[[max_len_param]][-length(params[[max_len_param]])]
    }
    current_prod <- prod(sapply(params, length))
  }

  # Optional expansion (only if extremely below the n_models)
  while (current_prod < (n_models / 2)) {
    min_len_param <- which.min(sapply(params, length))

    # Attempt a midpoint if numeric
    candidate_vals <- params[[min_len_param]]
    if (all(is.numeric(candidate_vals))) {
      diffs <- diff(sort(candidate_vals))
      max_diff_idx <- which.max(diffs)
      new_val <- mean(sort(candidate_vals)[c(max_diff_idx, max_diff_idx + 1)])
      new_val <- round(new_val, 3)
      if (!new_val %in% candidate_vals) {
        params[[min_len_param]] <- sort(c(candidate_vals, new_val))
      }
    }
    new_prod <- prod(sapply(params, length))
    if (new_prod == current_prod) break  # no change, avoid infinite loop
    current_prod <- new_prod
  }

  params
}
