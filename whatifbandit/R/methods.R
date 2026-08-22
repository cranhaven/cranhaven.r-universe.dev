#' Constructs `mab` and its other class variants
#' @name construct_mab
#' @description Simple constructor for proper `mab` subclasses as output
#' to [simulate_mab()] and [mab_from_rct()].
#' @param mab Named list output of [simulate_mab()] or [mab_from_rct()].
#' @param type Type of simulated trial, either `"rct"` or `"param"` to denote whether it was an RCT re-simulation or an simulation from population parameters.
#' @param multi Logical; `TRUE` denotes multiple trials.
#' @returns Input `mab` with appropriate S3 class, restructured for output
#' @keywords internal

construct_mab <- function(mab, type, multi) {
  class <- if (multi) {
    c(paste0("multi_", type, "_mab"), "multi_mab")
  } else {
    c(paste0("single_", type, "_mab"), "single_mab")
  }
  structure(
    list(
      new_data = mab$final_data,
      bandit = list(
        statistic = mab$bandits,
        assignment_prob = mab$assignment_prob,
        assignment_quant = mab$assignment_quantities
      ),
      means = mab$means,
      f_stats = mab$f_stats,
      contrasts = mab[["contrasts"]],
      models = mab$models,
      config = list(args = mab$args, call = mab$cl, parallel = mab$furrr)
    ),
    class = c(class, ".mab", "list")
  )
}
