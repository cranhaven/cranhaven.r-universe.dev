#' Benchmark Models
#' @format A list of benchmark models to estimate time and memory usage
#' for loading data.
#'
#' - time_small: A model for estimating time for small datasets (n_var < 1000).
#' - time_large: A model for estimating time for
#' larger datasets (n_var >= 1000).
#' - ram: A model for estimating RAM usage based on the number of variables.
#'
#' **Internal use only**: This dataset is used internally by some functions
#' and used in the package vignettes.
#' It is not intended for direct use by the end user.
#' @keywords internal data
"benchmark_models"
