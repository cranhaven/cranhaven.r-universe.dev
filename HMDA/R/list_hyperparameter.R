#' @title Create Hyperparameter List from a leaderboard dataset
#' @description Detects columns in a data frame that contain
#'              hyperparameters for H2O DRF/GBM algorithms and returns a list
#'              with the unique values from each parameter column.
#'
#' @param df  A data frame containing model results with
#'            hyperparameter columns.
#'
#' @return A named list where each hyperparameter element is a vector of unique
#'         values for a hyperparameter.
#'
#' @details This function scans the column names of the input data
#' frame for common H2O hyperparameter names, such as "ntrees",
#' "max_depth", "min_rows", "sample_rate",
#' "col_sample_rate_per_tree", "min_split_improvement",
#' "learn_rate", "mtries", and "seed". It extracts the unique
#' values from each matching column and returns them in a list.
#' The resulting list can be used as a hyperparameter grid for
#' tuning via H2O grid search functions.
#'
#' @author E. F. Haghish
list_hyperparameter <- function(df) {
  # Define a pattern for common H2O hyperparameter names
  pattern <- "(ntrees|max_depth|min_rows|sample_rate|"
  pattern <- paste0(pattern, "col_sample_rate_per_tree|",
                    "min_split_improvement|learn_rate|mtries)")

  # Identify columns that match the pattern (case-insensitive)
  hyper_cols <- names(df)[grepl(pattern, names(df),
                                ignore.case = TRUE)]

  # For each detected hyperparameter, get unique values
  hyper_list <- lapply(hyper_cols, function(col) {
    unique(df[[col]])
  })
  names(hyper_list) <- hyper_cols

  return(hyper_list)
}
