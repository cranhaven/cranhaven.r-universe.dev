#' @title Select Best Models by Performance Metrics
#' @description Detects all performance metric columns in a data frame,
#'   and for each metric, identifies the best model based on whether
#'   a higher or lower value is preferred. The function returns a vector
#'   of unique model IDs corresponding to the best models across all
#'   detected metrics.
#'
#' @param df  A data frame containing model performance results.
#'            It must include a column named \code{"model_id"} and one
#'            or more numeric columns for performance metrics.
#'
#' @return An integer or character vector of unique \code{model_id}
#'         values corresponding to the best model for each performance
#'         metric.
#'
#' @details The function first detects numeric columns (other than
#'         \code{"model_id"}) as performance metrics. It then uses a
#'         predefined mapping to determine the optimal direction for each
#'         metric: for example, higher values of \code{auc} and
#'         \code{aucpr} are better, while lower values of \code{logloss},
#'         \code{mean_per_class_error}, \code{rmse}, and \code{mse} are
#'         preferred. For any metric not in the mapping, the function
#'         assumes that lower values indicate better performance.
#'
#'         For each metric, the function identifies the row index that
#'         produces the best value according to the corresponding direction
#'         (using \code{which.max()} or \code{which.min()}). It then extracts
#'         the \code{model_id} from that row. The final result is a unique
#'         set of model IDs that represent the best models across all metrics.
#'
# @examples
# \dontrun{
#   # Example: Given a data frame 'merged' with performance metrics,
#   # select the best models per metric.
#   best_ids <- best_of_family(merged)
#   print(best_ids)
# }
#'
#' @author E. F. Haghish

best_of_family <- function(df) {
  # Exclude the "model_id" column and keep numeric columns only.
  metric_cols <- setdiff(names(df), "model_id")
  metric_cols <- metric_cols[sapply(df[, metric_cols, drop = FALSE],
                                    is.numeric)]

  # Define known metrics and their desired direction.
  # Metrics not defined here are assumed to be minimized.
  directions <- c(
    auc                  = "maximize",
    aucpr                = "maximize",
    r2                   = "maximize",
    accuracy             = "maximize",
    # precision            = "maximize",
    # recall               = "maximize",
    f1                   = "maximize",
    f2                   = "maximize",
    mcc                  = "maximize",
    logloss              = "minimize",
    mean_per_class_error = "minimize",
    rmse                 = "minimize",
    mse                  = "minimize",
    mae                  = "minimize"
  )

  # For each performance metric, determine the best model_id.
  best_ids <- sapply(metric_cols, function(metric) {
    # Determine the desired direction (default to "minimize")
    direction <- directions[[tolower(metric)]]
    if (is.null(direction)) direction <- "minimize"

    values <- df[[metric]]
    if (direction == "maximize") {
      best_row <- which.max(values)
    } else {
      best_row <- which.min(values)
    }

    df$model_id[best_row]
  })

  # Return unique model IDs across metrics.
  unique(best_ids)
}
