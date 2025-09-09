#' @title Select Best Models Across All Models in HMDA Grid
#' @description Scans a HMDA grid analysis data frame for H2O performance
#'   metric columns and, for each metric, selects the top \code{n_models}
#'   best-performing models based on the proper optimization direction
#'   (i.e., lower values are better for some metrics and higher values
#'   are better for others). The function then returns a summary data frame
#'   showing the union of these best models (without duplication) along with
#'   the corresponding metric values that led to their selection.
#'
#' @param df         A data frame of class \code{"hmda.grid.analysis"} containing
#'                   model performance results. It must include a column named
#'                   \code{model_ids} and one or more numeric columns representing
#'                   H2O performance metrics (e.g., \code{logloss}, \code{auc},
#'                   \code{rmse}, etc.).
#' @param n_models   Integer. The number of top models to select per metric.
#'                   Default is 1.
#'
#' @return A data frame containing the rows corresponding to the union of
#'         best model IDs (across all metrics) and the columns for
#'         \code{model_ids} plus the performance metrics that are present
#'         in the data frame.
#'
#' @details
#'   The function uses a predefined set of H2O performance metrics along with
#'   their desired optimization directions:
#'   \describe{
#'     \item{logloss, mae, mse, rmse, rmsle, mean_per_class_error}{Lower values
#'           are better.}
#'     \item{auc, aucpr, r2, accuracy, f1, mcc, f2}{Higher values are better.}
#'   }
#'
#'   For each metric in the predefined list that exists in \code{df} and is not
#'   entirely NA, the function orders the values (using \code{order()}) according
#'   to whether lower or higher values indicate better performance. It then selects
#'   the top \code{n_models} model IDs for that metric. The union of these model IDs
#'   is used to subset the original data frame. The returned data frame includes
#'   the \code{model_ids} column and the performance metric columns (from the
#'   predefined list) that were found in the input data frame.
#'
#' @examples
#' \dontrun{
#'   # Example: Create a hyperparameter grid for GBM models.
#'   predictors <- c("var1", "var2", "var3")
#'   response <- "target"
#'
#'   # Define hyperparameter ranges
#'   hyper_params <- list(
#'     ntrees = seq(50, 150, by = 25),
#'     max_depth = c(5, 10, 15),
#'     learn_rate = c(0.01, 0.05, 0.1),
#'     sample_rate = c(0.8, 1.0),
#'     col_sample_rate = c(0.8, 1.0)
#'   )
#'
#'   # Run the grid search
#'   grid <- hmda.grid(
#'     algorithm = "gbm",
#'     x = predictors,
#'     y = response,
#'     training_frame = h2o.getFrame("hmda.train.hex"),
#'     hyper_params = hyper_params,
#'     nfolds = 10,
#'     stopping_metric = "AUTO"
#'   )
#'
#'   # Assess the performances of the models
#'   grid_performance <- hmda.grid.analysis(grid)
#'
#'   # Return the best 2 models according to each metric
#'   hmda.best.models(grid_performance, n_models = 2)
#' }
#'
#' @importFrom utils head
#' @export
#' @author E. F. Haghish
hmda.best.models <- function(df, n_models = 1) {

  if (!inherits(df, "hmda.grid.analysis")) {
    warning("'df' is not of class 'hmda.grid.analysis'... watch out!")
  }

  # Define known performance metrics and their optimization directions
  known_metrics <- c("logloss", "mae", "mse", "rmse", "rmsle",
                     "mean_per_class_error", "auc", "aucpr",
                     "r2", "accuracy", "f1", "mcc", "f2")

  directions <- c(
    logloss = "minimize",
    mae = "minimize",
    mse = "minimize",
    rmse = "minimize",
    rmsle = "minimize",
    mean_per_class_error = "minimize",
    auc = "maximize",
    aucpr = "maximize",
    r2 = "maximize",
    accuracy = "maximize",
    f1 = "maximize",
    mcc = "maximize",
    f2 = "maximize"
  )

  # Check for the existence of the model_ids column
  if (!"model_ids" %in% names(df)) {
    stop("The data frame must contain a 'model_ids' column.")
  }

  # Initialize a vector to hold best model IDs across metrics
  best_model_ids <- c()

  # Loop over each known metric and determine the top n_models best model IDs
  for (metric in known_metrics) {
    if (metric %in% names(df)) {
      vals <- df[[metric]]
      if (all(is.na(vals))) next  # Skip metric if all values are NA

      dir <- directions[[metric]]
      if (is.null(dir)) dir <- "minimize"

      # Order indices according to performance (top n_models)
      if (dir == "maximize") {
        ordered_idx <- order(vals, decreasing = TRUE)
      } else {
        ordered_idx <- order(vals, decreasing = FALSE)
      }

      top_idx <- head(ordered_idx, n_models)
      best_model_ids <- c(best_model_ids, df$model_ids[top_idx])
    }
  }

  # Get the unique union of best model IDs
  best_model_ids <- unique(best_model_ids)

  # Determine which known metric columns exist in df
  existing_metrics <- intersect(known_metrics, names(df))

  # Subset the original data frame for the best models and only include
  # the model_ids and the performance metric columns.
  result <- df[df$model_ids %in% best_model_ids, c("model_ids", existing_metrics), drop = FALSE]

  # Drop metric columns that are entirely NA in the resulting subset
  for (col in existing_metrics) {
    if (all(is.na(result[[col]]))) {
      result[[col]] <- NULL
    }
  }

  return(result)
}
