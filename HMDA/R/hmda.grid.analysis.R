#' @title Analyze Hyperparameter Grid Performance
#' @description Reorders an HMDA grid based on a specified performance metric and
#'   supplements the grid's summary table with additional performance metrics
#'   extracted via cross-validation. The function returns a data frame of performance
#'   metrics for each model in the grid. This enables a detailed
#'   analysis of model performance across various metrics such as logloss, AUC, RMSE, etc.
#'
#' @param grid A HMDA grid object from which the performance summary will be extracted.
#' @param performance_metrics A character vector of additional performance metric
#'   names to be included in the analysis. Default is
#'   \code{c("logloss", "mse", "rmse", "rmsle", "auc", "aucpr",
#'   "mean_per_class_error", "r2")}.
#' @param sort_by A character string indicating the performance metric to sort the grid
#'   by. Default is \code{"logloss"}. For metrics such as logloss, mae, mse, rmse, and
#'   rmsle, lower values are better, while for metrics like AUC, AUCPR, and R2, higher
#'   values are preferred.
#'
#' @return A data frame of class \code{"hmda.grid.analysis"} that contains the merged
#'   performance summary table. This table includes the default metrics from the grid
#'   summary along with the additional metrics specified by \code{performance_metrics}
#'   (if available). The data frame is sorted according to the \code{sort_by} metric.
#'
#' @details
#'   The function performs the following steps:
#'   \enumerate{
#'     \item \strong{Grid Reordering:} It calls \code{h2o.getGrid()} to reorder the grid
#'           based on the \code{sort_by} metric. For metrics like "logloss", "mse",
#'           "rmse", and "rmsle", sorting is in ascending order; for others, it is in descending
#'           order.
#'     \item \strong{Performance Table Extraction:} The grid's summary table is converted into
#'           a data frame.
#'     \item \strong{Additional Metric Calculation:} For each metric specified in
#'           \code{performance_metrics} (other than the one used for sorting), the function
#'           initializes a column with NA values and iterates over each model in the grid
#'           (via its \code{model_ids}) to extract the corresponding cross-validated
#'           performance metric using functions such as \code{h2o.auc()}, \code{h2o.rmse()},
#'           etc. For threshold-based metrics (e.g., \code{f1}, \code{f2}, \code{mcc},
#'           \code{kappa}), it retrieves performance via \code{h2o.performance()}.
#'     \item \strong{Return:} The function returns the merged data frame of performance metrics.
#'   }
#'
#' @examples
#' \dontrun{
#'   # NOTE: This example may take a long time to run on your machine
#'
#'   # Initialize H2O (if not already running)
#'   library(HMDA)
#'   library(h2o)
#'   hmda.init()
#'
#'   # Import a sample binary outcome train/test set into H2O
#'   train <- h2o.importFile(
#'   "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_train_10k.csv")
#'   test <- h2o.importFile(
#'   "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_test_5k.csv")
#'
#'   # Identify predictors and response
#'   y <- "response"
#'   x <- setdiff(names(train), y)
#'
#'   # For binary classification, response should be a factor
#'   train[, y] <- as.factor(train[, y])
#'   test[, y] <- as.factor(test[, y])
#'
#'   # Run the hyperparameter search using DRF and GBM algorithms.
#'   result <- hmda.search.param(algorithm = c("gbm"),
#'                               x = x,
#'                               y = y,
#'                               training_frame = train,
#'                               max_models = 100,
#'                               nfolds = 10,
#'                               stopping_metric = "AUC",
#'                               stopping_rounds = 3)
#'
#'   # Assess the performances of the models
#'   grid_performance <- hmda.grid.analysis(gbm_grid1)
#'
#'   # Return the best 2 models according to each metric
#'   hmda.best.models(grid_performance, n_models = 2)
#' }
#'
#' @importFrom h2o h2o.F1 h2o.F2 h2o.auc h2o.aucpr h2o.getGrid h2o.logloss h2o.mcc
#' h2o.mse h2o.performance h2o.r2 h2o.rmse
#' @export
#' @author E. F. Haghish

hmda.grid.analysis <- function(grid,
                               performance_metrics =
                                 c("logloss", "mse", "rmse", "rmsle", "auc",
                                   "aucpr", "mean_per_class_error", "r2"),
                               sort_by = "logloss") {

  # prepare the performance dataset
  # ============================================================
  if (!sort_by %in% c("logloss", "mean_per_class_error", "rmse", "mse")) {
    grid <- h2o.getGrid(grid@grid_id, sort_by = sort_by, decreasing = TRUE)
  } else {
    grid <- h2o.getGrid(grid@grid_id, sort_by = sort_by, decreasing = FALSE)
  }
  performance <- as.data.frame(grid@summary_table)

  # get the additional metrics requested by the user
  # ============================================================
  newperf <- performance_metrics[!performance_metrics %in% sort_by]
  #if (j == "auc") performance[n, j] <- h2o.auc(h2o.getModel(unlist(grid@model_ids)[1]), xval = TRUE)

  IDS <- performance$model_ids
  for (j in newperf) {
    performance[, j] <- NA
    for (i in 1:length(IDS)) {
      MODEL <- h2o.getModel(IDS[i])
      if (j == "auc") try(performance[i, j] <- h2o.auc(MODEL, xval = TRUE), silent = TRUE)
      else if (j == "aucpr") try(performance[i, j] <- h2o.aucpr(MODEL, xval = TRUE), silent = TRUE)
      else if (j == "r2") try(performance[i, j] <- h2o.r2(MODEL, xval = TRUE), silent = TRUE)
      else if (j == "loggloss") try(performance[i, j] <- h2o.logloss(MODEL, xval = TRUE), silent = TRUE)
      #else if (j == "mae") try(performance[i, j] <- h2o.mae(MODEL, xval = TRUE), silent = TRUE)
      else if (j == "mse") try(performance[i, j] <- h2o.mse(MODEL, xval = TRUE), silent = TRUE)
      else if (j == "rmse") try(performance[i, j] <- h2o.rmse(MODEL, xval = TRUE), silent = TRUE)

      # for thresholds metrics...
      else if (j %in% c("f1","f2","mcc","kappa")) {
        PERF <- h2o.performance(model = MODEL, xval= TRUE)
        if (j == "f1") try(performance[i, j] <- h2o.F1(PERF), silent = TRUE)
        else if (j == "f2") try(performance[i, j] <- h2o.F2(PERF), silent = TRUE)
        else if (j == "mcc") try(performance[i, j] <- h2o.mcc(PERF), silent = TRUE)
        else if (j == "kappa") try(performance[i, j] <- h2otools::kappa(PERF), silent = TRUE)
      }
    }
  }

  # Drop columns that are entirely NA
  performance <- performance[, !apply(is.na(performance), 2, all)]

  class(performance) <- c("hmda.grid.analysis", "data.frame")

  return(performance)
}

# a <- hmda.grid.analysis(grid = grid, performance_metrics = c("logloss","auc","aucpr"), sort_by = "logloss")
#
# MODEL <- h2o.getModel(performance$model_ids[1])
#
# h2o.metric(object = perf)
