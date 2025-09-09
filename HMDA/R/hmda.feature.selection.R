#' @title Feature Selection Based on Weighted SHAP Values
#' @description This function selects "important", "inessential", and "irrelevant"
#' features based on a summary of weighted mean SHAP values obtained from a prior
#' analysis. It uses the SHAP summary table (found in the \code{wmshap} object)
#' to identify features that are deemed important according to a specified method
#' and cutoff. Features with a lower confidence interval (lowerCI) below zero
#' are labeled as "irrelevant", while the remaining features are classified as
#' "inessential" if they do not meet the importance criteria.
#'
#' @param wmshap A list object (typically returned by a weighted SHAP analysis)
#' that must contain a data frame \code{summaryShaps} with at least the columns
#' \code{"feature"}, \code{"mean"}, and \code{"lowerCI"}. It may also contain
#' additional columns for alternative selection methods.
#' @param method Character. Specify the method for selecting important features
#'               based on their weighted mean SHAP ratios. The default is
#'               \code{"mean"}, which selects features whose weighted mean shap ratio (WMSHAP)
#'               exceeds the \code{cutoff}. The alternative is
#'               \code{"lowerCI"}, which selects features whose lower bound of confidence
#'               interval exceeds the \code{cutoff}.
#' @param cutoff Numeric. The threshold cutoff for the selection method. Features
#' with a weighted SHAP value (or ratio) greater than or equal to this value
#' are considered important. Default is \code{0.01}.
#' @param top_n_features Integer. If specified, the function selects the top
#' \code{top_n_features} features (based on the sorted SHAP mean values),
#' overriding the cutoff and method arguments. If \code{NULL}, all features that
#' meet the cutoff criteria are used. Default is \code{NULL}.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{important}{A character vector of features deemed important.}
#'   \item{inessential}{A character vector of features considered inessential
#'         (present in the data but not meeting the importance criteria).}
#'   \item{irrelevant}{A character vector of features deemed irrelevant,
#'         defined as those with a lower confidence interval (lowerCI) below zero.}
#' }
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Retrieves the SHAP summary table from the \code{wmshap} object.
#'   \item Sorts the summary table in descending order based on the \code{mean}
#'         SHAP value.
#'   \item Identifies all features available in the summary.
#'   \item Classifies features as \strong{irrelevant} if their \code{lowerCI}
#'         value is below zero.
#'   \item If \code{top_n_features} is not specified, selects \strong{important}
#'         features as those whose value for the specified \code{method} column
#'         meets or exceeds the \code{cutoff}; the remaining features (excluding
#'         those marked as irrelevant) are classified as \strong{inessential}.
#'   \item If \code{top_n_features} is provided, the function selects the top
#'         \code{n} features (based on the sorted order) as important, with the
#'         rest (excluding irrelevant ones) being inessential.
#' }
#'
#' @examples
#' \dontrun{
#' library(HMDA)
#' library(h2o)
#' hmda.init()
#' h2o.removeAll()
#'
#' # Import a sample binary outcome dataset into H2O
#' train <- h2o.importFile(
#' "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_train_10k.csv")
#' test <- h2o.importFile(
#' "https://s3.amazonaws.com/h2o-public-test-data/smalldata/higgs/higgs_test_5k.csv")
#'
#' # Identify predictors and response
#' y <- "response"
#' x <- setdiff(names(train), y)
#'
#' # For binary classification, response should be a factor
#' train[, y] <- as.factor(train[, y])
#' test[, y] <- as.factor(test[, y])
#'
#' params <- list(learn_rate = c(0.01, 0.1),
#'                max_depth = c(3, 5, 9),
#'                sample_rate = c(0.8, 1.0)
#' )
#'
#' # Train and validate a cartesian grid of GBMs
#' hmda_grid1 <- hmda.grid(algorithm = "gbm", x = x, y = y,
#'                         grid_id = "hmda_grid1",
#'                         training_frame = train,
#'                         nfolds = 10,
#'                         ntrees = 100,
#'                         seed = 1,
#'                         hyper_params = gbm_params1)
#'
#' # Assess the performances of the models
#' grid_performance <- hmda.grid.analysis(hmda_grid1)
#'
#' # Return the best 2 models according to each metric
#' hmda.best.models(grid_performance, n_models = 2)
#'
#' # build an autoEnsemble model & test it with the testing dataset
#' meta <- hmda.autoEnsemble(models = hmda_grid1, training_frame = train)
#' print(h2o.performance(model = meta$model, newdata = test))
#'
#' # compute weighted mean shap values
#' wmshap <- hmda.wmshap(models = hmda_grid1,
#'                       newdata = test,
#'                       performance_metric = "aucpr",
#'                       standardize_performance_metric = FALSE,
#'                       performance_type = "xval",
#'                       minimum_performance = 0,
#'                       method = "mean",
#'                       cutoff = 0.01,
#'                       plot = TRUE)
#'
#' # identify the important features
#' selected <- hmda.feature.selection(wmshap,
#'                                    method = c("mean"),
#'                                    cutoff = 0.01)
#' print(selected)
#' }
#'
#' @export
#' @author E. F. Haghish

hmda.feature.selection <- function(wmshap,
                          method = c("mean"),
                          cutoff = 0.01,
                          top_n_features = NULL) {
  # Exclude features that do not meet the criteria
  # ====================================================
  summaryShaps <- wmshap$summaryShaps

  # Sort the results
  summaryShaps <- summaryShaps[order(summaryShaps$mean, decreasing = TRUE), ]

  # get a list of all features with wmshap values
  all_features <- as.vector(summaryShaps[ , "feature"])

  # define the removable items, where lowerCI is bellow zero
  irrelevant  <- as.vector(summaryShaps[summaryShaps[, "lowerCI"] < 0, "feature"])

  # select the features
  if (is.null(top_n_features)) {
    important <- as.vector(summaryShaps[summaryShaps[,method] >= cutoff, "feature"])
    inessential <- all_features[!all_features %in% c(important, irrelevant)]
  }
  else {
    important <- as.vector(summaryShaps[1:top_n_features, "feature"])
    inessential <- all_features[!all_features %in% c(important, irrelevant)]
  }

  results <- list(
    important = important,
    inessential = inessential,
    irrelevant= irrelevant
  )

  return(results)
}
