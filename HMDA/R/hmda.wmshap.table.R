#' @title Create SHAP Summary Table Based on the Given Criterion
#' @description Generates a summary table of weighted mean SHAP (WMSHAP) values
#'   and confidence intervals for each feature based on a weighted SHAP analysis.
#'   The function filters the SHAP summary table (from a \code{wmshap} object) by
#'   selecting features that meet or exceed a specified cutoff using a selection
#'   method (default "mean"). It then sorts the table by the mean SHAP value,
#'   formats the SHAP values along with their 95\% confidence intervals into a single
#'   string, and optionally adds human-readable feature descriptions from a provided
#'   dictionary. The output is returned as a markdown table using the \pkg{pander}
#'   package, or as a data frame if requested.
#'
#' @param wmshap             A wmshap object, returned by the hmda.wmshap function
#'                           containing a data frame \code{summaryShaps}.
#' @param method Character. Specify the method for selecting important features
#'               based on their weighted mean SHAP ratios. The default is
#'               \code{"mean"}, which selects features whose weighted mean shap ratio (WMSHAP)
#'               exceeds the \code{cutoff}. The alternative is
#'               \code{"lowerCI"}, which selects features whose lower bound of confidence
#'               interval exceeds the \code{cutoff}.
#' @param cutoff             Numeric. The threshold cutoff for the selection method;
#'                           only features with a value in the \code{method} column
#'                           greater than or equal to this value are retained.
#'                           Default is \code{0.01}.
#' @param round              Integer. The number of decimal places to round the
#'                           SHAP mean and confidence interval values. Default is
#'                           \code{3}.
#' @param exclude_features   Character vector. A vector of feature names to be
#'                           excluded from the summary table. Default is \code{NULL}.
#' @param dict               A data frame containing at least two columns named
#'                           \code{"name"} and \code{"description"}. If provided, the
#'                           function uses this dictionary to add human-readable feature
#'                           descriptions. Default is \code{NULL}.
#' @param markdown.table     Logical. If \code{TRUE}, the output is formatted as a
#'                           markdown table using the \pkg{pander} package; otherwise, a
#'                           data frame is returned. Default is \code{TRUE}.
#' @param split.tables       Integer. Controls table splitting in \code{pander()}.
#'                           Default is \code{120}.
#' @param split.cells        Integer. Controls cell splitting in \code{pander()}.
#'                           Default is \code{50}.
#'
#' @return If \code{markdown.table = TRUE}, returns a markdown table (invisibly)
#'         showing two columns: \code{"Description"} and \code{"WMSHAP"}. If
#'         \code{markdown.table = FALSE}, returns a data frame with these columns.
#'
#  @details
#    The function works as follows:
#    \enumerate{
#      \item Filters the \code{summaryShaps} data frame from the \code{wmshap}
#            object to retain only those features for which the value in the
#            \code{method} column is greater than or equal to the \code{cutoff}.
#      \item Excludes any features specified in \code{exclude_features}.
#      \item Sorts the filtered data frame in descending order by the \code{mean}
#            SHAP value.
#      \item Rounds the \code{mean}, \code{lowerCI}, and \code{upperCI} columns to
#            the specified number of decimal places.
#      \item Constructs a new \code{WMSHAP} column by concatenating the mean value
#            with its confidence interval.
#      \item Adds a \code{Description} column using the provided \code{dict} if available;
#            otherwise, uses the feature name.
#      \item Returns the final table either as a markdown table (via \pkg{pander}) or
#            as a data frame.
#    }
#'
#' @examples
#' \dontrun{
#'   library(HMDA)
#'   library(h2o)
#'   hmda.init()
#'
#'   # Import a sample binary outcome dataset into H2O
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
#'   params <- list(learn_rate = c(0.01, 0.1),
#'                  max_depth = c(3, 5, 9),
#'                  sample_rate = c(0.8, 1.0)
#'   )
#'
#'   # Train and validate a cartesian grid of GBMs
#'   hmda_grid1 <- hmda.grid(algorithm = "gbm", x = x, y = y,
#'                           grid_id = "hmda_grid1",
#'                           training_frame = train,
#'                           nfolds = 10,
#'                           ntrees = 100,
#'                           seed = 1,
#'                           hyper_params = gbm_params1)
#'
#'   # Assess the performances of the models
#'   grid_performance <- hmda.grid.analysis(hmda_grid1)
#'
#'   # Return the best 2 models according to each metric
#'   hmda.best.models(grid_performance, n_models = 2)
#'
#'   # build an autoEnsemble model & test it with the testing dataset
#'   meta <- hmda.autoEnsemble(models = hmda_grid1, training_frame = train)
#'   print(h2o.performance(model = meta$model, newdata = test))
#'
#'   # compute weighted mean shap values
#'   wmshap <- hmda.wmshap(models = hmda_grid1,
#'                         newdata = test,
#'                         performance_metric = "aucpr",
#'                         standardize_performance_metric = FALSE,
#'                         performance_type = "xval",
#'                         minimum_performance = 0,
#'                         method = "mean",
#'                         cutoff = 0.01,
#'                         plot = TRUE)
#'
#'   # identify the important features
#'   selected <- hmda.feature.selection(wmshap,
#'                                      method = c("mean"),
#'                                      cutoff = 0.01)
#'   print(selected)
#'
#'   # View the plot of weighted mean SHAP values and confidence intervals
#'   print(wmshap$plot)
#'
#'   # get the wmshap table output in Markdown format:
#'   md_table <- shapley.table(wmshap = wmshap,
#'                             method = "mean",
#'                             cutoff = 0.01,
#'                             round = 3,
#'                             markdown.table = TRUE)
#'   head(md_table)
#' }
#'
#' @export
#' @author E. F. Haghish

hmda.wmshap.table <- function(wmshap,
                          method = c("mean"),
                          cutoff = 0.01,
                          round = 3,
                          exclude_features = NULL,
                          dict = dictionary(raw, attribute = "label"),
                          markdown.table = TRUE,
                          split.tables = 120,
                          split.cells = 50) {

  return(shapley::shapley.table(wmshap = wmshap,
                         method = method,
                         cutoff = cutoff,
                         round = round,
                         exclude_features = exclude_features,
                         dict = dict,
                         markdown.table = markdown.table,
                         split.tables = split.tables,
                         split.cells = split.cells))
}
