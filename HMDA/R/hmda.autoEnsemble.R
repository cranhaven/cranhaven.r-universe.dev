#' @title Build Stacked Ensemble Model Using autoEnsemble R package
#' @description This function is a wrapper within the HMDA package that
#'   builds a stacked ensemble model by combining multiple H2O models. It
#'   leverages the \pkg{autoEnsemble} package to stack a set of trained models
#'   (e.g., from HMDA grid) into a stronger meta-learner. For more
#'   details on autoEnsemble, please see the GitHub repository at
#'   \url{https://github.com/haghish/autoEnsemble} and the CRAN package of
#'   autoEnsemble R package.
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.mcc
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
# @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @importFrom autoEnsemble ensemble
#'
#' @param models A grid object, such as HMDA grid, or a character vector of H2O model IDs.
#'   The \code{h2o.get_ids} function from \pkg{h2otools} can be used to extract model
#'   IDs from grids.
#' @param training_frame An H2OFrame (or data frame already uploaded to the H2O server)
#'   that contains the training data used to build the base models.
#' @param newdata An H2OFrame (or data frame already uploaded to the H2O server) to be used
#'   for evaluating the ensemble. If not specified, performance on the training data is used
#'   (for instance, cross-validation performance).
#' @param family A character string specifying the model family.
#' @param strategy A character vector specifying the ensemble strategy. The available
#'   strategy is \code{"search"} (default). The \code{"search"} strategy searches for
#'   the best combination of top-performing diverse models.
#' @param model_selection_criteria A character vector specifying the performance metrics
#'   to consider for model selection. The default is \code{c("auc", "aucpr", "mcc", "f2")}.
#'   Other possible criteria include \code{"f1point5"}, \code{"f3"}, \code{"f4"},
#'   \code{"f5"}, \code{"kappa"}, \code{"mean_per_class_error"}, \code{"gini"}, and
#'   \code{"accuracy"}.
#' @param min_improvement Numeric. The minimum improvement in the evaluation metric
#'   required to continue the ensemble search.
#' @param max Integer. The maximum number of models for each selection criterion.
#'   If \code{NULL}, a default value based on the top rank percentage is used.
#' @param top_rank Numeric vector. Specifies the percentage (or percentages) of the
#'   top models that should be considered for ensemble selection. If the strategy is
#'   \code{"search"}, the function searches for the best combination of models from
#'   the top to the bottom ranked; if the strategy is \code{"top"}, only the first value
#'   is used. Default is \code{seq(0.01, 0.99, 0.01)}.
#' @param stop_rounds Integer. The number of consecutive rounds with no improvement
#'   in the performance metric before stopping the search.
#' @param reset_stop_rounds Logical. If \code{TRUE}, the stopping rounds counter is
#'   reset each time an improvement is observed.
#' @param stop_metric Character. The metric used for early stopping; the default is
#'   \code{"auc"}. Other options include \code{"aucpr"} and \code{"mcc"}.
#' @param seed Integer. A random seed for reproducibility. Default is \code{-1}.
#' @param verbatim Logical. If \code{TRUE}, the function prints additional
#'   progress information for debugging purposes.
#'
#' @return A list containing:
#'   \describe{
#'     \item{model}{The ensemble model built by autoEnsemble.}
#'     \item{top_models}{A data frame of the top-ranked base models that were used
#'           in building the ensemble.}
#'   }
#'
#'
#' @details
#'   This wrapper function integrates with the HMDA package workflow to build a
#'   stacked ensemble model from a set of base H2O models. It calls the
#'   \code{ensemble()} function from the \pkg{autoEnsemble} package to construct the
#'   ensemble. The function is designed to work within HMDA's framework, where base
#'   models are generated via grid search or AutoML. For more details on the autoEnsemble
#'   approach, see:
#'   \itemize{
#'     \item GitHub: \url{https://github.com/haghish/autoEnsemble}
#'     \item CRAN: \url{https://CRAN.R-project.org/package=autoEnsemble}
#'   }
#'
#'   The ensemble strategy \code{"search"} (default) searches for the best combination
#'   of top-performing and diverse models to improve overall performance. The wrapper
#'   returns both the final ensemble model and the list of top-ranked models used in the
#'   ensemble.
#'
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
#' }
#' @export
#' @author E. F. Haghish

hmda.autoEnsemble <- function(models,
                              training_frame,
                              newdata = NULL,
                              family = "binary",
                              strategy = c("search"),
                              model_selection_criteria = c("auc","aucpr","mcc","f2"),
                              min_improvement = 0.00001,
                              max = NULL,
                              top_rank = seq(0.01, 0.99, 0.01),
                              stop_rounds = 3,
                              reset_stop_rounds = TRUE,
                              stop_metric = "auc",
                              seed = -1,
                              verbatim = FALSE) {

  return(
    ensemble(
      models = models,
      training_frame = training_frame,
      newdata = newdata,
      family = family,
      strategy = strategy,
      model_selection_criteria = model_selection_criteria,
      min_improvement = min_improvement,
      max = max,
      top_rank = top_rank,
      stop_rounds = stop_rounds,
      reset_stop_rounds = reset_stop_rounds,
      stop_metric = stop_metric,
      seed = seed,
      verbatim = verbatim
    )
  )
}
