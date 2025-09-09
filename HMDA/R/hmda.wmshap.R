#' @title Compute Weighted Mean SHAP Values and Confidence Intervals via shapley algorithm
#' @description This function is a wrapper for shapley package that computes the
#'   Weighted Mean SHAP (WMSHAP) values and corresponding confidence intervals for a
#'   grid of models (or an ensemble of base-learners) by calling the
#'   \code{shapley()} function. It uses the specified performance metric to assess the
#'   models' performances and use the metric as a weight
#'   and returns both the weighted mean SHAP values and, if requested, a plot of these
#'   values with confidence intervals. This approach considers the variability of feature
#'   importance across multiple models rather than reporting SHAP values from a single model.
#'   for more details about shapley algotithm, see \url{https://github.com/haghish/shapley}
#'
#'
#' @param models A grid object, an AutoML grid, an autoEnsemble object, or a character
#'   vector of H2O model IDs from which the SHAP values will be computed.
#' @param newdata An H2OFrame (or data frame already uploaded to the H2O server) on which
#'   the SHAP values will be evaluated.
#' @param plot Logical. If \code{TRUE}, a plot of the weighted mean SHAP values along with
#'   their confidence intervals is generated. Default is \code{TRUE}.
#' @param performance_metric Character. Specifies the performance metric to be used as
#'   weights for the SHAP values. The default is \code{"r2"}. For binary classification,
#'   alternatives include \code{"aucpr"}, \code{"auc"}, and \code{"f2"}.
#' @param standardize_performance_metric Logical. If \code{TRUE}, the performance metric
#'   (used as the weights vector) is standardized so that the sum of the weights equals
#'   the length of the vector. Default is \code{FALSE}.
#' @param performance_type Character. Specifies whether the performance metric should be
#'   retrieved from the training data ("train"), validation data ("valid"), or
#'   cross-validation ("xval"). Default is \code{"xval"}.
#' @param minimum_performance Numeric. The minimum performance threshold; any model with
#'   a performance equal to or lower than this threshold will have a weight of zero in
#'   the weighted SHAP calculation. Default is \code{0}.
#' @param method Character. Specify the method for selecting important features
#'               based on their weighted mean SHAP ratios. The default is
#'               \code{"mean"}, which selects features whose weighted mean shap ratio (WMSHAP)
#'               exceeds the \code{cutoff}. The alternative is
#'               \code{"lowerCI"}, which selects features whose lower bound of confidence
#'               interval exceeds the \code{cutoff}.
#' @param cutoff Numeric. The cutoff value used in the feature selection method
#'   (default is \code{0.01}).
#' @param top_n_features Integer. If specified, only the top \code{n} features with the
#'   highest weighted SHAP values will be selected, overriding the cutoff and method.
#'   Default is \code{NULL}, which means all features are considered.
#' @param n_models Integer. The minimum number of models that must meet the
#'   \code{minimum_performance} criterion in order to compute the weighted mean and
#'   confidence intervals of SHAP values. Set to \code{1} if a global summary for a
#'   single model is desired. The default is \code{10}.
#' @param sample_size Integer. The number of rows in \code{newdata} to use for the SHAP
#'   evaluation. By default, all rows of \code{newdata} are used.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{plot}{A ggplot2 object showing the weighted mean SHAP values and
#'           confidence intervals (if \code{plot = TRUE}).}
#'     \item{shap_values}{A data frame of the weighted mean SHAP values and confidence
#'           intervals for each feature.}
#'     \item{performance}{A data frame of performance metrics for all models used
#'           in the analysis.}
#'     \item{model_ids}{A vector of model IDs corresponding to the models evaluated.}
#'   }
#'
#' @details
#'   This function is designed as a wrapper for the HMDA package and calls the
#'   \code{shapley()} function from the \pkg{shapley} package. It computes the weighted
#'   average of SHAP values across multiple models, using a specified performance
#'   metric (e.g., R Squared, AUC, etc.) as the weight. The performance metric can be
#'   standardized if required. Additionally, the function selects top features based on
#'   different methods (e.g., \code{"mean"} or \code{"lowerCI"}) and
#'   can limit the number of features considered via \code{top_n_features}. The
#'   \code{n_models} parameter controls how many models must meet a minimum performance
#'   threshold to be included in the weighted SHAP calculation.
#'
#'   For more information on the shapley and WMSHAP approaches used in HMDA,
#'   please refer to the shapley package documentation and the following resources:
#'   \itemize{
#'     \item shapley GitHub: \url{https://github.com/haghish/shapley}
#'     \item shapley CRAN: \url{https://CRAN.R-project.org/package=shapley}
#'   }
#'
#' @importFrom shapley shapley
#' @importFrom utils setTxtProgressBar txtProgressBar globalVariables
#' @importFrom stats weighted.mean
#' @importFrom h2o h2o.stackedEnsemble h2o.getModel h2o.auc h2o.aucpr h2o.r2
#'             h2o.F2 h2o.mean_per_class_error h2o.giniCoef h2o.accuracy
#'             h2o.shap_summary_plot
# @importFrom h2otools h2o.get_ids
#' @importFrom curl curl
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar coord_flip ggtitle xlab
#'             ylab theme_classic theme scale_y_continuous margin expansion

#' @return a list including the GGPLOT2 object, the data frame of SHAP values,
#'         and performance metric of all models, as well as the model IDs.
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
#' }
#' @export
#' @author E. F. Haghish

hmda.wmshap <- function(models,
                        newdata,
                        #nboot = NULL,
                        plot = TRUE,
                        performance_metric = "r2",
                        standardize_performance_metric = FALSE,
                        performance_type = "xval",
                        minimum_performance = 0,
                        method = c("mean"),
                        cutoff = 0.01,
                        top_n_features = NULL,
                        n_models = 10,
                        sample_size = nrow(newdata)
                        #normalize_to = "upperCI"
) {
  return(
    shapley(models = models,
            newdata = newdata,
            #nboot = NULL,
            plot = plot,
            performance_metric = performance_metric,
            standardize_performance_metric = standardize_performance_metric,
            performance_type = performance_type,
            minimum_performance = minimum_performance,
            method = method,
            cutoff = cutoff,
            top_n_features = top_n_features,
            n_models = n_models,
            sample_size = nrow(newdata)
            #normalize_to = "upperCI"
            )
  )
}
