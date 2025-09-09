#' @title Suggest Hyperparameters for tuning HMDA Grids
#' @description Suggests candidate hyperparameter values for tree-based
#'   algorithms. It computes a hyperparameter grid whose total number
#'   of model combinations is near a specified target. For GBM models,
#'   default candidates include max_depth, ntrees, learn_rate,
#'   sample_rate, and col_sample_rate. For DRF models, if a vector of predictor
#'   variables (\code{x}) and a modeling family ("regression" or "classificaiton")
#'   are provided, a vector of mtries is also suggested.
#'
#' @param algorithm  A character string specifying the algorithm, which
#'                   can be either "gbm" (gradient boosting machines) or "drf"
#'                   (distributed random forest).
#' @param n_models   An integer for the desired approximate number of
#'                   model combinations in the grid. Must be at least 100.
#' @param x          (Optional) A vector of predictor names.
#'                   If provided and its length is at least 20, it is used
#'                   to compute mtries for DRF.å
#' @param family     (Optional) A character string indicating the
#'                   modeling family. Must be either "classification"
#'                   or "regression". This is used with \code{x} to
#'                   suggest mtries.
#'
#' @return A named list of hyperparameter value vectors. This list is
#'         suitable for use with HMDA and H2O grid search functions.
#' @details
#'   The function first checks that \code{n_models} is at least 100,
#'   then validates the \code{family} parameter if provided. The
#'   algorithm name is normalized to lowercase and must be either
#'   "gbm" or "drf". For "gbm", a default grid of hyperparameters is
#'   defined. For "drf", if both \code{x} and \code{family} are provided,
#'   the function computes mtries via \code{suggest_mtries()}. If not,
#'   a default grid is set without mtries. Finally, the candidate grid is
#'   pruned or expanded using \code{hmda.adjust.params()} so that the
#'   total number of combinations is near \code{n_models}.
#'
#' @examples
#' \dontrun{
#'   library(h2o)
#'   h2o.init()
#'
#'   # Example 1: Suggest hyperparameters for GBM with about 120 models.
#'   params_gbm <- hmda.suggest.param("gbm", n_models = 120)
#'   print(params_gbm)
#'
#'   # Example 2: Suggest hyperparameters for DRF (classification) with
#'   # 100 predictors.
#'   params_drf <- hmda.suggest.param(
#'                   algorithm = "drf",
#'                   n_models = 150,
#'                   x = paste0("V", 1:100),
#'                   family = "classification"
#'                 )
#'   print(params_drf)
#' }
#'
#' @export

hmda.suggest.param <- function(algorithm,
                               n_models,
                               x = NULL,
                               family = NULL) {
  # -------------------------------------------------------------------------
  # 1) Check the arguments
  # -------------------------------------------------------------------------
  if (n_models < 100) {
    stop("Number of models cannot be less than 100. \nThe higher the number of models, the more robust HMDA becomes.")
  }

  # Match argument for safety
  if (!is.null(family)) {
    if (family != "classification" & family != "regression") {
      stop("family should be either 'classification' or 'regression'")
    }
  }

  if (length(x) < 20) x <- NULL

  # -------------------------------------------------------------------------
  # 2) Normalize algorithm name (lowercase) and validate
  # -------------------------------------------------------------------------
  algorithm <- tolower(algorithm)
  if (!algorithm %in% c("gbm", "drf")) {
    stop("Invalid algorithm. Valid options are 'gbm' or 'drf'")
  }

  # -------------------------------------------------------------------------
  # 3) Define default hyperparameters
  # -------------------------------------------------------------------------
  if (algorithm == "gbm") {
    # Common hyperparameters for H2O GBM
    hyperparams <- list(
      max_depth       = c(3, 5, 7, 9),
      ntrees          = c(50, 100, 150),
      learn_rate      = c(0.01, 0.05, 0.1),
      sample_rate     = c(0.8, 1.0),
      col_sample_rate = c(0.8, 1.0)
    )
  } else if (algorithm == "drf") {
    # Common hyperparameters for H2O DRF
    # Note: mtries = -1 means "use all features" in H2O.
    if (is.null(x) & is.null(family)) {
      hyperparams <- list(
        ntrees      = c(50, 100, 150),
        max_depth   = c(20, 15, 25, 5, 10),
        sample_rate = c(0.632, 0.6, 0.66, 0.70, 0.58)
        # mtries      = suggest_mtries(p, family = family)
        # You may add more hyperparameters if you wish (e.g., binomial_double_trees, etc.).
      )
    } else if (!is.null(x) & !is.null(family)) {
      hyperparams <- list(
        ntrees      = c(50, 100, 150),
        max_depth   = c(5, 10, 15),
        sample_rate = c(0.632, 0.6, 0.66, 0.70, 0.58),
        mtries      = suggest_mtries(length(x), family = family)
        # You may add more hyperparameters if you wish (e.g., binomial_double_trees, etc.).
      )

    # otherwise define the default
    } else {
      hyperparams <- list(
        ntrees      = c(50, 100, 150),
        max_depth   = c(20, 15, 25, 5, 10),
        sample_rate = c(0.632, 0.6, 0.66, 0.70, 0.58)
        # mtries      = suggest_mtries(p, family = family)
        # You may add more hyperparameters if you wish (e.g., binomial_double_trees, etc.).
      )
    }
  }

  # else {
  #   # XGBoost hyperparameters in H2O
  #   hyperparams <- list(
  #     ntrees           = c(50, 100, 150),
  #     max_depth        = c(3, 5, 7, 9),
  #     learn_rate       = c(0.01, 0.05, 0.1),
  #     min_rows         = c(5, 10),
  #     subsample        = c(0.8, 1.0),
  #     col_sample_rate  = c(0.8, 1.0)
  #   )
  # }

  # -------------------------------------------------------------------------
  # 4) Prune or expand hyperparameters based on n_models
  # -------------------------------------------------------------------------
  hyperparams <- hmda.adjust.params(hyperparams, n_models)

  # -------------------------------------------------------------------------
  # 5) Return the final suggestion
  # -------------------------------------------------------------------------
  return(hyperparams)
}

