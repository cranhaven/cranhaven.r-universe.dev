#' @title Tune Hyperparameter Grid for HMDA Framework
#' @description Generates a hyperparameter grid for a single tree-based
#'   algorithm (either "drf" or "gbm") by running a grid search.
#'   The function validates inputs, generates an
#'   automatic grid ID for the grid (if not provided), and optionally
#'   saves the grid to a recovery directory. The resulting grid object
#'   contains all trained models and can be used for further analysis.
#'   For scientific computing, saving the grid is highly recommended
#'   to avoid future re-running the training!
#'
#' @param algorithm  Character. The algorithm to tune. Supported values
#'                   are "drf" (Distributed Random Forest) and "gbm"
#'                   (Gradient Boosting Machine). Only one algorithm
#'                   can be specified. (Case-insensitive)
#' @param grid_id    Character. Optional identifier for the grid search.
#'                   If \code{NULL}, an automatic grid_id is generated
#'                   using the algorithm name and the current time.
#' @param x          Vector. Predictor column names or indices.
#' @param y          Character. The response column name or index.
#' @param training_frame An H2OFrame containing the training data.
#'                   Default is \code{h2o.getFrame("hmda.train.hex")}.
#' @param validation_frame An H2OFrame for early stopping. Default is \code{NULL}.
#' @param hyper_params List. A list of hyperparameter vectors for tuning.
#'                     If you do not have a clue about how to specify the
#'                     hyperparameters, consider consulting \code{hmda.suggest.param}
#'                     and \code{hmda.search.param} functions, which provide
#'                     suggestions based on default values or random search.
#' @param nfolds     Integer. Number of folds for cross-validation.
#'                   Default is 10.
#' @param seed       Integer. A seed for reproducibility.
#'                   Default is \code{NULL}.
#' @param keep_cross_validation_predictions Logical. Whether to keep
#'                   cross-validation predictions. Default is \code{TRUE}.
#' @param recovery_dir  Character. Directory path to save the grid search
#'                   output. If provided, the grid is saved using
#'                   \code{h2o.saveGrid()}.
#' @param sort_by    Character. Metric used to sort the grid. Default is "logloss".
#' @param ...        Additional arguments passed to \code{h2o.grid()}.
#'
#' @return An object of class \code{H2OGrid} containing the grid search
#'         results.
#'
#' @details
#'   The function executes the following steps:
#'   \enumerate{
#'     \item \strong{Input Validation:} Ensures only one algorithm is specified
#'           and verifies that the training frame is an H2OFrame.
#'     \item \strong{Grid ID Generation:} If no \code{grid_id} is provided, it
#'           creates one using the algorithm name and the current time.
#'     \item \strong{Grid Search Execution:} Calls \code{h2o.grid()} with the
#'           provided hyperparameters and cross-validation settings.
#'     \item \strong{Grid Saving:} If a recovery directory is specified, the grid
#'           is saved to disk using \code{h2o.saveGrid()}.
#'   }
#'   The output is an H2O grid object that contains all the trained models.
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
#' }
#'
#' @importFrom h2o h2o.grid h2o.saveGrid
#' @export
#' @author E. F. Haghish

#infogram,targetencoder,deeplearning,glm,glrm,kmeans,naivebayes,pca,svd,,,,extendedisolationforest,aggregator,word2vec,stackedensemble,coxph,generic,gam,anovaglm,psvm,rulefit,upliftdrf,modelselection,isotonicregression,dt,adaboost
# , "XGBoost"
#"randomForest", "drf",  "gbm", "xrf", "isolationforest"
hmda.grid <- function(algorithm = c("drf",  "gbm"),
                      grid_id = NULL,
                      x,
                      y,
                      training_frame = h2o.getFrame("hmda.train.hex"),
                      validation_frame = NULL,
                      hyper_params = list(),

                      nfolds = 10,
                      seed = NULL,
                      keep_cross_validation_predictions = TRUE,


                      # recovery and saving
                      recovery_dir = NULL,

                      # returning the grid
                      sort_by = "logloss",

                      ...) {

  # Grammar check
  # ===========================================================
  if (length(algorithm) > 1) stop("only one algorithm is supported at the time")
  algorithm <- match.arg(algorithm) # Match argument for safety

  if (!inherits(training_frame, "H2OFrame")) {
    if (inherits(training_frame, "character")) {
      training_frame <- h2o.getFrame("hmda.train.hex")
    }
    else {
      stop("the 'training_frame' argument is not referenced to an existing H2O frame")
    }
  }

  # check and generate grid_id
  if (is.null(grid_id)) {
    grid_id <- paste0(algorithm, "_grid_", as.integer(Sys.time()))
    message(paste("an automatic 'grid_id' was generated:", grid_id))
  }

  # Define model IDs
  # ============================================================
  MODELIDS <- list()

  # There is a buggy error in h2o if validation frame is NULL, which is it's default!
  if (!is.null(validation_frame)) {
    grid <- h2o.grid(algorithm = algorithm,
                     y = y,
                     x = x,
                     training_frame = training_frame,
                     hyper_params = hyper_params,
                     grid_id = grid_id,
                     validation_frame = validation_frame,

                     # this setting ensures the models are comparable
                     seed = seed,
                     nfolds = nfolds,
                     keep_cross_validation_predictions = keep_cross_validation_predictions,
                     recovery_dir = recovery_dir,
                     ...)
  }
  else {
    grid <- h2o.grid(algorithm = algorithm,
                     y = y,
                     x = x,
                     training_frame = training_frame,
                     hyper_params = hyper_params,
                     grid_id = grid_id,

                     # this setting ensures the models are comparable
                     seed = seed,
                     nfolds = nfolds,
                     keep_cross_validation_predictions = keep_cross_validation_predictions,
                     recovery_dir = recovery_dir,
                     ...)
  }

  # Save the models if required
  # ============================================================
  if (!is.null(recovery_dir)) {
    h2o.saveGrid(
      grid_directory = paste0(recovery_dir,"/",grid_id),
      grid_id = grid_id,
      save_params_references = FALSE,
      export_cross_validation_predictions = TRUE
    )
  }

  return(grid)
}
