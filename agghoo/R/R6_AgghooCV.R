#' @title R6 class with agghoo functions fit() and predict().
#'
#' @description
#' Class encapsulating the methods to run to obtain the best predictor
#' from the list of models (see 'Model' class).
#'
#' @importFrom R6 R6Class
#'
#' @export
AgghooCV <- R6::R6Class("AgghooCV",
  public = list(
    #' @description Create a new AgghooCV object.
    #' @param data Matrix or data.frame
    #' @param target Vector of targets (generally numeric or factor)
    #' @param task "regression" or "classification".
    #'             Default: classification if target not numeric.
    #' @param gmodel Generic model returning a predictive function
    #'               Default: tree if mixed data, knn/ppr otherwise.
    #' @param loss Function assessing the error of a prediction
    #'             Default: error rate or mean(abs(error)).
    initialize = function(data, target, task, gmodel, loss) {
      private$data <- data
      private$target <- target
      private$task <- task
      private$gmodel <- gmodel
      private$loss <- loss
    },
    #' @description Fit an agghoo model.
    #' @param CV List describing cross-validation to run. Slots: \cr
    #'          - type: 'vfold' or 'MC' for Monte-Carlo (default: MC) \cr
    #'          - V: number of runs (default: 10) \cr
    #'          - test_size: percentage of data in the test dataset, for MC
    #'            (irrelevant for V-fold). Default: 0.2. \cr
    #'          - shuffle: wether or not to shuffle data before V-fold.
    #'            Irrelevant for Monte-Carlo; default: TRUE \cr
    #'        Default (if NULL): type="MC", V=10, test_size=0.2
    fit = function(CV = NULL) {
      CV <- checkCV(CV)
      n <- nrow(private$data)
      shuffle_inds <- NULL
      if (CV$type == "vfold" && CV$shuffle)
        shuffle_inds <- sample(n, n)
      # Result: list of V predictive models (+ parameters for info)
      private$pmodels <- list()
      for (v in seq_len(CV$V)) {
        # Prepare train / test data and target, from full dataset.
        # dataHO: "data Hold-Out" etc.
        test_indices <- get_testIndices(n, CV, v, shuffle_inds)
        d <- splitTrainTest(private$data, private$target, test_indices)
        best_model <- NULL
        best_error <- Inf
        for (p in seq_len(private$gmodel$nmodels)) {
          model_pred <- private$gmodel$get(d$dataTrain, d$targetTrain, p)
          prediction <- model_pred(d$dataTest)
          error <- private$loss(prediction, d$targetTest)
          if (error <= best_error) {
            newModel <- list(model=model_pred, param=private$gmodel$getParam(p))
            if (error == best_error)
              best_model[[length(best_model)+1]] <- newModel
            else {
              best_model <- list(newModel)
              best_error <- error
            }
          }
        }
        # Choose a model at random in case of ex-aequos
        private$pmodels[[v]] <- best_model[[ sample(length(best_model),1) ]]
      }
    },
    #' @description Predict an agghoo model (after calling fit())
    #' @param X Matrix or data.frame to predict
    predict = function(X) {
      if (!is.matrix(X) && !is.data.frame(X))
        stop("X: matrix or data.frame")
      if (!is.list(private$pmodels)) {
        warning("Please call $fit() method first")
        return (invisible(NULL))
      }
      V <- length(private$pmodels)
      oneLineX <- X[1,]
      if (is.matrix(X))
        # HACK: R behaves differently with data frames and matrices.
        oneLineX <- t(as.matrix(oneLineX))
      if (length(private$pmodels[[1]]$model(oneLineX)) >= 2)
        # Soft classification:
        return (Reduce("+", lapply(private$pmodels, function(m) m$model(X))) / V)
      n <- nrow(X)
      all_predictions <- as.data.frame(matrix(nrow=n, ncol=V))
      for (v in 1:V)
        all_predictions[,v] <- private$pmodels[[v]]$model(X)
      if (private$task == "regression")
        # Easy case: just average each row
        return (rowMeans(all_predictions))
      # "Hard" classification:
      apply(all_predictions, 1, function(row) {
        t <- table(row)
        # Next lines in case of ties (broken at random)
        tmax <- max(t)
        sample( names(t)[which(t == tmax)], 1 )
      })
    },
    #' @description Return the list of V best parameters (after calling fit())
    getParams = function() {
      lapply(private$pmodels, function(m) m$param)
    }
  ),
  private = list(
    data = NULL,
    target = NULL,
    task = NULL,
    gmodel = NULL,
    loss = NULL,
    pmodels = NULL
  )
)
