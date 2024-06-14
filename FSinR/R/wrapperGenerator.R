library(caret)
#' generaWrapper
#'
#' @author Alfonso Jiménez-Vílchez
#' @author Francisco Aragón Royón
#' @title Wrapper measure generator
#' @description Generates a wrapper function to be used as an evaluator \insertCite{kohavi1997}{FSinR} in the feature selection proccess, given a learner algorithm and related customizable parameters \insertCite{caret}{FSinR}. More specifically, the result of calling this function is another function that is passed on as a parameter to the \code{\link{featureSelection}} function. However, you can also run this function directly to generate an evaluation measure.
#'
#' @param learner Learner to be used. The models available are the models available in caret: http://topepo.github.io/caret/available-models.html
#' @param resamplingParams Control parameters for evaluating the impact of model tuning parameters. The arguments are the same as those of the caret trainControl function. By default an empty list. In this case the default caret values are used for resampling and fitting.
#' @param fittingParams Control parameters for choose the best model across the parameters. The arguments are the same as those of the caret train function (minus the parameters: x, y, form, data, method and trainControl). By default an empty list. In this case the default caret values are used for resampling and fitting.
#'
#' @return Returns a wrapper function that is used to generate an evaluation measure
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#' @import caret
#' @import e1071
#' @importFrom stats as.formula
#'
#' @examples
#'\dontrun{ 
#'
#' ## Examples of a wrapper evaluator generation
#'
#' wrapper_evaluator_1 <- wrapperEvaluator('knn')
#' wrapper_evaluator_2 <- wrapperEvaluator('mlp')
#' wrapper_evaluator_3 <- wrapperEvaluator('randomForest')
#'
#'
#' ## Examples of a wrapper evaluator generation (with parameters)
#'
#' # Values for the caret trainControl function (resampling parameters)
#' resamplingParams <- list(method = "repeatedcv", repeats = 3)
#' # Values for the caret train function (fitting parameters)
#' fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy",
#'                       tuneGrid = expand.grid(k = c(1:12)))
#'                       
#' wrapper_evaluator <- wrapperEvaluator('knn', resamplingParams, fittingParams)
#' 
#' 
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly to evaluate a set of features
#' ## Classification problem
#' 
#' # Generates the wrapper evaluation function
#' wrapper_evaluator <- wrapperEvaluator('knn')
#' # Evaluates features directly (parameters: dataset, target variable and features)
#' wrapper_evaluator(iris,'Species',c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))
#' }
wrapperEvaluator <- function(learner, resamplingParams=list(), fittingParams=list()) {

  wrapperEvaluatorFunction <- function(original_data, class, features) {

    # Check for empty set of features
    if( length(features) == 0 || features[1] == ''){
      stop('An empty set of features has been received for evaluation')
    }
    # Check for missing data
    if( any( apply(original_data, 2, function(x) { any(is.na(x)) } ) ) ){
      stop('Feature selection cannot be performed with missing values. Try to impute them previously with the preProcces function of the caret package')
    }
    # Obtain only the desired columns
    train_data <- subset(original_data, select = c(features,class))

    # trainControl
    ctrl <- do.call(caret::trainControl,resamplingParams)

    # train
    modelFit <- do.call(caret::train, append(list(
                                        form = as.formula(paste( class, ".", sep = "~")),
                                        data = train_data,
                                        method = learner,
                                        trControl = ctrl),
                                    fittingParams))

    # Row number of the best parameters tuned (these parameters have achieved the best result)
    rowBestTune <- as.numeric(rownames(modelFit$bestTune))
    # Best result for the selected metric
    modelFit$results[rowBestTune,modelFit$metric]
  }


  # Determine according to the selected metric whether maximization or minimization is to be done
  if (is.null(fittingParams$metric)) { # Metric not specified in the parameters

    target <- "unspecified"

  } else {# Metric specified in the parameters

    if (fittingParams$metric %in% c("Accuracy","Kappa","ROC","Sens","Spec","Rsquared","F","Precision","Recall")) {
      target <- "maximize"
    } else if (fittingParams$metric %in% c("RMSE","MAE","logLoss")) {
      target <- "minimize"
    } else {
      stop("Metric not supported")
    }

  }
  # Added as an attribute
  attr(wrapperEvaluatorFunction, 'target') <- target
  attr(wrapperEvaluatorFunction, 'shortName') <- paste(learner, "wrapper")
  attr(wrapperEvaluatorFunction, 'name') <- paste(learner, "Wrapper")
  attr(wrapperEvaluatorFunction, 'kind') <- "Set measure"
  attr(wrapperEvaluatorFunction, 'needsDataToBeDiscrete') <- FALSE
  attr(wrapperEvaluatorFunction,'needsDataToBeContinuous') <- FALSE

  return( wrapperEvaluatorFunction )
}
attr(wrapperEvaluator,'shortName') <- "wrapperGenerator"
attr(wrapperEvaluator,'name') <- "Wrapper Measure Generator"
attr(wrapperEvaluator,'methods') <- "Available models: http://topepo.github.io/caret/available-models.html"