
#' create.prediction
#'
#' @keywords internal
#'
create.prediction  <- function(model, prediction){
  prediction <- list(prediction = prediction, "var.pred" =  model$prmdt$var.pred)
  class(prediction) <- c("prediction.prmdt", "list")
  return(prediction)
}

#' predict.qda.prmdt
#'
#' @description Return prediction for a \code{\link[MASS]{qda}} model.
#'
#' @param object a \code{\link[MASS]{qda}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom MASS qda
#'
#' @return a vector or matrix of predictions for qda model.
#'
#' @export predict.qda.prmdt
#' @export
#'
predict.qda.prmdt <- function(object, newdata, type = "class", ...){
  if(type == "class"){
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), ...)$class
  }
  else if(type == "prob"){
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), ...)$posterior
  }
  else{
    stop("invalid type for prediction")
  }

  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.lda.prmdt
#'
#' @description Return prediction for a \code{\link[MASS]{lda}} model.
#'
#' @param object a \code{\link[MASS]{lda}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom MASS lda
#'
#' @return a vector or matrix of predictions for lda model.
#'
#' @export predict.lda.prmdt
#' @export
#'
predict.lda.prmdt <- function(object, newdata, type = "class", ...){
  if(type == "class"){
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), ...)$class
  }
  else if(type == "prob"){
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), ...)$posterior
  }
  else{
    stop("invalid type for prediction")
  }

  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.ada.prmdt
#'
#' @description Return prediction for a \code{\link[ada]{ada}} model.
#'
#' @param object a \code{\link[ada]{ada}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param n.iter number of iterations to consider for the prediction. By default this is iter from the ada call (n.iter< iter).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#' @importFrom stringr str_detect
#'
#' @return a vector or matrix of predictions for ada model.
#'
#' @export predict.ada.prmdt
#' @export
#'
predict.ada.prmdt <- function(object, newdata, type = "class", n.iter = NULL, ...){
  type <- ifelse(type == "class", "vector", type)
  ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), type, n.iter, ...)
  if(type == "prob"){
    colnames(ans) <- object$prmdt$levels
  }else{
    ans <- type_correction(object, ans, type == "vector")
  }
  return(create.prediction(object, ans))
}

#' predict.adabag.prmdt
#'
#' @description Return prediction for a \code{\link[adabag]{boosting}} model.
#'
#' @param object a \code{\link[adabag]{boosting}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom adabag boosting
#'
#' @return a vector or matrix of predictions adabag model.
#'
#' @export predict.adabag.prmdt
#' @export
#'
predict.adabag.prmdt <- function(object, newdata, type = "class",...){
  if(type == "class"){
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), ...)$class
    ans <- type_correction(object, ans, TRUE)
  }
  else if(type == "prob"){
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), ...)$prob
    colnames(ans) <- object$prmdt$levels
  }
  else{
    stop("invalid type for prediction")
  }
  return(create.prediction(object, ans))
}

#' predict.gbm.prmdt
#'
#' @description Return prediction for a \code{\link[gbm]{gbm}} model.
#'
#' @param object a \code{\link[gbm]{gbm}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param n.trees Number of trees used in the prediction. n.trees may be a vector in which case predictions are returned for each iteration specified
#' @param single.tree If single.tree=TRUE then predict.gbm returns only the predictions from tree(s) n.trees.
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom gbm gbm
#'
#' @return a vector or matrix of predictions gbm model.
#'
#' @export predict.gbm.prmdt
#' @export
#'
predict.gbm.prmdt <- function(object, newdata, type = "class", n.trees = NULL, single.tree = FALSE, ...) {

  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), newdata, type = "response", n.trees = n.trees, single.tree = single.tree, ...)
    ans <- type_correction(object, ans, type == "class")
  } else {
    ans <- predict(original_model(object), newdata, type = "response", n.trees = n.trees, single.tree = single.tree, ...)

    if(is.null(dim(ans))) {
      if(type == "class") {
        ans <- factor(ifelse(ans > 0.5, object$prmdt$levels[2], object$prmdt$levels[1]))
      }
    } else {
      if(type == "class") {
        ans <- apply(ans, 1, which.max)
        ans <- factor(object$prmdt$levels[ans])
      } else {
        colnames(ans) <- object$prmdt$levels
      }
    }
    ans <- type_correction(object, ans, type == "class")
  }

  return(create.prediction(object, ans))
}

#' predict.bayes.prmdt
#'
#' @description Return prediction for a \code{\link[e1071]{naiveBayes}} model.
#'
#' @param object a \code{\link[e1071]{naiveBayes}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param threshold Value replacing cells with 0 probabilities.
#' @param eps double for specifying an epsilon-range to apply laplace smoothing (to replace zero or close-zero probabilities by theshold).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#'
#' @return a vector or matrix of predictions for bayes model.
#'
#' @export predict.bayes.prmdt
#' @export
#'
predict.bayes.prmdt <- function(object, newdata, type = "class", threshold = 0.001, eps = 0, ...) {
  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), NULL, threshold, eps, ...)
    ans <- as.numeric(as.character(ans))
  } else {
    type <- ifelse(type == "prob", "raw", type)
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), type, threshold, eps, ...)
  }

  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.knn.prmdt
#'
#' @description Return prediction for a \code{\link[kknn]{train.kknn}} model.
#'
#' @param object a \code{\link[kknn]{train.kknn}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#'
#' @return a vector or matrix of predictions for knn model.
#'
#' @export predict.knn.prmdt
#' @export
#'
predict.knn.prmdt <- function(object, newdata, type = "class", ...) {
  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), type = "raw", get_test_less_predict(newdata, object$prmdt$var.pred), ...)
  } else {
    type <- ifelse(type == "class", "raw", type)
    ans <- predict(original_model(object), type = type, get_test_less_predict(newdata, object$prmdt$var.pred), ...)
  }

  ans <- type_correction(object, ans, type == "raw")
  return(create.prediction(object, ans))
}

#' predict.nnet.prmdt
#'
#' @description Return prediction for a \code{\link[nnet]{nnet}} model.
#'
#' @param object a \code{\link[nnet]{nnet}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#'
#' @return a vector or matrix of predictions for nnet model.
#'
#' @export predict.nnet.prmdt
#' @export
#'
predict.nnet.prmdt <- function(object, newdata, type = "class", ...) {
  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object),  get_test_less_predict(newdata, object$prmdt$var.pred), type = "raw", ...)
    ans <- ans[, 1]
  } else {
    type <- ifelse(type == "prob", "raw", type)
    ans <- predict(original_model(object),  get_test_less_predict(newdata, object$prmdt$var.pred), type, ...)

    num.class <- length(object$prmdt$levels)

    if(type == "raw"){
      if(num.class == 2){
        ans <- cbind(1 - ans, ans)
        colnames(ans) <- object$prmdt$levels
      }
    }
  }

  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.neuralnet.prmdt
#'
#' @description Return prediction for a \code{\link[neuralnet]{neuralnet}} model.
#'
#' @param object a \code{\link[neuralnet]{neuralnet}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#'
#' @return a vector or matrix of predictions for neuralnet.
#'
#' @export predict.neuralnet.prmdt
#' @export
#'
predict.neuralnet.prmdt <- function(object, newdata, type = "class", ...) {
  selector <- unlist(lapply(newdata, is.ordered))

  if(any(selector)){
    newdata[,selector] <- lapply(newdata[,selector, drop = FALSE], function(x) factor(x, ordered = FALSE, levels = levels(x)) )
  }

  var.predict <- object$prmdt$var.pred
  selector <- which(colnames(newdata) == var.predict)

  if(length(selector) != 0){
    suppressWarnings(newdata <- dummy.data.frame(newdata[, -selector, drop = FALSE]))
  }
  else{
    suppressWarnings(newdata <- dummy.data.frame(newdata))
  }

  if("prmdt.regression" %in% class(object)) {
    ans <- neuralnet::compute(original_model(object), newdata)
    if(type == "all"){
      return(create.prediction(object, ans))
    }

    ans <- ans$net.result[, 1]
    ans <- type_correction(object, ans, type == "class")
  } else {
    ans <- neuralnet::compute(original_model(object), newdata)

    if(type == "all"){
      return(create.prediction(object, ans))
    }

    ans <- ans$net.result
    colnames(ans) <- object$prmdt$levels

    if(type == "class"){
      ans <- max_col(ans)
      ans <- numeric_to_predict(predic.var = ans, niveles = object$prmdt$levels)
      ans <- type_correction(object, ans, type == "class")
    }
  }

  return(create.prediction(object, ans))
}

#' predict.randomForest.prmdt
#'
#' @description Return prediction for a \code{\link[randomForest]{randomForest}} model.
#'
#' @param object a \code{\link[randomForest]{randomForest}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param norm.votes Should the vote counts be normalized (i.e., expressed as fractions)? Ignored if object$type is regression.
#' @param predict.all Should the predictions of all trees be kept?
#' @param proximity Should proximity measures be computed? An error is issued if object$type is regression.
#' @param nodes Should the terminal node indicators (an n by ntree matrix) be return? If so, it is in the ``nodes'' attribute of the returned object.
#' @param cutoff (Classification only) A vector of length equal to number of classes. The `winning' class for an observation is the one with the maximum ratio of proportion of votes to cutoff. Default is taken from the forest$cutoff component of object (i.e., the setting used when running randomForest).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#'
#' @return a vector or matrix of predictions for randomforest model.
#'
#' @export predict.randomForest.prmdt
#' @export
#'
predict.randomForest.prmdt <- function(object, newdata, type = "class", norm.votes = TRUE, predict.all = FALSE, proximity = FALSE, nodes = FALSE, cutoff, ...){
  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), "response", norm.votes, predict.all, proximity, nodes, cutoff, ...)
    ans <- type_correction(object, ans, type == "response")
  } else {
    type <- ifelse(type == "class", "response", type)
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), type, norm.votes, predict.all, proximity, nodes, cutoff, ...)
    if(type == "prob"){
      class(ans) <- "matrix"
    }else{
      ans <- type_correction(object, ans, type == "response")
    }
  }

  return(create.prediction(object, ans))
}

#' predict.rpart.prmdt
#'
#' @importFrom stats na.pass predict
#'
#' @description Return prediction for a \code{\link[rpart]{rpart}} model.
#'
#' @param object a \code{\link[rpart]{rpart}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param na.action a function to determine what should be done with missing values in newdata. The default is to pass them down the tree using surrogates in the way selected when the model was built. Other possibilities are na.omit and na.fail.
#' @param ... additional arguments affecting the predictions produced.
#'
#' @return a vector or matrix of predictions for rpart model.
#'
#' @export predict.rpart.prmdt
#' @export
#'
predict.rpart.prmdt <- function(object, newdata, type = "class", na.action = na.pass, ...) {

  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), newdata, na.action = na.action, ...)
  } else {
    ans <- predict(original_model(object), newdata, type, na.action, ...)
  }

  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.svm.prmdt
#'
#' @description Return prediction for a \code{\link[e1071]{svm}} model.
#'
#' @param object a \code{\link[e1071]{svm}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param decision.values Logical controlling whether the decision values of all binary classifiers computed in multiclass classification shall be computed and returned.
#' @param na.action A function to specify the action to be taken if ‘NA’s are found. The default action is na.omit, which leads to rejection of cases with missing values on any required variable. An alternative is na.fail, which causes an error if NA cases are found. (NOTE: If given, this argument must be named.)
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict na.omit
#'
#' @return a vector or matrix of predictionsfor svm model.
#'
#' @export predict.svm.prmdt
#' @export
#'
predict.svm.prmdt <- function(object, newdata, type = "class", decision.values = FALSE, ..., na.action = na.omit) {
  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), newdata, decision.values, probability = F, ..., na.action = na.action)
    ans <- type_correction(object, ans,  type == "class")
  } else {
    ans <- predict(original_model(object), newdata, decision.values, probability = type == "prob", ..., na.action = na.action)
    if(type == "prob"){
      ans <- attr(ans, "probabilities")
      ans <- ans[,object$prmdt$levels]
    }else{
      ans <- type_correction(object, ans,  type == "class")
    }
  }

  return(create.prediction(object, ans))
}

#' predict.xgb.Booster
#'
#' @description Return prediction for a \code{\link[xgboost]{xgb.train}} model.
#'
#' @param object a \code{\link[xgboost]{xgb.train}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param missing Missing is only used when input is dense matrix. Pick a float value that represents missing values in data (e.g., sometimes 0 or some other extreme value is used).
#' @param outputmargin whether the prediction should be returned in the for of original untransformed sum of predictions from boosting iterations' results. E.g., setting outputmargin=TRUE for logistic regression would result in predictions for log-odds instead of probabilities.
#' @param ntreelimit Deprecated, use iterationrange instead.
#' @param predleaf whether predict leaf index.
#' @param predcontrib whether to return feature contributions to individual predictions (see Details).
#' @param approxcontrib whether to use a fast approximation for feature contributions (see Details).
#' @param predinteraction whether to return contributions of feature interactions to individual predictions (see Details).
#' @param reshape whether to reshape the vector of predictions to a matrix form when there are several prediction outputs per case. This option has no effect when either of predleaf, predcontrib, or predinteraction flags is TRUE.
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#'
#' @return a vector or matrix of predictions for xgb model.
#'
#' @export predict.xgb.Booster.prmdt
#' @export
#'
predict.xgb.Booster.prmdt <- function(object, newdata, type = "class", missing = NA, outputmargin = FALSE, ntreelimit = NULL, predleaf = FALSE, predcontrib = FALSE,
                                approxcontrib = FALSE, predinteraction = FALSE, reshape = FALSE, ...){

  .colnames <- all.vars(object$prmdt$vars)
  var.pred <-  object$prmdt$var.pred
  selector <- which(colnames(newdata) == var.pred)

  if(length(.colnames) == 1 && .colnames == "."){
    if(length(selector) != 0){
      .colnames <- colnames(newdata[,-selector, drop = FALSE])
    }
    else{
      .colnames <- colnames(newdata)
    }
  }

  test_aux <- newdata |> select(c(.colnames))  |> select_on_class(c("numeric","integer", "factor"))
  test_aux[] <- lapply(test_aux, as.numeric)
  test_aux  <- xgb.DMatrix(data = data.matrix(test_aux))

  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), test_aux, missing, outputmargin, ntreelimit, predleaf, predcontrib, approxcontrib, predinteraction, reshape, ...)
  } else {
    ans <- predict(original_model(object), test_aux, missing, outputmargin, ntreelimit, predleaf, predcontrib, approxcontrib, predinteraction, reshape, ...)

    num.class <- length(object$prmdt$levels)

    if(type == "class") {
      if(num.class > 2) {
        ans <- max.col(matrix(ans, ncol = num.class, byrow = TRUE))
      } else {
        ans <- ifelse(ans > 0.5, 2, 1)
      }
      ans <- numeric_to_predict(predic.var = ans, niveles = object$prmdt$levels)
    }

    if(type == "prob") {
      if(num.class > 2) {
        ans <- matrix(ans, ncol = num.class, byrow = TRUE)
      } else {
        ans <- matrix(ans, ncol = 1, byrow = TRUE)
        ans <- cbind(1 - ans, ans)
      }
      colnames(ans) <- object$prmdt$levels
    }
  }

  ans <- type_correction(object, ans, type == "class")
  return(create.prediction(object, ans))
}

#' predict.glm.prmdt
#'
#' @description Return prediction for a \code{\link[stats]{glm}} model.
#'
#' @param object a \code{\link[stats]{glm}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param se.fit logical switch indicating if standard errors are required.
#' @param dispersion the dispersion of the GLM fit to be assumed in computing the standard errors. If omitted, that returned by summary applied to the object is used.
#' @param terms with type = "terms" by default all terms are returned. A character vector specifies which terms are to be returned.
#' @param na.action function determining what should be done with missing values in newdata. The default is to predict NA.
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom stats na.pass predict
#'
#' @return a vector or matrix of predictions for glm model.
#'
#' @export predict.glm.prmdt
#' @export
#'
predict.glm.prmdt <- function(object, newdata, type = "class", se.fit = FALSE, dispersion = NULL, terms = NULL, na.action = na.pass, ...){

  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), "response",  se.fit = se.fit, dispersion = dispersion, terms = terms, na.action = na.action, ... = ...)
  } else {
    ans <- predict(original_model(object), get_test_less_predict(newdata, object$prmdt$var.pred), "response",  se.fit = se.fit, dispersion = dispersion, terms = terms, na.action = na.action, ... = ...)
    levels.class <- object$prmdt$levels

    if(type == "prob"){
      ans <- matrix(as.numeric(ans), ncol = 1, byrow = TRUE)
      ans <- cbind(1 - ans, ans)
      colnames(ans) <- levels.class
    }else{
      ans <- ifelse(ans > 0.5, levels.class[2], levels.class[1])
      ans <- type_correction(object, ans, type == "class")
    }
  }

  return(create.prediction(object, ans))
}

#' predict.glmnet.prmdt
#'
#' @description Return prediction for a \code{\link[glmnet]{glmnet}} model.
#'
#' @param object a \code{\link[glmnet]{glmnet}} model object for which prediction is desired.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' @param type type of prediction 'prob' or 'class' (default).
#' @param s a \code{\link[glmnet]{cv.glmnet}} object (optional).
#' @param ... additional arguments affecting the predictions produced.
#'
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom stats model.matrix
#'
#' @export predict.glmnet.prmdt
#' @export
#'
predict.glmnet.prmdt <- function(object, newdata, type = "class", s = NULL,...){
  newdata <- get_test_less_predict(newdata, object$prmdt$var.pred)
  #Importante usar model.matrix, también Convierte a dummy
  testing <- model.matrix( ~., newdata)[,-1]
  if(is.null(s) && !is.null(object$prmdt$lambda.min)){
    s <- object$prmdt$lambda.min
  }

  if("prmdt.regression" %in% class(object)) {
    ans <- predict(original_model(object), testing, s = s, type = "class", ...)[, 1]
    ans <- type_correction(object, ans, type == "class")
  } else {
    if(type == "prob"){
      ans <- predict(original_model(object), testing, s = s, type = "response", ...)
    }
    else{
      ans <- predict(original_model(object), testing, s = s, type = type, ...)
    }

    if(!(is.null(object$prmdt$lambda.min) && is.null(s))){
      ans <- type_correction(object, ans, type == "class")
    }
  }

  return(create.prediction(object, ans))
}
