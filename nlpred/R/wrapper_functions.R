#' Wrapper for fitting a super learner based on \code{SuperLearner}.
#' 
#' Compatible learner wrappers for this package should have a specific format.
#' Namely they should take as input a list called \code{train} that contains
#' named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
#' and predictors in a particular training fold. Other options may be passed in
#' to the function as well. The function must output a list with the following
#' named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
#' fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
#' on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
#' necessary if you desire to look at this model later, not used for internal 
#' computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
#' of \code{test$Y}. 
#' 
#' This particular wrapper implements the \link[SuperLearner]{SuperLearner} ensemble
#' methodology. We refer readers to the original package's documentation for more
#' details. 
#' 
#' @param train A list with named objects \code{Y} and \code{X} (see description).
#' @param test A list with named objects \code{Y} and \code{X} (see description).
#' @param SL.library \code{SuperLearner} library. See \link[SuperLearner]{SuperLearner} 
#' for further description. 
#' @param ... Other options (passed to \code{SuperLearner}) 
#' @return A list with named objects (see description). 
#' @export
#' @importFrom SuperLearner SuperLearner 
#' @importFrom stats predict
#' @examples
#' # load super learner package
#' library(SuperLearner)
#' # simulate data
#' # make list of training data
#' train_X <- data.frame(x1 = runif(50))
#' train_Y <- rbinom(50, 1, plogis(train_X$x1))
#' train <- list(Y = train_Y, X = train_X)
#' # make list of test data
#' test_X <- data.frame(x1 = runif(50))
#' test_Y <- rbinom(50, 1, plogis(train_X$x1))
#' test <- list(Y = test_Y, X = test_X)
#' # fit super learner 
#' sl_wrap <- superlearner_wrapper(train = train, 
#'                                 test = test, 
#'                                 SL.library = c("SL.mean","SL.glm"))

superlearner_wrapper <- function(train, test,
                                 SL.library = c("SL.mean"), 
                                 ...){
    sl_fit <- SuperLearner::SuperLearner(Y = train$Y, 
                                         X = train$X, SL.library = SL.library,
                                         newX = rbind(test$X,train$X),
                                         family = stats::binomial(), verbose = FALSE,
                                         ...)
    all_pred <- sl_fit$SL.pred
    ntest <- length(test$Y)
    ntrain <- length(train$Y)
    test_pred <- all_pred[1:ntest]
    train_pred <- all_pred[(ntest+1):(ntest+ntrain)]
    return(list(test_pred = test_pred, train_pred = train_pred,
                model = sl_fit, train_y = train$Y, test_y = test$Y))
}

#' Wrapper for fitting a lasso using package \code{glmnet}.
#' 
#' Compatible learner wrappers for this package should have a specific format.
#' Namely they should take as input a list called \code{train} that contains
#' named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
#' and predictors in a particular training fold. Other options may be passed in
#' to the function as well. The function must output a list with the following
#' named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
#' fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
#' on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
#' necessary if you desire to look at this model later, not used for internal 
#' computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
#' of \code{test$Y}. 
#' 
#' This particular wrapper implements \link[glmnet]{glmnet}. We refer readers to the 
#' original package's documentation for more
#' details. 
#' 
#' @param train A list with named objects \code{Y} and \code{X} (see description).
#' @param test A list with named objects \code{Y} and \code{X} (see description).
#' @param alpha See \link[glmnet]{glmnet} for further description. 
#' @param nfolds See \link[glmnet]{glmnet} for further description.  
#' @param nlambda See \link[glmnet]{glmnet} for further description.  
#' @param use_min See \link[glmnet]{glmnet} for further description. 
#' @param loss See \link[glmnet]{glmnet} for further description. 
#' @param ... Other options (passed to \code{cv.glmnet}) 
#' @return A list with named objects (see description). 
#' @export
#' @importFrom stats model.matrix
#' @examples
#' # load super learner package
#' library(glmnet)
#' # simulate data
#' # make list of training data
#' train_X <- data.frame(x1 = runif(50), x2 = runif(50))
#' train_Y <- rbinom(50, 1, plogis(train_X$x1))
#' train <- list(Y = train_Y, X = train_X)
#' # make list of test data
#' test_X <- data.frame(x1 = runif(50), x2 = runif(50))
#' test_Y <- rbinom(50, 1, plogis(train_X$x1))
#' test <- list(Y = test_Y, X = test_X)
#' # fit super learner 
#' glmnet_wrap <- glmnet_wrapper(train = train, test = test)

glmnet_wrapper <- function(train, test,
                           alpha = 1, nfolds = 5, 
                           nlambda = 100, use_min = TRUE, 
                           loss = "deviance", ...){
  
  design_train_X <- model.matrix(~ -1 + ., train$X)
  design_test_X <- model.matrix(~ -1 + ., test$X)

  glmnet_fit <- glmnet::cv.glmnet(x = design_train_X, y = train$Y, 
        lambda = NULL, type.measure = loss, nfolds = nfolds, 
        family = "binomial", alpha = alpha, nlambda = nlambda, ...)

  test_pred <- predict(glmnet_fit, newx = design_test_X, type = "response", s = ifelse(use_min, 
      "lambda.min", "lambda.1se"))
  train_pred <- predict(glmnet_fit, newx = design_train_X, type = "response", s = ifelse(use_min, 
      "lambda.min", "lambda.1se"))

    return(list(test_pred = test_pred, train_pred = train_pred,
                model = glmnet_fit, train_y = train$Y, test_y = test$Y))
}

#' Wrapper for fitting a random forest using \link[randomForest]{randomForest}.
#' 
#' Compatible learner wrappers for this package should have a specific format.
#' Namely they should take as input a list called \code{train} that contains
#' named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
#' and predictors in a particular training fold. Other options may be passed in
#' to the function as well. The function must output a list with the following
#' named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
#' fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
#' on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
#' necessary if you desire to look at this model later, not used for internal 
#' computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
#' of \code{test$Y}. 
#' 
#' This particular wrapper implements the \link[randomForest]{randomForest} ensemble
#' methodology. We refer readers to the original package's documentation for more
#' details. 
#' 
#' @param train A list with named objects \code{Y} and \code{X} (see description).
#' @param test A list with named objects \code{Y} and \code{X} (see description).
#' @param mtry See \link[randomForest]{randomForest}.
#' @param ntree See \link[randomForest]{randomForest}.
#' @param nodesize See \link[randomForest]{randomForest}.
#' @param maxnodes See \link[randomForest]{randomForest}.
#' @param importance See \link[randomForest]{randomForest}.
#' @param ... Other options (passed to \code{randomForest}) 
#' @return A list with named objects (see description). 
#' @export
#' @importFrom stats predict
#' @examples
#' # simulate data
#' # make list of training data
#' train_X <- data.frame(x1 = runif(50))
#' train_Y <- rbinom(50, 1, plogis(train_X$x1))
#' train <- list(Y = train_Y, X = train_X)
#' # make list of test data
#' test_X <- data.frame(x1 = runif(50))
#' test_Y <- rbinom(50, 1, plogis(train_X$x1))
#' test <- list(Y = test_Y, X = test_X)
#' # fit randomforest 
#' rf_wrap <- randomforest_wrapper(train = train, test = test)

randomforest_wrapper <- function(train, test,
                                 mtry = floor(sqrt(ncol(train$X))), 
    ntree = 1000, nodesize = 1, maxnodes = NULL, importance = FALSE,...){
    rf_fit <- randomForest::randomForest(y = as.factor(train$Y), 
            x = train$X, ntree = ntree, xtest = rbind(test$X, train$X), 
            keep.forest = TRUE, mtry = mtry, nodesize = nodesize, 
            maxnodes = maxnodes, importance = importance, ...)
    all_psi <- rf_fit$test$votes[,2]
    ntest <- length(test$Y)
    ntrain <- length(train$Y)
    test_pred <- all_psi[1:ntest]
    train_pred <- all_psi[(ntest+1):(ntest+ntrain)]
    return(list(test_pred = test_pred, train_pred = train_pred,
                model = NULL, train_y = train$Y, test_y = test$Y))
}
#' Wrapper for fitting a random forest using \link[ranger]{ranger}.
#' 
#' Compatible learner wrappers for this package should have a specific format.
#' Namely they should take as input a list called \code{train} that contains
#' named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
#' and predictors in a particular training fold. Other options may be passed in
#' to the function as well. The function must output a list with the following
#' named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
#' fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
#' on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
#' necessary if you desire to look at this model later, not used for internal 
#' computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
#' of \code{test$Y}. 
#' 
#' This particular wrapper implements the \link[ranger]{ranger} ensemble
#' methodology. We refer readers to the original package's documentation for more
#' details. 
#' 
#' @param train A list with named objects \code{Y} and \code{X} (see description).
#' @param test A list with named objects \code{Y} and \code{X} (see description).
#' @param num.trees See \link[ranger]{ranger}.
#' @param mtry See \link[ranger]{ranger}.
#' @param write.forest See \link[ranger]{ranger}.
#' @param probability See \link[ranger]{ranger}.
#' @param min.node.size See \link[ranger]{ranger}.
#' @param replace See \link[ranger]{ranger}.
#' @param sample.fraction See \link[ranger]{ranger}.
#' @param num.threads See \link[ranger]{ranger}.
#' @param verbose See \link[ranger]{ranger}.
#' @param ... Other options (passed to \code{ranger}) 
#' @return A list with named objects (see description). 
#' @export
#' @importFrom stats predict
#' @examples
#' # simulate data
#' # make list of training data
#' train_X <- data.frame(x1 = runif(50))
#' train_Y <- rbinom(50, 1, plogis(train_X$x1))
#' train <- list(Y = train_Y, X = train_X)
#' # make list of test data
#' test_X <- data.frame(x1 = runif(50))
#' test_Y <- rbinom(50, 1, plogis(train_X$x1))
#' test <- list(Y = test_Y, X = test_X)
#' # fit ranger
#' rf_wrap <- ranger_wrapper(train = train, test = test)

ranger_wrapper <- function(train, test,
                           num.trees = 500, 
                           mtry = floor(sqrt(ncol(train$X))), 
                           write.forest = TRUE, probability = TRUE, 
                           min.node.size = 5, 
                           replace = TRUE, 
                           sample.fraction = ifelse(replace, 1, 0.632), 
                           num.threads = 1, verbose = TRUE, ...){

    fit <- ranger::ranger(myY ~ ., data = cbind(myY = factor(train$Y), train$X), 
        num.trees = num.trees, mtry = mtry, min.node.size = min.node.size, 
        replace = replace, sample.fraction = sample.fraction, 
        write.forest = write.forest, probability = probability, 
        num.threads = num.threads, 
        verbose = verbose, ...)
    pred_data <- rbind(test$X, train$X)
    all_psi <- predict(fit, data = pred_data)$predictions[, "1"]
    ntest <- length(test$Y)
    ntrain <- length(train$Y)
    test_pred <- all_psi[1:ntest]
    train_pred <- all_psi[(ntest+1):(ntest+ntrain)]
    return(list(test_pred = test_pred, train_pred = train_pred,
                model = NULL, train_y = train$Y, test_y = test$Y))
}

#' Wrapper for fitting a logistic regression using \code{glm}.
#' 
#' Compatible learner wrappers for this package should have a specific format.
#' Namely they should take as input a list called \code{train} that contains
#' named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
#' and predictors in a particular training fold. Other options may be passed in
#' to the function as well. The function must output a list with the following
#' named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
#' fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
#' on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
#' necessary if you desire to look at this model later, not used for internal 
#' computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
#' of \code{test$Y}. 
#' 
#' This particular wrapper implements a logistic regression using \link[stats]{glm}. 
#' We refer readers to the original package's documentation for more
#' details. 
#' 
#' @param train A list with named objects \code{Y} and \code{X} (see description).
#' @param test A list with named objects \code{Y} and \code{X} (see description).
#' @return A list with named objects (see description). 
#' @export
#' @importFrom stats glm 
#' @importFrom stats predict
#' @examples
#' # simulate data
#' # make list of training data
#' train_X <- data.frame(x1 = runif(50))
#' train_Y <- rbinom(50, 1, plogis(train_X$x1))
#' train <- list(Y = train_Y, X = train_X)
#' # make list of test data
#' test_X <- data.frame(x1 = runif(50))
#' test_Y <- rbinom(50, 1, plogis(train_X$x1))
#' test <- list(Y = test_Y, X = test_X)
#' # fit glm
#' glm_wrap <- glm_wrapper(train = train, test = test)

glm_wrapper <- function(train, test){
    if(!is.data.frame(train$X)){
      train$X <- data.frame(train$X)
    }
    if(!is.data.frame(test$X)){
      test$X <- data.frame(test$X)
    }
    glm_fit <- stats::glm(train$Y ~ ., data = train$X, family = stats::binomial())

    train_pred <- stats::predict(glm_fit, newdata = train$X, type = "response")
    test_pred <- stats::predict(glm_fit, newdata = test$X, type = "response")
    
    return(list(test_pred = test_pred, train_pred = train_pred,
                model = NULL, train_y = train$Y, test_y = test$Y))
}

#' Wrapper for fitting a forward stepwise logistic regression using \code{glm}.
#' 
#' Compatible learner wrappers for this package should have a specific format.
#' Namely they should take as input a list called \code{train} that contains
#' named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
#' and predictors in a particular training fold. Other options may be passed in
#' to the function as well. The function must output a list with the following
#' named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
#' fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
#' on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
#' necessary if you desire to look at this model later, not used for internal 
#' computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
#' of \code{test$Y}. 
#' 
#' This particular wrapper implements a forward stepwise logistic regression using 
#' \link[stats]{glm} and \link[stats]{step}. We refer readers to the original package's 
#' documentation for more details. 
#' 
#' @param train A list with named objects \code{Y} and \code{X} (see description).
#' @param test A list with named objects \code{Y} and \code{X} (see description).
#' @return A list with named objects (see description). 
#' @export
#' @importFrom stats glm predict step formula
#' @examples
#' # simulate data
#' # make list of training data
#' train_X <- data.frame(x1 = runif(50))
#' train_Y <- rbinom(50, 1, plogis(train_X$x1))
#' train <- list(Y = train_Y, X = train_X)
#' # make list of test data
#' test_X <- data.frame(x1 = runif(50))
#' test_Y <- rbinom(50, 1, plogis(train_X$x1))
#' test <- list(Y = test_Y, X = test_X)
#' # fit stepwise glm
#' step_wrap <- stepglm_wrapper(train = train, test = test)
stepglm_wrapper <- function(train, test){
    glm_full <- stats::glm(train$Y ~ ., data = train$X, family = stats::binomial())
    glm_fit <- stats::step(stats::glm(train$Y ~ 1, data = train$X, family = stats::binomial()), scope = stats::formula(glm_full), 
        direction = "forward", trace = 0, k = 2)
    Psi_nBn_0 <- function(x){
      stats::predict(glm_fit, newdata = x, type = "response")
    }
    train_pred <- Psi_nBn_0(train$X)
    test_pred <- Psi_nBn_0(test$X)
    return(list(test_pred = test_pred, train_pred = train_pred,
                model = glm_fit, train_y = train$Y, test_y = test$Y))
}


#' Wrapper for fitting eXtreme gradient boosting via \code{xgboost}
#' 
#' Compatible learner wrappers for this package should have a specific format.
#' Namely they should take as input a list called \code{train} that contains
#' named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
#' and predictors in a particular training fold. Other options may be passed in
#' to the function as well. The function must output a list with the following
#' named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
#' fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
#' on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
#' necessary if you desire to look at this model later, not used for internal 
#' computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
#' of \code{test$Y}. 
#' 
#' This particular wrapper implements eXtreme gradient boosting using 
#' \link[xgboost]{xgboost}. We refer readers to the original package's 
#' documentation for more details. 
#' 
#' @param train A list with named objects \code{Y} and \code{X} (see description).
#' @param test A list with named objects \code{Y} and \code{X} (see description).
#' @param ntrees See \link[xgboost]{xgboost}
#' @param max_depth See \link[xgboost]{xgboost}
#' @param shrinkage See \link[xgboost]{xgboost}
#' @param minobspernode See \link[xgboost]{xgboost}
#' @param params See \link[xgboost]{xgboost}
#' @param nthread See \link[xgboost]{xgboost}
#' @param verbose See \link[xgboost]{xgboost}
#' @param save_period See \link[xgboost]{xgboost}
#' @return A list with named objects (see description). 
#' @export
#' @examples
#' # simulate data
#' # make list of training data
#' train_X <- data.frame(x1 = runif(50))
#' train_Y <- rbinom(50, 1, plogis(train_X$x1))
#' train <- list(Y = train_Y, X = train_X)
#' # make list of test data
#' test_X <- data.frame(x1 = runif(50))
#' test_Y <- rbinom(50, 1, plogis(train_X$x1))
#' test <- list(Y = test_Y, X = test_X)
#' # fit xgboost
#' xgb_wrap <- xgboost_wrapper(train = train, test = test)
 
xgboost_wrapper <- function(test, train, ntrees = 500, 
    max_depth = 4, shrinkage = 0.1, minobspernode = 2, params = list(), 
    nthread = 1, verbose = 0, save_period = NULL){
    x <- stats::model.matrix(~. - 1, data = train$X)
    xgmat <- xgboost::xgb.DMatrix(data = x, label = train$Y)
    xgboost_fit <- xgboost::xgboost(data = xgmat, objective = "binary:logistic", 
            nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
            eta = shrinkage, verbose = verbose, nthread = nthread, 
            params = params, save_period = save_period)
    newx <- model.matrix(~. - 1, data = test$X)

    test_pred <- predict(xgboost_fit, newdata = newx)
    train_pred <- predict(xgboost_fit, newdata = x)

    return(list(test_pred = test_pred, train_pred = train_pred,
                model = xgboost_fit, train_y = train$Y, test_y = test$Y))
}