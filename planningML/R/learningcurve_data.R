#'
#' Generate descriptive summary for objects returned by functions in EHRsampling
#'
#' Generate descriptive summary for objects returned by functions in EHRsampling.
#'
#'
#' @param x a matrix of predictor variables
#' @param y a vector of binary outcome, encoded as a factor and denoted by 1 for events and 0 for non-events
#' @param method training method to get performance measurements. Available options are "log" (logistic regression, default),
#'               "regul.log" (regularized logistic regression), "svm" (support vector machine), "rf" (random forest) and "lda" (linear discriminant analysis)
#' @param metric default = "MCC". The target performance estimation metric that you want to optimize. Other choice
#'               can be "AUC".
#' @param pct.train the percentage of data that goes to training. Default is 0.8
#' @param batchsize sample size for each training batch
#' @param class.prob probability of the event
#' @param nfold number of folds in cross validation
#' @param nrepeat number of repeats for cross validation
#'
#' @import caret
#' @importFrom pROC roc
#' @importFrom dplyr sample_n mutate %>%
#' @importFrom stats binomial na.exclude predict
#' @return \code{learningcurve_data()} returns a data frame of sample size and the corresponding performance measurements.
#'
#'
#' @export

learningcurve_data = function(x, y, method="log", metric="MCC", batchsize = 60, class.prob,
                              pct.train=0.8, nfold=5, nrepeat=10){

  ## Train the model and get AUC measurements


  dt_full <- data.frame(x, y = as.factor(y))

  ## Get common testing dataset:
  train_index = createDataPartition(dt_full$y, p = pct.train, list = FALSE)
  train_df <- dt_full[train_index,]
  test_df <- dt_full[-train_index,]

  test_df <- test_df %>%
    mutate(y = as.factor(ifelse(y == 0,"No","Yes")))

  ssize = c()
  eval = c()

  for (i in 1:floor(nrow(train_df)/batchsize)){

    n = batchsize * i

    # set.seed(nseed)
    MySummary  <- function(data, lev = levels(dt_full$y), model = NULL){
      a1 <- defaultSummary(data, lev, model)
      b1 <- twoClassSummary(data, lev, model)
      c1 <- prSummary(data, lev, model)
      d1 <- multiClassSummary(data, lev, model)
      out <- c(a1, b1, c1, d1)

      return(out)
    }

    nfold_orig = nfold


    res = c()
    for (j in 1:nrepeat){

      # trace back the original nfold
      nfold <- nfold_orig

      # get training data
      train_index1 = createDataPartition(train_df$y, p = n/nrow(train_df), list = FALSE)
      train_df1 <- dt_full[train_index1,]
      # test_df <- dt_full[-train_index,]

      train_df1 <- train_df1 %>%
        mutate(y = as.factor(ifelse(y == 0,"No","Yes")))
      # test_df <- test_df %>%
      #   mutate(y = as.factor(ifelse(y == 0,"No","Yes")))

      if (length(which(train_df1$y == "Yes")) < 2 * nfold) {
        nfold = floor(length(which(train_df1$y == "Yes"))/2)
        warningtext = paste("Each cross validation fold should contain at least 2 events, so the number of fold is modified to", nfold,
                            "when sample size equals", n)
        warning(warningtext)
      }

      # cvIndex <- createFolds(factor(train_df$y), nfold, returnTrain = F)

      cvmethod="cv"

      control <- trainControl(search = "random",
                              method = cvmethod, number = nfold, classProbs = TRUE,
                              summaryFunction = MySummary,allowParallel = FALSE)

      if (method == "log"){
        error<- try( fit.log <- train(y ~ ., data=train_df1, method="glm", trControl=control,family=binomial(),na.action=na.exclude) )
        cartClasses <- predict(fit.log, newdata = test_df)
        cartConfusion = confusionMatrix(data = cartClasses, test_df$y)
        cart.ROC <- roc(predictor = as.numeric(cartClasses), response = as.numeric(test_df$y))
        # res = c(res, as.numeric(cart.ROC$auc))

        if (metric == "MCC"){
          PPV = cartConfusion$byClass["Pos Pred Value"]
          NPV = cartConfusion$byClass["Neg Pred Value"]
          TPR = cartConfusion$byClass["Sensitivity"]
          TNR = cartConfusion$byClass["Specificity"]
          MCC.n = sqrt(TPR*TNR*PPV*NPV) - sqrt((1-TPR)*(1-TNR)*(1-PPV)*(1-NPV))
          names(MCC.n) = "MCC"
          res = c(res, MCC.n)
        }
        if (metric == "AUC"){
          res = c(res, as.numeric(cart.ROC$auc))
        }
      }

      if (method == "regul.log"){
        error<- try( fit.log <- train(y ~ ., data=train_df1, method="glmnet", trControl=control,metric="ROC",family="binomial") )
        cartClasses <- predict(fit.log, newdata = test_df)
        cartConfusion = confusionMatrix(data = cartClasses, test_df$y)
        cart.ROC <- roc(predictor = as.numeric(cartClasses), response = as.numeric(test_df$y))
        # res = c(res, as.numeric(cart.ROC$auc))

        if (metric == "MCC"){
          PPV = cartConfusion$byClass["Pos Pred Value"]
          NPV = cartConfusion$byClass["Neg Pred Value"]
          TPR = cartConfusion$byClass["Sensitivity"]
          TNR = cartConfusion$byClass["Specificity"]
          MCC.n = sqrt(TPR*TNR*PPV*NPV) - sqrt((1-TPR)*(1-TNR)*(1-PPV)*(1-NPV))
          names(MCC.n) = "MCC"
          res = c(res, MCC.n)
        }
        if (metric == "AUC"){
          res = c(res, as.numeric(cart.ROC$auc))
        }
      }

      if (method == "svm"){
        error<- try(fit.svm <- train(y ~ ., data = train_df1,
                                     method = "svmRadial", trControl = control, metric = "ROC"))
        cartClasses <- predict(fit.svm, newdata = test_df)
        cartConfusion = confusionMatrix(data = cartClasses, test_df$y)
        cart.ROC <- roc(predictor = as.numeric(cartClasses), response = as.numeric(test_df$y))
        if (metric == "MCC"){
          PPV = cartConfusion$byClass["Pos Pred Value"]
          NPV = cartConfusion$byClass["Neg Pred Value"]
          TPR = cartConfusion$byClass["Sensitivity"]
          TNR = cartConfusion$byClass["Specificity"]
          MCC.n = sqrt(TPR*TNR*PPV*NPV) - sqrt((1-TPR)*(1-TNR)*(1-PPV)*(1-NPV))
          names(MCC.n) = "MCC"
          res = c(res, MCC.n)
        }
        if (metric == "AUC"){
          res = c(res, as.numeric(cart.ROC$auc))
        }
      }

      if (method == "rf"){
        error<- try(fit.rf <- train(y ~., data=train_df1, method = 'rf', trControl=control, metric = "ROC"))
        cartClasses <- predict(fit.rf, newdata = test_df)
        cartConfusion = confusionMatrix(data = cartClasses, test_df$y)
        cart.ROC <- roc(predictor = as.numeric(cartClasses), response = as.numeric(test_df$y))
        if (metric == "MCC"){
          PPV = cartConfusion$byClass["Pos Pred Value"]
          NPV = cartConfusion$byClass["Neg Pred Value"]
          TPR = cartConfusion$byClass["Sensitivity"]
          TNR = cartConfusion$byClass["Specificity"]
          MCC.n = sqrt(TPR*TNR*PPV*NPV) - sqrt((1-TPR)*(1-TNR)*(1-PPV)*(1-NPV))
          names(MCC.n) = "MCC"
          res = c(res, MCC.n)
        }
        if (metric == "AUC"){
          res = c(res, as.numeric(cart.ROC$auc))
        }
      }

      if (method == "lda"){
        error<- try(fit.lda <- train(y ~., data=train_df1, method = 'lda', trControl=control, metric = "ROC"))
        cartClasses <- predict(fit.lda, newdata = test_df)
        cartConfusion = confusionMatrix(data = cartClasses, test_df$y)
        cart.ROC <- roc(predictor = as.numeric(cartClasses), response = as.numeric(test_df$y))
        if (metric == "MCC"){
          PPV = cartConfusion$byClass["Pos Pred Value"]
          NPV = cartConfusion$byClass["Neg Pred Value"]
          TPR = cartConfusion$byClass["Sensitivity"]
          TNR = cartConfusion$byClass["Specificity"]
          MCC.n = sqrt(TPR*TNR*PPV*NPV) - sqrt((1-TPR)*(1-TNR)*(1-PPV)*(1-NPV))
          names(MCC.n) = "MCC"
          res = c(res, MCC.n)
        }
        if (metric == "AUC"){
          res = c(res, as.numeric(cart.ROC$auc))
        }
      }

    }

    eval = c(eval, mean(res))
    # auc = c(auc, (res))
    ssize = c(ssize, n)

  }

  lcurvedt <- data.frame(ssize = ssize,
                         eval = eval)
  colnames(lcurvedt) = c("Sample size", metric)

  lcurvedt

}
