visa.mix <- function(x, y, factorID = NULL, weight, family = c('gaussian', 'binomial'))
{
  family <- match.arg(family)
  # Check the data
  if (family == "binomial") {
    if (!all(y %in% c(0, 1)))
      stop("There can only be 0 or 1 in y when using binomial family")
  }
  if(all(is.na(x)))
  {
    stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation)
         to eliminate missing data before passing X and y to calculate.")
  }

  else if(is.numeric(x)==TRUE & is.null(factorID)==TRUE){
    n <- length(y)
    p <- ncol(x)
    ########################### family = binomial
    if (family == 'binomial'){

      #####
      svm <- e1071::svm(y = as.factor(y), x = x,  fitted = FALSE, probability = TRUE)
      pi_svm <- attr(predict(svm, newdata = x, probability = TRUE), "prob")[, "1"]

      #####
      sGboost <- mboost::glmboost(y = as.factor(y), x = x, center = F, family = Binomial(link = "logit"))
      pi_Gboost <- predict(sGboost, newdata = x, type = 'response')

      #####
      data_rf <- data.frame(y=as.factor(y),x)
      srandomf <- randomForest::randomForest(y ~ ., data=data_rf)
      pi_rf <- predict(srandomf,newdata=data.frame(x),type='prob')[,2]

      #####
      sksvm <- kernlab::ksvm(x, as.factor(y), scaled = FALSE, kernel = 'rbfdot', prob.model=TRUE)
      pi_ksvm <- predict(sksvm,newdata=data.frame(x),type='probabilities')[,2]

      #####
      xgmat <- xgboost::xgb.DMatrix(data = x, label = y)
      skxgb <- xgboost::xgboost(data = xgmat, objective="binary:logistic", nrounds = 10, verbose = 0,  eval_metric = "logloss")
      pi_xgb <- predict(skxgb, xgmat)

      est_mix <- cbind(pi_svm, pi_Gboost, pi_rf, pi_ksvm, pi_xgb)
      colnames(est_mix) <-  c("SVM", "L2B", "RF", "kSVM", "Xgboost")
      pi_mix <- est_mix %*% weight
    }

    ########################### family = gaussian
    if (family == 'gaussian'){

      svm <- e1071::svm(y = y, x = x,  fitted = FALSE)
      hat_svm <- predict(svm, newdata = x)

      #####
      sGboost <- mboost::glmboost(y=as.numeric(y), x = x, center=F)
      hat_Gboost <- predict(sGboost, newdata = x)

      #####
      data_rf <- data.frame(y,x)
      srandomf <- randomForest::randomForest(y ~ ., data=data_rf)
      hat_rf <- predict(srandomf,newdata=data.frame(x))

      #####
      sksvm <- kernlab::ksvm(x, y, scaled = FALSE, kernel = 'rbfdot')
      hat_ksvm <- predict(sksvm,newdata=data.frame(x))

      #####
      xgmat <-xgboost::xgb.DMatrix(data = x, label = y)
      skxgb <- xgboost::xgboost(data = xgmat, nrounds = 10, verbose = 0)
      hat_xgb <- predict(skxgb, xgmat)

      est_mix <- cbind(hat_svm, hat_Gboost, hat_rf, hat_ksvm, hat_xgb)
      colnames(est_mix) <- c("SVM", "L2B", "RF", "kSVM", "Xgboost")
      h_mix <- est_mix %*% weight
    }
  }
  if(family == 'gaussian') object <- h_mix
  if(family == 'binomial') object <- pi_mix
  return(object)
}
