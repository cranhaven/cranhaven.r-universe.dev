###weight provide the calculation of model averging methods for propensity score and baseline functions.
visa.weight <- function(x, y, factorID = NULL, subset = NULL, family = c('gaussian', 'binomial'), n_train = ceiling(n/2),
                        no_rep = 20, p0 = 0.5, psi = 1, prior = TRUE) {

  family <- match.arg(family)
  # Check the data
  if (family == "binomial") {
    if (!all(y %in% c(0, 1)))
      stop("There can only be 0 or 1 in y when using binomial family")
  }
  if(is.null(subset)==FALSE)
  {
    y <- y[subset]
    x <- x[subset, ]
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
      wt_calc=function(rep_id) {
        lw_num <- matrix(0,nrow=1, 5)
        tindex <- sample(n,n_train, replace = FALSE)
        x1 <- x[tindex,];x2=x[-tindex,]
        y1 <- y[tindex];y2=y[-tindex]

        svm <- e1071::svm(y = as.factor(y1), x = x1,  fitted = FALSE, probability = TRUE)
        pi0 <- attr(predict(svm, newdata = x1, probability = TRUE), "prob")[, "1"]
        spre_svm0 <- log(pi0/(1-pi0))
        pi1 <- attr(predict(svm, newdata = x2, probability = TRUE), "prob")[, "1"]
        spre_svm1 <- log(pi1/(1-pi1))
        dk <- as.vector(spre_svm1)
        fk <- ifelse(dk < 0, log(1 + exp(dk)), dk + log(1 + exp(-dk)))
        lw_num[1] <- sum(y2 * dk) - sum(fk)

        #####
        sGboost <- mboost::glmboost(y=as.factor(y1),x=x1,center=F,family=Binomial(link = "logit"))
        spre_Gboost0 <- predict(sGboost,newdata=x1)*2
        spre_Gboost1 <- predict(sGboost,newdata=x2)*2
        dk <- as.vector(spre_Gboost1)
        fk <- ifelse(dk < 0, log(1 + exp(dk)), dk + log(1 + exp(-dk)))
        lw_num[2] <- sum(y2 * dk) - sum(fk)

        #####
        data_rf <- data.frame(y1=as.factor(y1),x1)
        srandomf <- randomForest::randomForest(y1 ~ ., data=data_rf)
        pi0 <- predict(srandomf,newdata=data.frame(x1),type='prob')[,2]
        spre_rf0 <- log(pi0/(1-pi0))
        pi1 <- predict(srandomf,newdata=data.frame(x2),type='prob')[,2]
        spre_rf1 <- log(pi1/(1-pi1))
        dk <- as.vector(spre_rf1)
        fk <- ifelse(dk < 0, log(1 + exp(dk)), dk + log(1 + exp(-dk)))
        lw_num[3] <- sum(y2 * dk) - sum(fk)

        #####
        sksvm <- kernlab::ksvm(x1, as.factor(y1), scaled = FALSE, kernel = 'rbfdot', prob.model=TRUE)
        pi0=predict(sksvm,newdata=data.frame(x1),type='probabilities')[,2]
        spre_ksvm0 <- log(pi0/(1-pi0))
        pi1=predict(sksvm,newdata=data.frame(x2),type='probabilities')[,2]
        spre_ksvm1 <- log(pi1/(1-pi1))
        dk <- as.vector(spre_ksvm1)
        fk <- ifelse(dk < 0, log(1 + exp(dk)), dk + log(1 + exp(-dk)))
        lw_num[4] <- sum(y2 * dk) - sum(fk)

        #####
        xgmat1 <- xgboost::xgb.DMatrix(data = x1, label = y1)
        xgmat2 <- xgboost::xgb.DMatrix(data = x2)
        skxgb <- xgboost::xgboost(data = xgmat1, objective="binary:logistic", nrounds = 10, verbose = 0,  eval_metric = "logloss")
        pi0 <- predict(skxgb, xgmat1)
        spre_xgb0 <- log(pi0/(1-pi0))
        pi1 <- predict(skxgb, xgmat2)
        spre_xgb1 <- log(pi1/(1-pi1))
        dk <- as.vector(spre_xgb1)
        fk <- ifelse(dk < 0, log(1 + exp(dk)), dk + log(1 + exp(-dk)))
        lw_num[5] <- sum(y2 * dk) - sum(fk)

        return(lw_num)
      }

      lw_num=matrix(unlist(parallel::mclapply(seq(no_rep), wt_calc)), nrow = no_rep, ncol = 5, byrow = TRUE)
      if (prior == TRUE) {
        ck0=-log(1-p0)+log(5)
        #ck1=-log(p0)+ck_compute(n_mo, sk, p)
        ck=c(rep(ck0,5))
        lw_num=sweep(lw_num, MARGIN = 2, psi*ck, "-")
      }
      lw_num=sweep(lw_num, MARGIN = 1, apply(lw_num, 1, max), "-")
      w_num=exp(lw_num)
      weight=colMeans(w_num/rowSums(w_num))
      weight_se=apply(w_num,2,sd)/sqrt(no_rep)
    }


    ########################### family = gaussian
    if (family == 'gaussian'){
      wt_calc=function(rep_id) {
        lw_num <- matrix(0, nrow=1, 5)
        tindex <- sample(n, n_train, replace = FALSE)
        x1=x[tindex,];x2=x[-tindex,]
        y1=y[tindex];y2=y[-tindex]

        svm <- e1071::svm(y = y1, x = x1,  fitted = FALSE)
        spre_svm0 <- predict(svm, newdata = x1)
        spre_svm1 <- predict(svm, newdata = x2)
        sigmak <- sqrt(sum((y1-spre_svm0)^2)/n_train)
        dk <- sum((y2-spre_svm1)^2)
        lw_num[1] <- (-(n-n_train))*log(sigmak)-((sigmak)^(-2))*dk/2

        #####
        sGboost <- mboost::glmboost(y=as.numeric(y1),x=x1,center=F)
        spre_Gboost0 <- predict(sGboost,newdata=x1)
        spre_Gboost1 <- predict(sGboost,newdata=x2)
        sigmak <- sqrt(sum((y1-spre_Gboost0)^2)/n_train)
        dk <- sum((y2-spre_Gboost1)^2)
        lw_num[2] <- (-(n-n_train))*log(sigmak) - ((sigmak)^(-2))*dk/2

        #####
        data_rf <- data.frame(y1,x1)
        srandomf <- randomForest::randomForest(y1 ~ ., data=data_rf)
        spre_rf0 <- predict(srandomf,newdata=data.frame(x1))
        spre_rf1 <- predict(srandomf,newdata=data.frame(x2))
        sigmak <- sqrt(sum((y1-spre_rf0)^2)/n_train)
        dk <- sum((y2-spre_rf1)^2)
        lw_num[3] <- (-(n-n_train))*log(sigmak)-((sigmak)^(-2))*dk/2

        #####
        sksvm <- kernlab::ksvm(x1, y1, scaled = FALSE, kernel = 'rbfdot')
        spre_ksvm0 <- predict(sksvm,newdata=data.frame(x1))
        spre_ksvm1 <- predict(sksvm,newdata=data.frame(x2))
        sigmak <- sqrt(sum((y1-spre_ksvm0)^2)/n_train)
        dk <- sum((y2-spre_ksvm1)^2)
        lw_num[4] <- (-(n-n_train))*log(sigmak)-((sigmak)^(-2))*dk/2

        #####
        xgmat1 <- xgboost::xgb.DMatrix(data = x1, label = y1)
        xgmat2 <- xgboost::xgb.DMatrix(data = x2)
        skxgb <- xgboost::xgboost(data = xgmat1, nrounds = 10, verbose = 0)
        spre_xgb0 <- predict(skxgb, xgmat1)
        spre_xgb1 <- predict(skxgb, xgmat2)
        sigmak <- sqrt(sum((y1-spre_xgb0)^2)/n_train)
        dk <- sum((y2-spre_xgb1)^2)
        lw_num[5] <- (-(n-n_train))*log(sigmak)-((sigmak)^(-2))*dk/2

        return(lw_num)
      }

      lw_num <- matrix(unlist(lapply(seq(no_rep), wt_calc)), nrow = no_rep, ncol = 5, byrow = TRUE)
      if (prior == TRUE) {
        ck0 <- -log(1-p0)+log(5)
        #ck1=-log(p0)+ck_compute(n_mo, sk, p)
        ck <- c(rep(ck0,5))
        lw_num <- sweep(lw_num, MARGIN = 2, psi*ck, "-")
      }
      lw_num <- sweep(lw_num, MARGIN = 1, apply(lw_num, 1, max), "-")
      w_num <- exp(lw_num)
      weight <- colMeans(w_num/rowSums(w_num))
      weight_se <- apply(w_num,2,sd)/sqrt(no_rep)
    }
  }

  names(weight) <- c("SVM", "L2B", "RF", "kSVM", "Xgboost")
  object <- list(weight = weight, weight_se = weight_se)
  return(object)
}
