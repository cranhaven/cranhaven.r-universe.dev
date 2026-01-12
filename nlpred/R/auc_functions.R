utils::globalVariables(c(":="))

#' Estimates of CVAUC
#' 
#' This function computes K-fold cross-validated estimates of the area under
#' the receiver operating characteristics (ROC) curve (hereafter, AUC). This
#' quantity can be interpreted as the probability that a randomly selected 
#' case will have higher predicted risk than a randomly selected control. 
#' 
#' To estimate the AUC of a particular prediction algorithm, K-fold cross-validation
#' is commonly used: data are partitioned into K distinct groups and the
#' prediction algorithm is developed using K-1 of these groups. In standard K-fold
#' cross-validation, the AUC of this prediction algorithm is estimated using
#' the remaining fold. This can be problematic when the number of observations is 
#' small or the number of cross-validation folds is large. 
#' 
#' Here, we estimate relevant nuisance parameters in the training sample and use
#' the validation sample to perform some form of bias correction -- either through
#' cross-validated targeted minimum loss-based estimation, estimating equations, 
#' or one-step estimation. When aggressive learning algorithms are applied, it is
#' necessary to use an additional layer of cross-validation in the training sample
#' to estimate the nuisance parameters. This is controlled via the \code{nested_cv}
#' option below. 
#' 
#' 
#' @param Y A numeric vector of outcomes, assume to equal \code{0} or \code{1}.
#' @param X A \code{data.frame} or \code{matrix} of variables for prediction.
#' @param K The number of cross-validation folds (default is \code{10}).
#' @param learner A wrapper that implements the desired method for building a 
#' prediction algorithm. See See \code{?glm_wrapper} or read the package vignette
#' for more information on formatting \code{learner}s.
#' @param nested_cv A boolean indicating whether nested cross validation should
#' be used to estimate the distribution of the prediction function. Default (\code{TRUE})
#' is best choice for aggressive \code{learner}'s, while \code{FALSE} is reasonable
#' for smooth \code{learner}'s (e.g., logistic regression).
#' @param nested_K If nested cross validation is used, how many inner folds should 
#' there be? Default (\code{K-1}) affords quicker computation by reusing training
#' fold learner fits. 
#' @param parallel A boolean indicating whether prediction algorithms should be 
#' trained in parallel. Default to \code{FALSE}. 
#' @param max_cvtmle_iter Maximum number of iterations for the bias correction
#' step of the CV-TMLE estimator (default \code{10}). 
#' @param cvtmle_ictol The CV-TMLE will iterate \code{max_cvtmle_iter} is reached 
#' or mean of cross-validated efficient influence function is less than 
#' \code{cvtmle_ictol}. 
#' @param prediction_list For power users: a list of predictions made by \code{learner}
#' that has a format compatible with \code{cvauc}.
#' @param ... Other arguments, not currently used
#' @importFrom SuperLearner CVFolds
#' @importFrom cvAUC ci.cvAUC
#' @importFrom stats uniroot
#' @importFrom Rdpack reprompt
#' @importFrom assertthat assert_that
#' @export
#' @return An object of class \code{"cvauc"}. \describe{
#' \item{\code{est_cvtmle}}{cross-validated targeted minimum loss-based estimator of K-fold CV AUC}
#' \item{\code{iter_cvtmle}}{iterations needed to achieve convergence of CVTMLE algorithm}
#' \item{\code{cvtmle_trace}}{the value of the CVTMLE at each iteration of the targeting algorithm}
#' \item{\code{se_cvtmle}}{estimated standard error based on targeted nuisance parameters}
#' \item{\code{est_init}}{plug-in estimate of CV AUC where nuisance parameters are estimated
#' in the training sample}
#' \item{\code{est_empirical}}{the standard K-fold CV AUC estimator}
#' \item{\code{se_empirical}}{estimated standard error for the standard estimator}
#' \item{\code{est_onestep}}{cross-validated one-step estimate of K-fold CV AUC}
#' \item{\code{se_onestep}}{estimated standard error for the one-step estimator}
#' \item{\code{est_esteq}}{cross-validated estimating equations estimate of K-fold CV AUC}
#' \item{\code{se_esteq}}{estimated standard error for the estimating equations estimator 
#' (same as for one-step)}
#' \item{\code{folds}}{list of observation indexes in each validation fold}
#' \item{\code{ic_cvtmle}}{influence function evaluated at the targeted nuisance parameter
#' estimates}
#' \item{\code{ic_onestep}}{influence function evaluated at the training-fold-estimated
#' nuisance parameters}
#' \item{\code{ic_esteq}}{influence function evaluated at the training-fold-estimated 
#' nuisance parameters}
#' \item{\code{ic_empirical}}{influence function evaluated at the validation-fold 
#' estimated nuisance parameters}
#' \item{\code{prediction_list}}{a list of output from the cross-validated model training; 
#' see the individual wrapper function documentation for further details}
#' }
#' 
#' @examples
#' # simulate data
#' n <- 200
#' p <- 10
#' X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
#' Y <- rbinom(n, 1, plogis(X[,1] + X[,10]))
#' 
#' # get cv auc estimates for logistic regression
#' cv_auc_ests <- cv_auc(Y = Y, X = X, K = 5, learner = "glm_wrapper")
#' 
#' # get cv auc estimates for random forest
#' # using nested cross-validation for nuisance parameter estimation
#' \donttest{
#' fit <- cv_auc(Y = Y, X = X, K = 5, 
#'               learner = "randomforest_wrapper", 
#'               nested_cv = TRUE)
#' }

cv_auc <- function(Y, X, K = 10, learner = "glm_wrapper", 
                  nested_cv = TRUE,
                  nested_K = K - 1,
                  parallel = FALSE, 
                  max_cvtmle_iter = 10, 
                  cvtmle_ictol = 1 / length(Y), 
                  prediction_list = NULL,
                  ...){
  # test inputs
  assertthat::assert_that(all(Y %in% c(0,1)))
  if(!nested_cv){
    assertthat::assert_that(K > 1)
  }else{
    assertthat::assert_that(K > 2)
    assertthat::assert_that(nested_K > 1)
  }

  # sample size
  n <- length(Y)
  # make outer cross-validation folds
  folds <- SuperLearner::CVFolds(
            N = n, id = NULL, Y = Y, cvControl = list(
              V = K, stratifyCV = ifelse(K <= sum(Y) & K <= sum(!Y), TRUE, FALSE), 
              shuffle = TRUE, validRows = NULL
            )
          )
  # train learners in all necessary combination of folds
  if(is.null(prediction_list)){
    prediction_list <- .get_predictions(
      learner = learner, Y = Y, X = X, K = K, nested_K = nested_K, 
      folds = folds, parallel = FALSE, nested_cv = nested_cv
    )
  }

  # make long data for targeting step of cvtmle
  if(!nested_cv){
    long_data_list <- lapply(prediction_list, .make_long_data, gn = mean(Y))
  }else{
    long_data_list <- sapply(1:K, .make_long_data_nested_cv, gn = mean(Y), 
                             prediction_list = prediction_list, folds = folds,
                             simplify = FALSE)    
  }
  
  # targeting step for cvtmle
  epsilon_0 <- rep(0, max_cvtmle_iter)
  epsilon_1 <- rep(0, max_cvtmle_iter)
  iter <- 0
  update_long_data_list <- long_data_list
  # combine list into data frame
  full_long_data <- Reduce(rbind, update_long_data_list)
  # compute mean of EIF
  D1 <- .Dy(full_long_data, y = 1)
  D0 <- .Dy(full_long_data, y = 0)
  ic <- ic_os <- D1 + D0 
  PnDstar <- mean(ic)

  # compute initial estimate of training fold-specific learner distribution
  if(!nested_cv){
    dist_psix_y0_star <- lapply(prediction_list, .get_psi_distribution, 
                           y = 0, epsilon = epsilon_0)
    dist_psix_y1_star <- lapply(prediction_list, .get_psi_distribution, y = 1,
                           epsilon = epsilon_1)
  }else{
    dist_psix_y0_star <- sapply(1:K, .get_psi_distribution_nested_cv, 
                           y = 0, epsilon = epsilon_0, folds = folds, 
                           prediction_list = prediction_list, simplify = FALSE)
    dist_psix_y1_star <- sapply(1:K, .get_psi_distribution_nested_cv, 
                           y = 1, epsilon = epsilon_1, folds = folds, 
                           prediction_list = prediction_list, simplify = FALSE)
  }
  # get initial estimate of AUC
  init_auc <- mean(mapply(FUN = .get_auc, dist_y0 = dist_psix_y0_star, 
                     dist_y1 = dist_psix_y1_star))
  # hard code in for case that psi is constant and doesn't depend on Y
  if(identical(dist_psix_y0_star, dist_psix_y1_star)){
    est_onestep <- init_auc
    tmle_auc <- init_auc
    est_esteq <- init_auc
  }else{
    # CV one-step estimator
    est_onestep <- init_auc + PnDstar
    se_onestep <- sqrt(var(ic_os)/n)

    # CV estimating equations estimator
    if(!nested_cv){
      est_esteq <- tryCatch({
        stats::uniroot(.estim_fn, interval = c(0, 1), 
                       prediction_list = prediction_list, 
                       gn = mean(Y))$root
      }, error = function(e){ return(NA) })
      se_esteq <- se_onestep
    }else{
      est_esteq <- tryCatch({
        stats::uniroot(.estim_fn_nested_cv, interval = c(0, 1), 
                       prediction_list = prediction_list, gn = mean(Y), 
                       folds = folds, K = K)$root
      }, error = function(e){ return(NA) })
      se_esteq <- se_onestep
    }

    tmle_auc <- rep(NA, max_cvtmle_iter)
    PnDstar <- Inf
    # targeting steps for CVTMLE
    # iterate until max_cvtmle_iter reached of mean of IC < cvtmle_ictol
    while(abs(PnDstar) > cvtmle_ictol & iter < max_cvtmle_iter){
      iter <- iter + 1
      # make weight for loss function
      full_long_data$targeting_weight_0 <- 
        as.numeric(full_long_data$Y == 0)/(full_long_data$gn) * full_long_data$dFn 
      # fit intercept only model with weights
      suppressWarnings(
        fluc_mod_0 <- glm(outcome ~ offset(logit_Fn), family = stats::binomial(),
                          data = full_long_data[full_long_data$Yi == 0,], 
                          weights = full_long_data$targeting_weight_0[full_long_data$Yi == 0],
                          start = 0)
      )
      epsilon_0[iter] <- as.numeric(fluc_mod_0$coef[1])
      # if unstable glm fit (as evidenced by coefficient > 100) redo using grid search
      coef_tol <- 1e2
      if(abs(fluc_mod_0$coef) > coef_tol){
        # try a grid search
        eps_seq <- seq(-coef_tol, coef_tol, length = 10000)
        llik0 <- sapply(eps_seq, fluc_mod_optim_0, 
                        fld = full_long_data[full_long_data$Yi == 0,])
        idx_min <- which.min(llik0)
        epsilon_0[iter] <- eps_seq[idx_min]      
      }
      # update values in long_data_list
      if(!nested_cv){
        update_long_data_list <- lapply(prediction_list, .make_long_data, gn = mean(Y),
                                 epsilon_0 = epsilon_0, epsilon_1 = epsilon_1,
                                 update = TRUE)
      }else{
        update_long_data_list <- sapply(1:K, .make_long_data_nested_cv, gn = mean(Y),
                                 epsilon_0 = epsilon_0, epsilon_1 = epsilon_1,
                                 prediction_list = prediction_list, folds = folds, 
                                 update = TRUE, simplify = FALSE)
      }
      # update full long data
      full_long_data <- Reduce(rbind, update_long_data_list)
      
      # useful sanity check
      # D0_tmp <- c(.Dy(full_long_data, y = 0))
      # mean(D0_tmp)

      # make weight for loss function
      full_long_data$targeting_weight_1 <- 
        as.numeric(full_long_data$Y == 1)/(full_long_data$gn) * full_long_data$dFn 
      # fit intercept only model with weights
      suppressWarnings(
        fluc_mod_1 <- glm(outcome ~ offset(logit_Fn), family = stats::binomial(),
                          data = full_long_data[full_long_data$Yi == 1,], 
                          weights = full_long_data$targeting_weight_1[full_long_data$Yi == 1],
                          start = 0)
      )
      # update values in long_data_list
      epsilon_1[iter] <- as.numeric(fluc_mod_1$coef[1])
      if(abs(fluc_mod_1$coef) > coef_tol){
        eps_seq <- seq(-coef_tol, coef_tol, length = 1000)
        llik1 <- sapply(eps_seq, fluc_mod_optim_1, 
                        fld = full_long_data[full_long_data$Yi == 1,])
        idx_min <- which.min(llik1)
        epsilon_1[iter] <- eps_seq[idx_min] 
      }
      if(!nested_cv){
        update_long_data_list <- lapply(prediction_list, .make_long_data, gn = mean(Y),
                                 epsilon_0 = epsilon_0, epsilon_1 = epsilon_1,
                                 update = TRUE)
      }else{
        update_long_data_list <- sapply(1:K, .make_long_data_nested_cv, gn = mean(Y),
                                 epsilon_0 = epsilon_0, epsilon_1 = epsilon_1,
                                 prediction_list = prediction_list, folds = folds, 
                                 update = TRUE, simplify = FALSE)
      }
      # update full long data
      full_long_data <- Reduce(rbind, update_long_data_list)
      
      # compute mean of EIF
      D1 <- .Dy(full_long_data, y = 1)
      D0 <- .Dy(full_long_data, y = 0)
      ic <- D1 + D0 
      PnDstar <- mean(ic)

      # compute estimated cv-AUC 
      if(!nested_cv){
        dist_psix_y0_star <- lapply(prediction_list, .get_psi_distribution, 
                               y = 0, epsilon = epsilon_0)
        dist_psix_y1_star <- lapply(prediction_list, .get_psi_distribution, y = 1,
                               epsilon = epsilon_1)
      }else{
        dist_psix_y0_star <- sapply(1:K, .get_psi_distribution_nested_cv, 
                               y = 0, epsilon = epsilon_0, folds = folds, 
                               prediction_list = prediction_list, simplify = FALSE)
        dist_psix_y1_star <- sapply(1:K, .get_psi_distribution_nested_cv, 
                               y = 1, epsilon = epsilon_1, folds = folds, 
                               prediction_list = prediction_list, simplify = FALSE)
      }

      # get AUC
      tmle_auc[iter] <- mean(mapply(FUN = .get_auc, dist_y0 = dist_psix_y0_star, 
                         dist_y1 = dist_psix_y1_star))

    }
  }
  # standard error of CVTMLE
  se_cvtmle <- sqrt(var(ic)/n)

  # compute standard cvAUC estimator
  valid_pred_list <- lapply(prediction_list[1:K], "[[", "test_pred")
  valid_label_list <- lapply(prediction_list[1:K], "[[", "test_y")
  if(K <= n){
    regular_cvauc <- tryCatch({
      ci.cvAUC_withIC(predictions = valid_pred_list, labels = valid_label_list)
    }, error = function(e){ return(list(cvAUC = NA, se = NA))})
  }else{
    # this is for computing the weird LOO estimator
    regular_cvauc <- tryCatch({
      ci.cvAUC_withIC(predictions = unlist(valid_pred_list),
                            labels = unlist(valid_label_list))
    }, error = function(e){ return(list(cvAUC = NA, se = NA)) })
  }
  est_empirical <- regular_cvauc$cvAUC
  se_empirical <- regular_cvauc$se
  ic_emp <- regular_cvauc$ic
  if(init_auc == 0.5){
    se_onestep <- se_cvtmle <- se_esteq <- se_empirical
    iter <- 1
  }
 
  # format output
  out <- list()
  out$est_cvtmle <- tmle_auc[iter]
  out$iter_cvtmle <- iter
  out$cvtmle_trace <- tmle_auc
  out$se_cvtmle <- se_cvtmle
  out$est_init <- init_auc
  out$est_empirical <- est_empirical
  out$se_empirical <- se_empirical
  out$est_onestep <- est_onestep
  out$se_onestep <- se_onestep
  out$est_esteq <- est_esteq
  out$se_esteq <- se_esteq
  out$folds <- folds
  out$ic_cvtmle <- ic
  out$ic_onestep <- ic_os
  out$ic_esteq <- ic_os
  out$ic_empirical <- ic_emp

  out$prediction_list <- prediction_list
  class(out) <- "cvauc"
  return(out)
}


#' ci.cvAUC_withIC
#' 
#' This function is nearly verbatim \link[cvAUC]{ci.cvAUC} from the cvAUC package. 
#' The only difference is that it additionally returns estimated influence functions.
#' 
#' @param predictions A vector, matrix, list, or data frame containing the predictions.
#' @param labels A vector, matrix, list, or data frame containing the true class labels. Must have the 
#' same dimensions as \code{predictions}.
#' @param label.ordering The default ordering of the classes can be changed by supplying 
#' a vector containing the negative and the positive class label (negative label first, 
#' positive label second).
#' @param folds If specified, this must be a vector of fold ids equal in length to \code{predictions} 
#' and \code{labels}, or a list of length V (for V-fold cross-validation) of vectors of indexes for 
#' the observations contained in each fold. The \code{folds} argument must only be specified if 
#' the \code{predictions} and \code{labels} arguments are vectors.
#' @param confidence number between 0 and 1 that represents confidence level.
#' 
#' @importFrom cvAUC AUC cvAUC
#' @importFrom data.table data.table
#' @importFrom stats var qnorm binomial
#' 
#' @return A list containing the following named elements: 
#' \item{cvAUC}{Cross-validated area under the curve estimate.}
#' \item{se}{Standard error.}
#' \item{ci}{A vector of length two containing the upper and lower bounds for the confidence interval.}
#' \item{confidence}{A number between 0 and 1 representing the confidence.}
#' \item{ic}{A vector of the influence function evaluated at observations.}

ci.cvAUC_withIC <- function(predictions, labels, label.ordering = NULL, 
                            folds = NULL, confidence = 0.95) {
  
  # Pre-process the input
  clean <- .process_input(predictions = predictions, labels = labels, 
                          label.ordering = label.ordering, folds = folds,
                          ids = NULL, confidence = confidence)
  
  predictions <- clean$predictions  # Length-V list of predicted values
  labels <- clean$labels  # Length-V list of true labels
  pos <- levels(labels[[1]])[[2]]  # Positive class label
  neg <- levels(labels[[1]])[[1]]  # Negative class label
  n_obs <- length(unlist(labels))  # Number of observations
  
  # Inverse probability weights across entire data set
  w1 <- 1/(sum(unlist(labels) == pos)/n_obs)  # Inverse weights for positive class
  w0 <- 1/(sum(unlist(labels) == neg)/n_obs)  # Inverse weights for negative class

  # This is required to cleanly get past R CMD CHECK
  # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  pred <- label <- NULL
  fracNegLabelsWithSmallerPreds <- fracPosLabelsWithLargerPreds <- icVal <- NULL  

  .IC <- function(fold_preds, fold_labels, pos, neg, w1, w0) {
      n_rows <- length(fold_labels)
      n_pos <- sum(fold_labels == pos)
      n_neg <- n_rows - n_pos
      auc <- cvAUC::AUC(fold_preds, fold_labels)
      DT <- data.table::data.table(pred = fold_preds, label = fold_labels)
      DT <- DT[order(pred, -xtfrm(label))]
      DT[, fracNegLabelsWithSmallerPreds := cumsum(label == neg)/n_neg]
      DT <- DT[order(-pred, label)]
      DT[, fracPosLabelsWithLargerPreds := cumsum(label == pos)/n_pos]
      DT[, icVal := ifelse(label == pos, w1 * 
                           (fracNegLabelsWithSmallerPreds - auc), 
                           w0 * (fracPosLabelsWithLargerPreds - auc))]
      return(DT$icVal)
  }

  icOut <- mapply(FUN = .IC, SIMPLIFY = FALSE, fold_preds = predictions, 
    fold_labels = labels, MoreArgs = list(pos = pos, neg = neg, w1 = w1, w0 = w0))
  # not back-sorted
  ic <- unlist(icOut)
  # Estimated variance
  sighat2 <- mean(unlist(lapply(icOut, stats::var)))
  se <- sqrt(sighat2/n_obs)  
  cvauc <- cvAUC::cvAUC(predictions, labels)$cvAUC
  z <- stats::qnorm(confidence + (1 - confidence)/2)
  ci_cvauc <- c(cvauc - (z * se), cvauc + (z * se))
  ci_cvauc[1] <- ifelse(ci_cvauc[1] < 0, 0, ci_cvauc[1])  #Truncate CI at [0,1]
  ci_cvauc[2] <- ifelse(ci_cvauc[2] > 1, 1, ci_cvauc[2]) 
  
  return(list(cvAUC = cvauc, se = se, ci = ci_cvauc, confidence = confidence, ic = ic))
}

#' Unexported function from cvAUC package
#' @param predictions A vector, matrix, list, or data frame containing the predictions.
#' @param labels A vector, matrix, list, or data frame containing the true class labels. Must have the 
#' same dimensions as \code{predictions}.
#' @param label.ordering The default ordering of the classes can be changed by supplying 
#' a vector containing the negative and the positive class label (negative label first, 
#' positive label second).
#' @param folds If specified, this must be a vector of fold ids equal in length to \code{predictions} 
#' and \code{labels}, or a list of length V (for V-fold cross-validation) of vectors of indexes for 
#' the observations contained in each fold. The \code{folds} argument must only be specified if 
#' the \code{predictions} and \code{labels} arguments are vectors.
#' @param ids Vector of ids
#' @param confidence confidence interval level
#' @importFrom ROCR prediction
.process_input <- function (predictions, labels, label.ordering = NULL, folds = NULL, 
    ids = NULL, confidence = NULL) 
{
    .vec_to_list <- function(idxs, vec) {
        return(vec[idxs])
    }
    if (!is.null(folds)) {
        if (class(predictions) == "list" | class(labels) == "list") {
            stop("If folds is specified, then predictions and labels must both be vectors.")
        }
        if (length(predictions) != length(labels)) {
            stop("predictions and labels must be equal length")
        }
        if (is.vector(folds) && !is.list(folds)) {
            if (length(folds) != length(labels)) {
                stop("folds vector must be the same length as the predictions/labels vectors.")
            }
            else {
                fids <- as.list(unique(folds))
                folds <- lapply(fids, function(fid, folds) {
                  which(folds == fid)
                }, folds)
            }
        }
        else if (!is.list(folds)) {
            stop("If specifying the folds argument, folds must be a list\n of vectors of indices that correspond to each CV fold or a vector of fold numbers\n the same size as the predictions/labels vectors.")
        }
        else if (length(unlist(folds)) != length(labels)) {
            stop("Number of observations in the folds argument does not equal number of predictions/labels.")
        }
        predictions <- sapply(folds, .vec_to_list, vec = predictions)
        labels <- sapply(folds, .vec_to_list, vec = labels)
        if (length(labels) > length(unlist(labels))) {
            stop("Number of folds cannot exceed the number of observations.")
        }
    }
    pred <- ROCR::prediction(predictions = predictions, labels = labels, 
        label.ordering = label.ordering)
    predictions <- pred@predictions
    labels <- pred@labels
    if (!is.null(ids)) {
        if (is.list(ids)) {
            if (length(unlist(ids)) != length(unlist(labels))) {
                stop("ids must contain same number of observations as predictions/labels.")
            }
        }
        else if (is.vector(ids)) {
            if (is.null(folds)) {
                ids <- list(ids)
            }
            else {
                ids <- sapply(folds, .vec_to_list, vec = ids)
            }
        }
        else if (is.matrix(ids) | is.data.frame(ids)) {
            ids <- as.list(data.frame(ids))
        }
        else {
            stop("Format of ids is invalid.")
        }
        if (length(ids) > 1) {
            n_ids <- sum(sapply(ids, function(i) {
                length(unique(i))
            }))
            if (length(unique(unlist(ids))) != n_ids) {
                warning("Observations with the same id are currently spread across multiple folds.\nAll observations with the same id must be in the same fold to avoid bias.")
            }
        }
    }
    if (!is.null(confidence)) {
        if (is.numeric(confidence) && length(confidence) == 1) {
            if (confidence <= 0 | confidence >= 1) {
                stop("confidence value must fall within (0,1)")
            }
        }
    }
    return(list(predictions = predictions, labels = labels, folds = folds, 
        ids = ids))
}

#' Helper function for CVTMLE grid search
#' @param epsilon Fluctuation parameter 
#' @param fld The \code{full_long_data_list} object created
#' @param tol Tolerance on predictions close to 0 or 1
#' @return A numeric value of negative log-likelihood
fluc_mod_optim_0 <- function(epsilon, fld, tol = 1e-3){
  p_eps <- stats::plogis(fld$logit_Fn + epsilon)
  p_eps[p_eps == 1] <- 1 - tol
  p_eps[p_eps == 0] <- tol
  loglik <- -sum(fld$targeting_weight_0 * (fld$outcome * log(p_eps) + (1-fld$outcome) * log(1 - p_eps)))
  return(loglik)
}
#' Helper function for CVTMLE grid search
#' @param epsilon Fluctuation parameter 
#' @param fld full_long_data_list
#' @param tol Tolerance on predictions close to 0 or 1
#' @return A numeric value of negative log-likelihood
fluc_mod_optim_1 <- function(epsilon, fld, tol = 1e-3){
  p_eps <- stats::plogis(fld$logit_Fn + epsilon)
  p_eps[p_eps == 1] <- 1 - tol
  p_eps[p_eps == 0] <- tol
  loglik <- -sum(fld$targeting_weight_1 * (fld$outcome * log(p_eps) + (1-fld$outcome) * log(1 - p_eps)))
  return(loglik)
}

#' An estimating function for cvAUC
#' @param auc The value of auc to find root for
#' @param prediction_list Entry in prediction_list
#' @param gn Marginal probability of outcome
#' @return A numeric value of the estimating function evaluated at current
#' \code{auc} estimate. 
.estim_fn <- function(auc = 0.5, prediction_list, gn){
  # get first influence function piece for everyone
  ic_1 <- 
  Reduce("c",lapply(prediction_list, function(x){
    thisFn <- sapply(1:length(x$test_y), function(i){
      ifelse(x$test_y[i] == 1, 
             F_nBn_star(x$test_pred[i], y = 0, train_pred = x$train_pred,
                        train_y = x$train_y)/ gn , 
             (1 - F_nBn_star(x$test_pred[i], y = 1, train_pred = x$train_pred,
                        train_y = x$train_y))/(1 - gn))
    })
  }))
  all_y <- unlist(lapply(prediction_list, "[[", "test_y"))
  ic_2 <- rep(0, length(all_y))
  ic_2[all_y == 0] <- - auc / (1 - gn)
  ic_2[all_y == 1] <- - auc / gn
  return(mean(ic_1 + ic_2))
}

#' An estimating function for cvAUC with initial estimates generated via 
#' nested cross-validation
#' @param auc The value of auc to find root for
#' @param prediction_list Entry in prediction_list
#' @param gn Marginal probability of outcome
#' @param folds Cross-validation folds
#' @param K Number of CV folds
#' @return A numeric value of the estimating function evaluated at current
#' \code{auc} estimate. 
.estim_fn_nested_cv <- function(auc = 0.5, prediction_list, folds, gn, K){
  # get first influence function piece for everyone
  ic_1 <- 
  Reduce("c",sapply(1:K, function(x){
    valid_folds_idx <- which(unlist(lapply(prediction_list, function(z){ 
    x %in% z$valid_folds }), use.names = FALSE))

    # get only inner validation predictions
    inner_valid_prediction_and_y_list <- lapply(prediction_list[valid_folds_idx[-1]], 
                                        function(z){
      # pick out the fold that is not the outer validation fold
      inner_valid_idx <- which(!(z$valid_ids %in% folds[[x]]))
      # get predictions for this fold
      inner_pred <- z$test_pred[inner_valid_idx]
      inner_y <- z$test_y[inner_valid_idx]
      return(list(test_pred = inner_pred, inner_test_y = inner_y))
    })
    thisFn <- sapply(1:length(prediction_list[[valid_folds_idx[1]]]$test_y), function(i){
      ifelse(prediction_list[[valid_folds_idx[1]]]$test_y[i] == 1, 
       F_nBn_star_nested_cv(prediction_list[[valid_folds_idx[1]]]$test_pred[i], y = 0,
          inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)/ gn , 
       (1 - F_nBn_star_nested_cv(prediction_list[[valid_folds_idx[1]]]$test_pred[i], y = 1,
          inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list))/(1 - gn))
    })
  }))
  all_y <- unlist(lapply(prediction_list[1:K], "[[", "test_y"))
  ic_2 <- rep(0, length(all_y))
  ic_2[all_y == 0] <- - auc / (1 - gn)
  ic_2[all_y == 1] <- - auc / gn
  return(mean(ic_1 + ic_2))
}

#' Compute one of the terms of the efficient influence function
#' @param full_long_data A long form data set
#' @param y Which portion of the EIF to compute
#' @return Vector of one piece of EIF evaluated at estimates in \code{full_long_data}
.Dy <- function(full_long_data, y){
  by(full_long_data, full_long_data$id, function(x){
    sum((-1)^y * as.numeric(x$Y == y)/(x$gn) * (x$outcome - x$Fn) * x$dFn)
  })
}

#' Compute the targeted conditional cumulative distribution of the learner at a point
#' @param psi_x Value to compute conditional (on Y=y) cdf of learner
#' @param y Value of Y to condition on 
#' @param train_pred Values of Psi_nBn(X) from training sample
#' @param train_y Values of Y from training sample
#' @param epsilon Vector of fluctuation parameter estimates
#' @param tol Truncation level for logistic transformation
#' @importFrom stats plogis
#' @return Numeric value of CDF at \code{psi_x}
F_nBn_star <- function(psi_x, y, train_pred, train_y, 
                       epsilon = 0, tol = 1e-3){
  stats::plogis(SuperLearner::trimLogit(mean(train_pred[train_y %in% y] <= psi_x), tol) +
          sum(epsilon))
}

#' Compute the targeted conditional cumulative distribution of the learner at a point
#' where the initial distribution is based on cross validation
#' @param psi_x Value to compute conditional (on Y=y) cdf of learner
#' @param y Value of Y to condition on 
#' @param epsilon Vector of fluctuation parameter estimates
#' @param tol A truncation level when taking logit transformations. 
#' @param inner_valid_prediction_and_y_list A list of predictions and y's from \code{.get_predictions}.
#' @return Numeric value of CDF at \code{psi_x}
F_nBn_star_nested_cv <- function(psi_x, y, inner_valid_prediction_and_y_list, 
                                 epsilon = 0, tol = 1e-3){
  # get cdf estimated in each validation fold
  all_cv_est <- lapply(inner_valid_prediction_and_y_list, function(z){
    stats::plogis(SuperLearner::trimLogit(mean(z$test_pred[z$inner_test_y %in% y] <= psi_x), tol) +
          sum(epsilon))
  })
  # average over folds
  return(mean(unlist(all_cv_est, use.names = FALSE), na.rm = TRUE))
}

#' Worker function to make long form data set needed for
#' CVTMLE targeting step 
#' 
#' @param x An entry in the "predictions list" that has certain
#' named values (see \code{?.get_predictions})
#' @param gn An estimate of the probability that \code{Y = 1}.
#' @param update A boolean of whether this is called for initial
#' construction of the long data set or as part of the targeting loop. 
#' If the former, empirical "density" estimates are used. If the latter
#' these are derived from the targeted cdf. 
#' @param epsilon_0 If \code{update = TRUE}, a vector of TMLE fluctuation
#' parameter estimates used to add the CDF and PDF of Psi(X) to the data set.
#' @param epsilon_1 Same as for \code{epsilon_0}.
#' @param tol A truncation level when taking logit transformations. 
#' 
#' @return A long form data list of a particular set up. Columns are named id 
#' (multiple rows per observation in validation sample), 
#' u (if Yi = 0, these are the values of psi(x) in the
#' training sample for obs with Y = 1, if Yi = 1, these are values of psi(x) in
#' the training sample for obs. with Y = 0), 
#' Yi (this observation's value of Y), Fn (estimated value of the cdf of psi(X) 
#' given Y = Yi in the training sample), 
#' dFn (estimated value of the density of psi(X) given Y = (1-Yi) in the 
#' training sample), psi (the value of this observations Psihat(P_{n,B_n}^0)),
#' gn (estimate of marginal of Y e.g., computed in whole sample), outcome (indicator
#' that psix <= u), logit_Fn (the cdf estimate on the logit scale, needed for 
#' offset in targeting model).

.make_long_data <- function(x, gn, update = FALSE, epsilon_0 = 0, epsilon_1 = 0,
                          tol = 1e-3
                          ){
  # first the dumb way, writing a loop over x$test_pred
  uniq_train_psi_y0 <- sort(unique(x$train_pred[x$train_y == 0]))
  uniq_train_psi_y1 <- sort(unique(x$train_pred[x$train_y == 1]))
  # ord_train_psi_y0 <- order(x$train_pred[x$train_y == 0])
  # ord_train_psi_y1 <- order(x$train_pred[x$train_y == 1])
  
  n_valid <- length(x$test_pred)
  n_train <- length(x$train_pred)
  n1_train <- sum(x$train_y)
  n1_uniq_train <- length(uniq_train_psi_y1)
  n0_train <- n_train - n1_train
  n0_uniq_train <- length(uniq_train_psi_y0)
  n1_valid <- sum(x$test_y)
  n0_valid <- n_valid - n1_valid
  valid_ids <- as.numeric(names(x$test_pred))
  tot_length <- n1_valid * n0_uniq_train + n0_valid * n1_uniq_train

  idVec <- rep(NA, tot_length)
  uVec <- rep(NA, tot_length)
  YuVec <- rep(NA, tot_length)
  YiVec <- rep(NA, tot_length)
  F_1n_Bn_uVec <- rep(NA, tot_length)
  F_0n_Bn_uVec <- rep(NA, tot_length)
  dF_1n_Bn_uVec <- rep(NA, tot_length)
  dF_0n_Bn_uVec <- rep(NA, tot_length)
  FnVec <- rep(NA, tot_length)
  dFnVec <- rep(NA, tot_length)
  psiVec <- rep(NA, tot_length)

  # cumulative dist of psi_n | Y = 1 evaluated at all values of 
  # psi_n associated with Y = 0 observations
  F1nBn <- sapply(uniq_train_psi_y0, 
                  F_nBn_star, train_pred = x$train_pred, y = 1, 
                  train_y = x$train_y, epsilon = epsilon_1)
  # cumulative dist of psi_n | Y = 0 evaluated at all values of 
  # psi_n associated with Y = 1 observations
  F0nBn <- sapply(uniq_train_psi_y1, 
                  F_nBn_star, train_pred = x$train_pred, y = 0, 
                  train_y = x$train_y, epsilon = epsilon_0)

  # empirical dens of psi_n | Y = 1 and Y = 0 evaluated at all 
  if(!update){
    dF1nBn <- as.numeric(table(x$train_pred[x$train_y == 1])/n1_train)
    dF0nBn <- as.numeric(table(x$train_pred[x$train_y == 0])/n0_train)
  }else{
    # cumulative dist of psi_n | Y = 1 evaluated at all values of 
    # psi_n associated with Y = 0 observations
    F1nBn_1 <- sapply(uniq_train_psi_y1, 
                    F_nBn_star, train_pred = x$train_pred, y = 1, 
                    train_y = x$train_y, epsilon = epsilon_1)
    dF1nBn <- diff(c(0, F1nBn_1))
    # cumulative dist of psi_n | Y = 0 evaluated at all values of 
    # psi_n associated with Y = 1 observations
    F0nBn_0 <- sapply(uniq_train_psi_y0, 
                    F_nBn_star, train_pred = x$train_pred, y = 0, 
                    train_y = x$train_y, epsilon = epsilon_0)
    dF0nBn <- diff(c(0, F0nBn_0))
  }

  # loop over folks in validation fold
  cur_start <- 1
  for(i in seq_len(n_valid)){
    if(x$test_y[i] == 0){
      cur_end <- cur_start + n1_uniq_train - 1
      idVec[cur_start:cur_end] <- x$valid_ids[i]
      # ordered unique values of psi in training | y = 1
      uVec[cur_start:cur_end] <- uniq_train_psi_y1
      # value of this Y_i
      YiVec[cur_start:cur_end] <- 0
      # cdf of psi | y = 0 in training at each u
      FnVec[cur_start:cur_end] <- F0nBn
      # pdf of psi | y = 1 in training at each u
      dFnVec[cur_start:cur_end] <- dF1nBn
      # vector of this psi
      psiVec[cur_start:cur_end] <- x$test_pred[i]
    }else{
      cur_end <- cur_start + n0_uniq_train - 1
      idVec[cur_start:cur_end] <- x$valid_ids[i]
      # ordered unique values of psi in training | y = 0
      uVec[cur_start:cur_end] <- uniq_train_psi_y0
      # value of this Y_i
      YiVec[cur_start:cur_end] <- 1
      # cdf of psi | y = 1 in training at each u
      FnVec[cur_start:cur_end] <- F1nBn
      # pdf of psi | y = 0 in training at each u
      dFnVec[cur_start:cur_end] <- dF0nBn
      # vector of this psi
      psiVec[cur_start:cur_end] <- x$test_pred[i]
    }
    cur_start <- cur_end + 1
  }

  out <- data.frame(id = idVec, u = uVec,
                   Yi = YiVec, Fn = FnVec, dFn = dFnVec,
                   psi = psiVec)

  # add in gn
  out$gn <- NA
  out$gn[out$Yi == 1] <- gn
  out$gn[out$Yi == 0] <- 1 - gn
  # add in "outcome"
  out$outcome <- as.numeric(out$psi <= out$u)
  # add in logit(Fn)
  out$logit_Fn <- SuperLearner::trimLogit(out$Fn, tol)
  return(out)
}


#' Worker function to make long form data set needed for
#' CVTMLE targeting step when nested cv is used
#' 
#' @param x The outer validation fold
#' @param prediction_list The full prediction list
#' @param gn An estimate of the marginal dist. of Y
#' @param update Boolean of whether this is called for initial
#' construction of the long data set or as part of the targeting loop. 
#' If the former, cross-validated empirical "density" estimates are used. 
#' If the latter these are derived from the targeted cdf. 
#' @param epsilon_0 If \code{update = TRUE}, a vector of TMLE fluctuation
#' parameter estimates used to add the CDF and PDF of Psi(X) to the data set
#' @param epsilon_1 Ditto above
#' @param folds Vector of CV folds
#' @param tol A truncation level when taking logit transformations. 
#' 
#' @return A long form data list of a particular set up. Columns are named id 
#' (multiple per obs. in validation sample), u (if Yi = 0, these are the unique 
#' values of psi(x) in the inner validation samples for psi fit on inner training
#' samples for obs with Y = 1, if Yi = 1, these are values of psi(x) in
#' the inner validation samples for psi fit on inner training samples for obs. 
#' with Y = 0), Yi (this id's value of Y), Fn (cross-validation estimated value 
#' of the cdf of psi(X) given Y = Yi in the training sample), 
#' dFn (cross-validated estimate of the density of psi(X) given Y = (1-Yi) in the 
#' training sample), psi (the value of this observations Psihat(P_{n,B_n}^0)),
#' gn (estimate of marginal of Y e.g., computed in whole sample), outcome (indicator
#' that psix <= u), logit_Fn (the cdf estimate on the logit scale, needed for 
#' offset in targeting model).
.make_long_data_nested_cv <- function(x, prediction_list, folds, gn, update = FALSE, epsilon_0 = 0, epsilon_1 = 0,
                          # tol = .Machine$double.neg.eps, 
                          tol = 1e-3
                          ){
  # find all V-1 fold CV fits with this x in them. These will be the inner
  # CV fits that are needed. The first entry in this vector will correspond
  # to the outer V fold CV fit, which is what we want to make the outcome 
  # of the long data list with. 
  valid_folds_idx <- which(unlist(lapply(prediction_list, function(z){ 
    x %in% z$valid_folds }), use.names = FALSE))

  # get only inner validation predictions
  inner_valid_prediction_and_y_list <- lapply(prediction_list[valid_folds_idx[-1]], 
                                        function(z){
    # pick out the fold that is not the outer validation fold
    inner_valid_idx <- which(!(z$valid_ids %in% folds[[x]]))
    # get predictions for this fold
    inner_pred <- z$test_pred[inner_valid_idx]
    inner_y <- z$test_y[inner_valid_idx]
    return(list(test_pred = inner_pred, inner_test_y = inner_y))
  })

  # now get all values of psi from inner validation with Y = 0 
  uniq_train_psi_y0 <- sort(unique(unlist(lapply(inner_valid_prediction_and_y_list, function(z){
    z$test_pred[z$inner_test_y == 0]    
  }), use.names = FALSE)))
  uniq_train_psi_y1 <- sort(unique(unlist(lapply(inner_valid_prediction_and_y_list, function(z){
    z$test_pred[z$inner_test_y == 1]    
  }), use.names = FALSE)))
  
  # number in outer validation sample 
  # NOTE: valid_folds_idx[1] is the fit on V-1 folds 
  n_valid <- length(prediction_list[[valid_folds_idx[1]]]$test_pred)
  # number in outer training sample... is this what I want?
  n_train <- length(prediction_list[[valid_folds_idx[1]]]$train_pred)
  n1_train <- sum(prediction_list[[valid_folds_idx[1]]]$train_y)
  n1_uniq_train <- length(uniq_train_psi_y1)
  n0_train <- n_train - n1_train
  n0_uniq_train <- length(uniq_train_psi_y0)
  n1_valid <- sum(prediction_list[[valid_folds_idx[1]]]$test_y)
  n0_valid <- n_valid - n1_valid
  tot_length <- n1_valid * n0_uniq_train + n0_valid * n1_uniq_train

  idVec <- rep(NA, tot_length)
  uVec <- rep(NA, tot_length)
  YuVec <- rep(NA, tot_length)
  YiVec <- rep(NA, tot_length)
  F_1n_Bn_uVec <- rep(NA, tot_length)
  F_0n_Bn_uVec <- rep(NA, tot_length)
  dF_1n_Bn_uVec <- rep(NA, tot_length)
  dF_0n_Bn_uVec <- rep(NA, tot_length)
  FnVec <- rep(NA, tot_length)
  dFnVec <- rep(NA, tot_length)
  psiVec <- rep(NA, tot_length)

  # now need CV-averaged CDF
  # cumulative dist of psi_n | Y = 1 evaluated at all values of 
  # psi_n associated with Y = 0 observations
  F1nBn <- sapply(uniq_train_psi_y0, 
                  F_nBn_star_nested_cv, y = 1, 
                  epsilon = epsilon_1,
                  inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)

  # cumulative dist of psi_n | Y = 0 evaluated at all values of 
  # psi_n associated with Y = 1 observations
  F0nBn <- sapply(uniq_train_psi_y1, 
                  F_nBn_star_nested_cv, y = 0, 
                  epsilon = epsilon_0,
                  inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)

  # cv empirical density of psi_n | Y = 1 and Y = 0 evaluated at all 
  # cumulative dist of psi_n | Y = 1 evaluated at all values of 
  # psi_n associated with Y = 0 observations
  F1nBn_1 <- sapply(uniq_train_psi_y1, 
                  F_nBn_star_nested_cv, y = 1, 
                  epsilon = epsilon_1,
                  inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)
  dF1nBn <- diff(c(0, F1nBn_1))
  # cumulative dist of psi_n | Y = 0 evaluated at all values of 
  # psi_n associated with Y = 1 observations
  F0nBn_0 <- sapply(uniq_train_psi_y0, 
                  F_nBn_star_nested_cv, y = 0, 
                  epsilon = epsilon_0,
                  inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)
  dF0nBn <- diff(c(0, F0nBn_0))

  # loop over folks in validation fold
  cur_start <- 1
  for(i in seq_len(n_valid)){
    if(prediction_list[[valid_folds_idx[1]]]$test_y[i] == 0){
      cur_end <- cur_start + n1_uniq_train - 1
      idVec[cur_start:cur_end] <- prediction_list[[valid_folds_idx[1]]]$valid_ids[i]
      # ordered unique values of psi in training | y = 1
      uVec[cur_start:cur_end] <- uniq_train_psi_y1
      # value of this Y_i
      YiVec[cur_start:cur_end] <- 0
      # cdf of psi | y = 0 in training at each u
      FnVec[cur_start:cur_end] <- F0nBn
      # pdf of psi | y = 1 in training at each u
      dFnVec[cur_start:cur_end] <- dF1nBn
      # vector of this psi
      psiVec[cur_start:cur_end] <- prediction_list[[valid_folds_idx[1]]]$test_pred[i]
    }else{
      cur_end <- cur_start + n0_uniq_train - 1
      idVec[cur_start:cur_end] <- prediction_list[[valid_folds_idx[1]]]$valid_ids[i]
      # ordered unique values of psi in training | y = 0
      uVec[cur_start:cur_end] <- uniq_train_psi_y0
      # value of this Y_i
      YiVec[cur_start:cur_end] <- 1
      # cdf of psi | y = 1 in training at each u
      FnVec[cur_start:cur_end] <- F1nBn
      # pdf of psi | y = 0 in training at each u
      dFnVec[cur_start:cur_end] <- dF0nBn
      # vector of this psi
      psiVec[cur_start:cur_end] <- prediction_list[[valid_folds_idx[1]]]$test_pred[i]
    }
    cur_start <- cur_end + 1
  }
  out <- data.frame(id = idVec, u = uVec,
                   Yi = YiVec, Fn = FnVec, dFn = dFnVec,
                   psi = psiVec)

  # add in gn
  out$gn <- NA
  out$gn[out$Yi == 1] <- gn
  out$gn[out$Yi == 0] <- 1 - gn
  # add in "outcome"
  out$outcome <- as.numeric(out$psi <= out$u)
  # add in logit(Fn)
  out$logit_Fn <- SuperLearner::trimLogit(out$Fn, tol)
  return(out)
}

#' Compute the AUC given the cdf and pdf of psi 
#' 
#' See \code{?.get_psi_distribution} to understand expected input format
#' 
#' @param dist_y0 Distribution of psi given Y = 0
#' @param dist_y1 Distribution of psi given Y = 1
#' @return Numeric value of AUC
.get_auc <- function(dist_y0, dist_y1){
  if(identical(dist_y0, dist_y1)){
    tot <- 0.5
  } else {
    tot <- 0
    for(i in seq_along(dist_y0$psix)){
      idx <- findInterval(x = dist_y0$psix[i], vec = dist_y1$psix)
      p1 <- ifelse(idx == 0, 1, (1 - dist_y1$Fn[idx]))
      p2 <- dist_y0$dFn[i]
      tot <- tot + p1 * p2
    }
  }
  return(tot)
}

#' Compute the conditional (given Y = y) estimated distribution of psi
#' 
#' @param x An entry in the output from .get_predictions
#' @param y What value of Y to compute dist. est. 
#' @param epsilon A vector of estimated coefficients form tmle fluctuation 
#' submodels. 
#' 
#' @return A data.frame with the distribution of psi given Y = y with names
#' psix (what value estimates are evaluated at), dFn (density estimates),
#' Fn (cdf estimates)
.get_psi_distribution <- function(x, y, epsilon = 0){
    this_n <- length(x$train_pred[x$train_y == y])
    uniq_train_psi_y <- sort(unique(x$train_pred[x$train_y == y]))
    FynBn_y <- sapply(uniq_train_psi_y, 
                F_nBn_star, train_pred = x$train_pred, y = y, 
                train_y = x$train_y, epsilon = epsilon)
    dFynBn <- diff(c(0, FynBn_y))
    out <- data.frame(psix = uniq_train_psi_y, 
                      dFn = dFynBn,
                      Fn = FynBn_y)
    return(out)
}


#' Compute the conditional (given Y = y) CV-estimated distribution of psi
#' 
#' @param x The outer validation fold withheld
#' @param y What value of Y to compute dist. est. 
#' @param prediction_list List output from .get_predictions.
#' @param folds Cross validation fold indicator. 
#' @param epsilon A vector of estimated coefficients form tmle fluctuation 
#' submodels. 
#' 
#' @return A data.frame with the distribution of psi given Y = y with names
#' psix (what value estimates are evaluated at), dFn (density estimates),
#' Fn (cdf estimates)
.get_psi_distribution_nested_cv <- function(x, y, prediction_list, folds, epsilon = 0){
  # find all V-1 fold CV fits with this x in them. These will be the inner
  # CV fits that are needed. The first entry in this vector will correspond
  # to the outer V fold CV fit, which is what we want to make the outcome 
  # of the long data list with. 
  valid_folds_idx <- which(unlist(lapply(prediction_list, function(z){ 
    x %in% z$valid_folds }), use.names = FALSE))

  # get only inner validation predictions
  inner_valid_prediction_and_y_list <- lapply(prediction_list[valid_folds_idx[-1]], 
                                        function(z){
    # pick out the fold that is not the outer validation fold
    inner_valid_idx <- which(!(z$valid_ids %in% folds[[x]]))
    # get predictions for this fold
    inner_pred <- z$test_pred[inner_valid_idx]
    inner_y <- z$test_y[inner_valid_idx]
    return(list(test_pred = inner_pred, inner_test_y = inner_y))
  })

  # now get all values of psi from inner validation with Y = 0 
  uniq_train_psi_y <- sort(unique(unlist(lapply(inner_valid_prediction_and_y_list, function(z){
    z$test_pred[z$inner_test_y == y]    
  }), use.names = FALSE)))
  
  FynBn_y <- sapply(uniq_train_psi_y, 
                  F_nBn_star_nested_cv, y = y, 
                  epsilon = epsilon,
                  inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)
    dFynBn <- diff(c(0, FynBn_y))
    out <- data.frame(psix = uniq_train_psi_y, 
                      dFn = dFynBn,
                      Fn = FynBn_y)
    return(out)
}

#' Worker function for fitting prediction functions (possibly in parallel)
#' 
#' @param learner The wrapper to use
#' @param Y The outcome
#' @param X The predictors
#' @param K The number of folds
#' @param parallel Whether to compute things in parallel using future
#' @param folds Vector of CV fold assignments
#' @param nested_cv Is nested CV being used?
#' @param nested_K How many folds of nested CV?
#' @importFrom utils combn
#' @return A list of the result of the wrapper executed in each fold
.get_predictions <- function(learner, Y, X, K = 10, folds, parallel, nested_cv = FALSE,
                            nested_K = K - 1){
  .doFit <- function(x, tmpX, Y, folds, learner, seed = 21){
    set.seed(seed)
    out <- do.call(learner, args=list(
            train = list(Y = Y[-unlist(folds[x])], 
                         X = tmpX[-unlist(folds[x]),,drop=FALSE]),
            test = list(Y = Y[unlist(folds[x])], 
                        X = tmpX[unlist(folds[x]),,drop=FALSE]))
    )
    out$valid_ids <- unlist(folds[x], use.names = FALSE)
    out$valid_folds <- x
    return(out)
  }

  
  if(!nested_cv){
    valid_folds <- split(seq(K),factor(seq(K)))
  }else{
    if(nested_K == K - 1){
      combns <- utils::combn(K, 2)
      valid_folds <- c(split(seq(K), factor(seq(K))),
                       split(combns, col(combns)))
    }
  }

  if(parallel){
    stop("Parallel processing code needs to be re-written.")
  }else{
    # TO DO: make clear that this gets called if nested_cv = FALSE
    if(nested_K == K - 1){
      predFitList <- lapply(valid_folds ,FUN = .doFit, tmpX = X, 
                            Y = Y, folds = folds, learner = learner)
    }else if(nested_cv & nested_K != K - 1){
      inner_folds <- vector(mode = "list", length = K)
      for(k in seq_len(K)){
        train_idx <- unlist(folds[-k], use.names = FALSE)
        # these will just be numbers 1:length(train_idx)
        inner_folds[[k]] <- SuperLearner::CVFolds(N = length(train_idx), 
                                                        id = NULL, Y = Y[train_idx], 
                                   cvControl = list(V = nested_K, 
                                                    stratifyCV = ifelse(nested_K <= sum(Y[train_idx]) 
                                                                        & nested_K <= sum(!Y[train_idx]), 
                                                                        TRUE, FALSE), 
                                   shuffle = TRUE, validRows = NULL))
        # so replace them with actual ids
        inner_folds[[k]] <- lapply(inner_folds[[k]], function(x){
          train_idx[x]
        })
      }
      fold_combos <- expand.grid(outer_K = seq_len(K),
                                 inner_K = c(0,seq_len(nested_K)))
      # here x will be a data.frame with columns outer_K and inner_K
      .doFit2 <- function(x, tmpX, Y, folds, inner_folds, learner, seed = 21){
        if(x[2] == 0){
          set.seed(21)
          # in this case, just remove from folds
          out <- do.call(learner, args=list(train = list(Y = Y[-unlist(folds[x[1]])], 
                                                         X = tmpX[-unlist(folds[x[1]]),,drop=FALSE]),
                                      test = list(Y = Y[unlist(folds[x[1]])], 
                                                  X = tmpX[unlist(folds[x[1]]),,drop=FALSE])))
          out$valid_ids <- unlist(folds[x[1]], use.names = FALSE)
          out$valid_folds <- x[1]
        }else{
          # browser()
          # in this case, remove from folds and inner folds
          outer_valid_idx <- unlist(folds[x[1]], use.names = FALSE)
          inner_valid_idx <- unlist(inner_folds[[x[1]]][x[2]], use.names = FALSE)
          remove_idx <- c(outer_valid_idx, inner_valid_idx)
          out <- do.call(learner, args=list(train = list(Y = Y[-remove_idx], 
                                                         X = tmpX[-remove_idx,,drop=FALSE]),
                                      test = list(Y = Y[inner_valid_idx], 
                                                  X = tmpX[inner_valid_idx, , drop = FALSE])))
          out$valid_ids <- inner_valid_idx
          # leave this corresponding to outer validation fold?
          out$valid_folds <- x[1]
        }
        return(out)
      }
      predFitList <- apply(fold_combos, 1, FUN = .doFit2, 
                           tmpX = X, Y = Y, folds = folds, 
                           inner_folds = inner_folds, 
                           learner = learner)      
    }
  }
  
  # return results
  return(predFitList)
}


#' Compute the leave-pair-out cross-validation estimator of AUC.
#' 
#' This estimator is computed by leaving out a pair of one case (\code{Y = 1}) and
#' one control (\code{Y = 0}). The learner is trained on the remaining observations
#' and predicted values are obtained for the left-out pair. The estimate is given by
#' the proportion of left-out pairs for which the case had higher predicted risk
#' than the control. 
#' 
#' @param Y A numeric vector of outcomes, assume to equal \code{0} or \code{1}.
#' @param X A \code{data.frame} of variables for prediction.
#' @param max_pairs The maximum number of pairs to leave out. 
#' @param learner A wrapper that implements the desired method for building a 
#' prediction algorithm. See \code{?glm_wrapper} or read the package vignette
#' for more information on formatting \code{learner}s.
#' @param parallel A boolean indicating whether prediction algorithms should be 
#' trained in parallel. Default to \code{FALSE}. 
#' @param ... Other options (not currently used)
#' @export
#' @examples
#' # simulate data
#' X <- data.frame(x1 = rnorm(50))
#' Y <- rbinom(50, 1, plogis(X$x1))
#' # compute lpo_auc for logistic regression
#' lpo <- lpo_auc(Y = Y, X = X, learner = "glm_wrapper")
#' 

lpo_auc <- function(Y, X, learner = "glm_wrapper", 
                    max_pairs = NULL, parallel = FALSE, ...){
  case_idx <- which(Y == 1)
  control_idx <- which(Y == 0)
  grid_idx <- expand.grid(case = case_idx, control = control_idx)
  if(!is.null(max_pairs)){
    if(nrow(grid_idx) > max_pairs){
      # randomly select max_pairs pairs
      grid_idx <- grid_idx[sample(1:nrow(grid_idx), size = max_pairs, replace = FALSE),]
    }
  }
  folds <- split(grid_idx, seq_len(nrow(grid_idx)))

  prediction_list <- .get_predictions(learner = learner, Y = Y, X = X, 
                               K = length(folds), folds=folds, parallel = parallel,
                               nested_cv = FALSE)

  zero_one_vec <- lapply(prediction_list, function(x){
    as.numeric(x$test_pred[1] > x$test_pred[2])
  })

  auc <- mean(unlist(zero_one_vec))

  return(list(auc = auc))
}

#' Compute the bootstrap-corrected estimator of AUC.
#' 
#' This estimator is computed by re-sampling with replacement (i.e., bootstrap
#' sampling) from the data. The AUC is computed for the learner trained on the 
#' full data. The AUC is then computed for the learner trained on each bootstrap
#' sample. The average difference between the full data-trained learner and 
#' the bootstrap-trained learner is computed to estimate the bias in the full-data-estimated
#' AUC. The final estimate of AUC is given by the difference in the full-data AUC 
#' and the estimated bias.
#' 
#' @param Y A numeric vector of outcomes, assume to equal \code{0} or \code{1}.
#' @param X A \code{data.frame} of variables for prediction.
#' @param B The number of bootstrap samples. 
#' @param learner A wrapper that implements the desired method for building a 
#' prediction algorithm. See \code{?glm_wrapper} or read the package vignette
#' for more information on formatting \code{learner}s.
#' @param correct632 A boolean indicating whether to use the .632 correction.
#' @param ... Other options, not currently used. 
#' @return A list with \code{$auc} as the bootstrap-corrected AUC estimate and 
#' \code{$n_valid_boot} as the number of bootstrap of bootstrap samples where \code{learner} 
#' successfully executed.
#' @export
#' @examples 
#' # simulate data
#' X <- data.frame(x1 = rnorm(50))
#' Y <- rbinom(50, 1, plogis(X$x1))
#' # compute lpo_auc for logistic regression 
#' # use small B for fast run
#' boot <- boot_auc(Y = Y, X = X, B = 25, learner = "glm_wrapper")
#' 
boot_auc <- function(Y, X, B = 500, learner = "glm_wrapper", correct632 = FALSE, ...){
  n <- length(Y)
  # get naive AUC
  full_fit <- do.call(learner, args=list(train = list(Y = Y, X = X),
                                      test = list(Y = Y, X = X)))
  naive_auc <- cvAUC::cvAUC(predictions = full_fit$test_pred,
                            labels = Y)$cvAUC
  all_boot <- replicate(B, one_boot_auc(Y = Y, X = X, n = n, 
                                        correct632 = correct632,
                                        learner = learner))
  if(!correct632){
    mean_optimism <- mean(all_boot, na.rm = TRUE)
    corrected_auc <- naive_auc - mean_optimism
  }else{
    # get overfitting index
    # average boot auc
    auc_b <- mean(all_boot, na.rm = TRUE)
    # first copy each prediction n times
    long_pred <- rep(full_fit$test_pred, each = n)
    # now copy observed outcomes n times 
    long_y <- rep(Y, n)
    # get "overfit" correction factor
    g <- cvAUC::cvAUC(predictions = long_pred, labels = long_y)$cvAUC
    # define relative overfitting rate
    R <- (auc_b - naive_auc)/(g - naive_auc)
    # 632 weight
    w <- 0.632 / (1 - 0.368*R)
    # weighted estimate
    corrected_auc <- (1 - w)*naive_auc + w * auc_b
  }

  return(list(auc = corrected_auc, n_valid_boot = sum(!is.na(all_boot))))
}

#' Internal function used to perform one bootstrap sample. The function
#' \code{try}s to fit \code{learner} on a bootstrap sample. If for some reason
#' (e.g., the bootstrap sample contains no observations with \code{Y = 1}) 
#' the learner fails, then the function returns \code{NA}. These \code{NA}s 
#' are ignored later when computing the bootstrap corrected estimate. 
#' @param Y A numeric binary outcome
#' @param X A \code{data.frame} of variables for prediction.
#' @param correct632 A boolean indicating whether to use the .632 correction.
#' @param learner A wrapper that implements the desired method for building a 
#' prediction algorithm. See \code{?glm_wrapper} or read the package vignette
#' for more information on formatting \code{learner}s.
#' @param n Number of observations
#' @return If \code{learner} executes successfully, a numeric estimate of AUC
#' on this bootstrap sample. Otherwise the function returns \code{NA}.
one_boot_auc <- function(Y, X, n, correct632, learner){
  idx <- sample(seq_len(n), replace = TRUE)
  train_y <- Y[idx]
  train_X <- X[idx, , drop = FALSE]
  fit <- tryCatch({
    do.call(learner, args=list(train = list(Y = train_y, X = train_X),
                                    test = list(Y = Y, X = X)))
  }, error = function(e){
    return(NA)
  })
  if(!(class(fit) == "logical")){
    if(!correct632){
      train_cvauc <- tryCatch({cvAUC::cvAUC(predictions = fit$train_pred,
                              labels = train_y)$cvAUC}, error = function(e){
                                return(NA)})
      test_cvauc <- tryCatch({cvAUC::cvAUC(predictions = fit$test_pred,
                              labels = Y)$cvAUC}, error = function(e){
                                return(NA)})
      out <- train_cvauc - test_cvauc
    }else{
      # compute auc on held-out observations
      oob_idx <- which(!(1:n %in% idx))
      out <- tryCatch({cvAUC::cvAUC(predictions = fit$test_pred[oob_idx],
                              labels = Y[oob_idx])$cvAUC}, error = function(e){
                                return(NA)})
    }
    return(out)
  }else{
    return(NA)
  }
}
