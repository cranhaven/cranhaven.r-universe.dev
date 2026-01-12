#' Estimates of CV SCNP
#' 
#' This function computes K-fold cross-validated estimates of estimates of 
#' cross-validated sensitivity-constrained rate of negative prediction (SCRNP). This
#' quantity can be interpreted as the rate of negative classification for a fixed 
#' constraint on the sensitivity of a prediction algorithm. Thus, if an algorithm
#' has a high SCRNP, it will also have a high positive predictive value. 
#' 
#' To estimate the SCRNP using K-fold cross-validation is problematic. If 
#' data are partitioned into K distinct groups, depending on the sample size 
#' and choice of K, the validation sample may be quite small. In order to estimate 
#' SCRNP, we require estimation of a quantile of the predictor's distribution. More extreme
#' quantiles (which correspond to high sensitivity constraints) are difficult to estimate
#' using few observations. Here, we estimate relevant nuisance parameters in the training sample and use
#' the validation sample to perform some form of bias correction -- either through
#' cross-validated targeted minimum loss-based estimation, estimating equations, 
#' or one-step estimation. When aggressive learning algorithms are applied, it is
#' necessary to use an additional layer of cross-validation in the training sample
#' to estimate the nuisance parameters. This is controlled via the \code{nested_cv}
#' option below. 
#' 
#' @param Y A numeric vector of outcomes, assume to equal \code{0} or \code{1}.
#' @param X A \code{data.frame} or \code{matrix} of variables for prediction.
#' @param K The number of cross-validation folds (default is \code{10}).
#' @param sens The sensitivity constraint imposed on the rate of negative prediction
#' (see description).
#' @param learner A wrapper that implements the desired method for building a 
#' prediction algorithm. 
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
#' \code{cvtmle_cvtmle_ictol}. 
#' @param quantile_type Type of quantile estimator to be used. See \link[stats]{quantile}
#' for description. 
#' @param prediction_list For power users: a list of predictions made by \code{learner}
#' that has a format compatible with \code{cvauc}.
#' @param ... Other arguments, not currently used
#' @importFrom SuperLearner CVFolds
#' @importFrom cvAUC ci.cvAUC
#' @importFrom stats uniroot
#' @export
#' @return An object of class \code{"scrnp"}. \describe{
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
#' \item{\code{est_esteq}}{cross-validated estimating equations estimate of K-fold CV AUC
#' (here, equivalent to one-step, since the estimating equation is linear in SCRNP)}
#' \item{\code{se_esteq}}{estimated standard error for the estimating equations estimator 
#' (same as one-step)}
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
#' @examples
#' # simulate data
#' n <- 200
#' p <- 10
#' X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
#' Y <- rbinom(n, 1, plogis(X[,1] + X[,10]))
#' 
#' # estimate cv scrnp of logistic regression
#' scrnp_ests <- cv_scrnp(Y = Y, X = X, K = 5, 
#'                        nested_cv = FALSE, 
#'                        learner = "glm_wrapper")
#' 
#' # estimate cv scrnp of random forest with nested 
#' # cross-validation for nuisance parameter estimation
#' \donttest{
#' scrnp_ests <- cv_scrnp(Y = Y, X = X, K = 5, 
#'                        nested_cv = TRUE, 
#'                        learner = "randomforest_wrapper")
#' }

cv_scrnp <- function(Y, X, K = 10, sens = 0.95, 
                     learner = "glm_wrapper", 
                     nested_cv = TRUE, 
                     nested_K = K - 1, 
                     parallel = FALSE, 
                     max_cvtmle_iter = 10, 
                     cvtmle_ictol = 1/length(Y), 
                     quantile_type = 8,
                     prediction_list = NULL, 
                     ...){
  # test inputs
  assertthat::assert_that(all(Y %in% c(0,1)))
  assertthat::assert_that(0 < sens & sens < 1)
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
      folds=folds, parallel = FALSE, nested_cv = nested_cv
    )
  }

  # get quantile estimates
  if(!nested_cv){
    quantile_list <- lapply(prediction_list[seq_len(K)], .get_quantile, p = 1 - sens,
                            quantile_type = quantile_type)
  }else{
    quantile_list <- sapply(1:K, .get_nested_cv_quantile, quantile_type = quantile_type,
                             prediction_list = prediction_list, folds = folds,
                             p = 1 - sens, simplify = FALSE) 
  }

  # get density estimate 
  if(!nested_cv){
    density_list <- mapply(x = prediction_list[1:K], c0 = quantile_list, 
                           FUN = .get_density, SIMPLIFY = FALSE)
  }else{
    density_list <- mapply(x = split(seq_len(K), seq_len(K)), c0 = quantile_list, 
                           FUN = .get_density, SIMPLIFY = FALSE,
                           MoreArgs = list(prediction_list = prediction_list,
                                           folds = folds, nested_cv = nested_cv))
  }

  # make targeting data
  if(!nested_cv){
    target_and_pred_data <- .make_targeting_data(prediction_list = prediction_list, 
                                      quantile_list = quantile_list, 
                                      density_list = density_list, 
                                      folds = folds, gn = mean(Y))
    target_data <- target_and_pred_data$out
    pred_data <- target_and_pred_data$out_pred
  }else{
    target_and_pred_data <- sapply(seq_len(K), .make_targeting_data, 
                                        prediction_list = prediction_list, 
                                      quantile_list = quantile_list, 
                                      density_list = density_list, folds = folds,
                                      gn = mean(Y), nested_cv = TRUE, simplify = FALSE)
    target_data <- Reduce("rbind", lapply(target_and_pred_data, "[[", "out"))
    pred_data <- Reduce("rbind", lapply(target_and_pred_data, "[[", "out_pred"))
  }
  target_data$weight <- with(target_data, 1 - Y/gn * f_ratio)
  pred_data$weight <- with(pred_data, 1 - Y/gn * f_ratio)
  target_data$logit_Fn <- SuperLearner::trimLogit(target_data$Fn, trim = 1e-5)
  pred_data$logit_Fn <- SuperLearner::trimLogit(pred_data$Fn, trim = 1e-5)
  
  fluc_mod <- glm(ind ~ -1 + offset(logit_Fn) + weight, 
                  data = target_data, family = "binomial", start = 0)
  target_data$Fnstar <- fluc_mod$fitted.values
  pred_data$Fnstar <- predict(fluc_mod, newdata = pred_data, type = "response")
  # compute initial non-targeted estimates
  init_estimates <- by(pred_data, pred_data$fold, function(x){
    x$Fn[x$Y==0][1] * (1-x$gn[1]) + x$Fn[x$Y==1][1] * x$gn[1]
  })

  # compute parameter for each fold
  cvtmle_estimates <- by(pred_data, pred_data$fold, function(x){
    x$Fnstar[x$Y==0][1] * (1-x$gn[1]) + x$Fnstar[x$Y==1][1] * x$gn[1]
  })

  target_data$F0nstar <- NaN
  target_data$F1nstar <- NaN
  for(k in seq_len(K)){
    target_data$F0nstar[target_data$fold == k] <- pred_data$Fnstar[pred_data$Y == 0 & pred_data$fold == k][1]
    target_data$F0n[target_data$fold == k] <- pred_data$Fn[pred_data$Y == 0 & pred_data$fold == k][1]
    target_data$F1nstar[target_data$fold == k] <- pred_data$Fnstar[pred_data$Y == 1 & pred_data$fold == k][1]
    target_data$F1n[target_data$fold == k] <- pred_data$Fn[pred_data$Y == 1 & pred_data$fold == k][1]
  }

  target_data$DY_cvtmle <- with(target_data, Fnstar - (gn*F1nstar + (1 - gn)*F0nstar))
  target_data$Dpsi_cvtmle <- with(target_data, weight * (ind - Fnstar))
  
  target_data$DY_os <- with(target_data, Fn - (gn*F1n + (1 - gn)*F0n))
  target_data$Dpsi_os <- with(target_data, weight * (ind - Fn))

  # cvtmle estimates
  est_cvtmle <- mean(cvtmle_estimates)
  se_cvtmle <- sqrt(var(target_data$DY_cvtmle + target_data$Dpsi_cvtmle) / n)
  # initial estimate
  est_init <- mean(unlist(init_estimates))
  est_onestep <- est_init + mean(target_data$DY_os + target_data$Dpsi_os)
  se_onestep <- sqrt(var(target_data$DY_os + target_data$Dpsi_os) / n)

  # get CV estimator
  cv_empirical_estimates <- .get_cv_estim(prediction_list[1:K], sens = sens, 
                                            gn = mean(Y), quantile_type = quantile_type)

  # sample split estimate
  est_empirical <- mean(unlist(lapply(cv_empirical_estimates, "[[", "est")))
  var_empirical <- mean(unlist(lapply(cv_empirical_estimates, function(x){
    var(x$ic)
  })))
  ic_empirical <- Reduce(c, lapply(cv_empirical_estimates, "[[", "ic"))
  se_empirical <- sqrt(var_empirical / n)
  
  # format output
  out <- list()
  out$est_cvtmle <- est_cvtmle
  # out$iter_cvtmle <- iter
  # out$cvtmle_trace <- tmle_auc
  out$se_cvtmle <- se_cvtmle
  out$est_init <- est_init
  out$est_empirical <- est_empirical
  out$se_empirical <- se_empirical
  out$est_onestep <- est_onestep
  out$se_onestep <- se_onestep
  out$est_esteq <- est_onestep
  out$se_esteq <- se_onestep
  out$se_cvtmle_type <- out$se_esteq_type <- 
    out$se_empirical_type <- out$se_onestep_type <- "std"
  out$ic_cvtmle <- target_data$DY_cvtmle + target_data$Dpsi_cvtmle
  out$ic_onestep <- target_data$DY_os + target_data$Dpsi_os
  out$ic_empirical <- ic_empirical
  out$prediction_list <- prediction_list
  class(out) <- "scrnp"
  return(out)
}

#' Helper function to get results for a single cross-validation fold
#' @param x An entry in prediction_list.
#' @param sens The sensitivity constraint.
#' @param gn An estimate of the marginal probability that \code{Y = 1}.
#' @param quantile_type The type of quantile estimate to use.
#' @param ... Other options (not currently used)
#' @importFrom stats quantile
.get_one_fold <- function(x, sens, gn, quantile_type = 8, ...){
  # get quantile 
  c0 <- stats::quantile(x$test_pred[x$test_y == 1], p = 1 - sens, type = quantile_type)
  # get influence function
  F1nc0 <- mean(x$test_pred[x$test_y == 1] <= c0)
  F0nc0 <- mean(x$test_pred[x$test_y == 0] <= c0)
  FYnc0 <- ifelse(x$test_y == 1, F1nc0, F0nc0)
  Psi <- gn * F1nc0 + (1-gn) * F0nc0
  DY <- FYnc0 - Psi
  # get density estimate
  dens <- tryCatch({.get_density(x = x, c0 = c0, 
                      bounded_kernel = FALSE,
                      x_name = "test_pred", 
                      y_name = "test_y",
                      nested_cv = FALSE, prediction_list = NULL, 
                      folds = NULL)}, error = function(e){
    list(f_0_c0 = 1, f_10_c0 = 1)
  })
  weight <- (1 - x$test_y / gn * dens$f_0_c0/dens$f_10_c0)
  ind <- as.numeric(x$test_pred <= c0)
  Dpsi <- weight * (ind - FYnc0)

  return(list(est = Psi, ic = DY + Dpsi))
}

#' Helper function to turn prediction_list into CV estimate of SCRNP
#' @param prediction_list Properly formatted list of predictions.
#' @param sens The sensitivity constraint.
#' @param gn The marginal probability that \code{Y = 1}.
#' @param quantile_type The type of quantile estimate to use.
#' @param ... Other options (not currently used)
.get_cv_estim <- function(prediction_list, sens, gn, quantile_type = 8, ...){
  allFolds <- lapply(prediction_list, .get_one_fold, sens = sens, gn = gn,
                     quantile_type = quantile_type)
  return(allFolds)
}

#' Helper function to get quantile for a single training fold data 
#' when nested CV is NOT used. 
#' @param x An entry in prediction_list.
#' @param p The quantile to get.
#' @param quantile_type The type of quantile estimate to use.
#' @importFrom stats quantile 
.get_quantile <- function(x, p, quantile_type = 8){
  stats::quantile(x$train_pred[x$train_y == 1], p = p, type = quantile_type)
}

#' Helper function to get quantile for a single training fold data 
#' when nested CV is used. 
#' @param x An entry in prediction_list.
#' @param p The quantile to get.
#' @param prediction_list Properly formatted list of predictions.
#' @param folds Cross-validation fold assignments. 
#' @param quantile_type The type of quantile estimate to use.
#' @importFrom stats quantile 
.get_nested_cv_quantile <- function(x, p, prediction_list, folds,
                                 quantile_type = 8){
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

  # now get all values of psi from inner validation with Y = 1
  train_psi_1 <- unlist(lapply(inner_valid_prediction_and_y_list, function(z){
    z$test_pred[z$inner_test_y == 1]    
  }), use.names = FALSE)

  # get the quantile
  c0 <- quantile(train_psi_1, p = p, type = quantile_type)

  return(c0)
}

#' Function to estimate density needed to evaluate standard errors.
#' @param x An entry in prediction_list.
#' @param c0 The point at which the density estimate is evaluated.
#' @param bounded_kernel Should a bounded kernel be used? Default is \code{FALSE}.
#' @param x_name Name of variable to compute density of.
#' @param y_name Name of variable to stratify density computation on.
#' @param nested_cv Use nested CV to estimate density?
#' @param prediction_list Properly formatted list of predictions.
#' @param folds Cross-validation fold assignments.
#' @param maxDens The maximum allowed value for the density. 
#' @param ... Other options (not currently used)
#' @importFrom np npudensbw npudens
#' @importFrom stats predict
#' @importFrom bde bde
.get_density <- function(x, c0, bounded_kernel = FALSE,
                        x_name = "train_pred", 
                        y_name = "train_y",
                        nested_cv = FALSE, prediction_list = NULL, 
                        folds = NULL, maxDens = 1e3, ... ){
  if(!nested_cv){
    if(!bounded_kernel){
      if(length(unique(x[[x_name]][x$train_y == 1])) > 1){
        # density given y = 1
        fitbw <- np::npudensbw(x[[x_name]][x[[y_name]] == 1])
        fit <- np::npudens(fitbw)
        # estimate at c0
        f_10_c0 <- stats::predict(fit, edat = c0)
      }else{
        f_10_c0 <- maxDens
      }
      if(length(unique(x[[x_name]])) > 1){
        # marginal density
        fitbw_marg <- np::npudensbw(x[[x_name]])
        fit_marg <- np::npudens(fitbw_marg)
        # estimate at c0
        f_0_c0 <- stats::predict(fit_marg, edat = c0)
      }else{
        f_0_c0 <- maxDens
      }
    }else{
      # density given Y = 1
      fit_1 <- bde::bde(dataPoints = x[[x_name]][x$train_y == 1],
                      dataPointsCache = c0,
                      estimator = "betakernel")
      f_10_c0 <- fit_1@densityCache
      fit_all <- bde::bde(dataPoints = x[[x_name]],
                      dataPointsCache = c0,
                      estimator = "betakernel")
      f_0_c0 <- fit_all@densityCache
    }
  }else{
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
    all_pred <- Reduce("c", lapply(inner_valid_prediction_and_y_list, "[[",
                                   "test_pred"))
    all_y <- Reduce("c", lapply(inner_valid_prediction_and_y_list, "[[",
                                   "inner_test_y"))
    if(bounded_kernel){
      # TO DO: Implement CV bandwidth selection for bounded kernels
      # density given Y = 1
      fit_1 <- bde::bde(dataPoints = all_pred[all_y == 1],
                      dataPointsCache = c0,
                      estimator = "betakernel")
      f_10_c0 <- fit_1@densityCache
      fit_all <- bde::bde(dataPoints = all_pred,
                      dataPointsCache = c0,
                      estimator = "betakernel")
      f_0_c0 <- fit_all@densityCache
    }else{
      if(length(unique(all_pred[all_y == 1])) > 1){
        # density given y = 1
        fitbw <- np::npudensbw(all_pred[all_y == 1])
        fit <- np::npudens(fitbw)
        # estimate at c0
        f_10_c0 <- stats::predict(fit, edat = c0)
      }else{
        f_10_c0 <- maxDens
      }
      if(length(unique(all_pred[all_y == 1])) > 1){
        # marginal density
        fitbw_marg <- np::npudensbw(all_pred)
        fit_marg <- np::npudens(fitbw_marg)
        # estimate at c0
        f_0_c0 <- stats::predict(fit_marg, edat = c0)
      }else{
        f_0_c0 <- maxDens
      }
    }
  }
  # return both
  return(list(f_10_c0 = f_10_c0, f_0_c0 = f_0_c0))
}


#' Helper function for making data set in proper format for CVTMLE
#' 
#' @param x A numeric identifier of which entry in \code{prediction_list} to operate on.
#' @param prediction_list Properly formatted list of predictions.
#' @param quantile_list List of estimated quantile for each fold.
#' @param density_list List of density estimates for each fold.
#' @param folds Cross-validation fold assignments.
#' @param nested_cv A boolean indicating whether nested CV was used in estimation.
#' @param gn An estimate of the marginal probability that \code{Y = 1}. 
.make_targeting_data <- function(x, prediction_list, quantile_list, 
                               density_list, folds,
                               nested_cv = FALSE, gn){
  K <- length(folds)
  if(!nested_cv){
    Y_vec <- Reduce(c, lapply(prediction_list, "[[", "test_y"))
    Y_vec_pred <- rep(c(0,1), K)
    n <- length(Y_vec)
    fold_vec <- sort(rep(seq_len(K), unlist(lapply(folds, length))))
    fold_vec_pred <- sort(rep(seq_len(K), 2))
    gn_vec <- gn
    F_nBn_vec <- Reduce(c, mapply(FUN = function(m, c0){
      F_nBn_y1_at_c0 <- F_nBn_star(psi_x = c0, y = 1, train_pred = m$train_pred, train_y = m$train_y)
      F_nBn_y0_at_c0 <- F_nBn_star(psi_x = c0, y = 0, train_pred = m$train_pred, train_y = m$train_y)
      ifelse(m$test_y == 0, F_nBn_y0_at_c0, F_nBn_y1_at_c0)
    }, c0 = quantile_list, m = prediction_list))
    F_nBn_vec_pred <- Reduce(c, mapply(FUN = function(m, c0){
      F_nBn_y1_at_c0 <- F_nBn_star(psi_x = c0, y = 1, train_pred = m$train_pred, train_y = m$train_y)
      F_nBn_y0_at_c0 <- F_nBn_star(psi_x = c0, y = 0, train_pred = m$train_pred, train_y = m$train_y)
      c(F_nBn_y0_at_c0, F_nBn_y1_at_c0)
    }, c0 = quantile_list, m = prediction_list))

    dens_ratio <- Reduce(c, mapply(FUN = function(m, dens){
      if(dens[[1]] == 0){ dens[[1]] <- 1e-3 }
      if(dens[[2]] > 1e3){ dens[[2]] <- 1e3 }
      rep(dens[[2]]/dens[[1]], length(m$test_y))
    }, m = prediction_list, dens = density_list))
    dens_ratio_pred <- Reduce(c, mapply(FUN = function(m, dens){
      if(dens[[1]] == 0){ dens[[1]] <- 1e-3 }
      if(dens[[2]] > 1e3){ dens[[2]] <- 1e3 }
      rep(dens[[2]]/dens[[1]], 2)
    }, m = prediction_list, dens = density_list))
    ind <- Reduce(c, mapply(m = prediction_list, c0 = quantile_list, function(m, c0){
      as.numeric(m$test_pred <= c0)
    }))
    ind_pred <- c(NA, NA)
  }else{
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
    
    Y_vec <- Reduce(c, lapply(prediction_list[x], "[[", "test_y"))
    Y_vec_pred <- c(0,1)    
    fold_vec <- rep(x, length(Y_vec))
    fold_vec_pred <- rep(x, length(Y_vec_pred))
    gn_vec <- gn
    n <- length(Y_vec)
    F_nBn_y1_at_c0 <- F_nBn_star_nested_cv(psi_x = quantile_list[[x]], y = 1, 
                                           inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)
    F_nBn_y0_at_c0 <- F_nBn_star_nested_cv(psi_x = quantile_list[[x]], y = 0, 
                                           inner_valid_prediction_and_y_list = inner_valid_prediction_and_y_list)
    F_nBn_vec <- ifelse(prediction_list[[x]]$test_y == 0, F_nBn_y0_at_c0, F_nBn_y1_at_c0)
    F_nBn_vec_pred <- c(F_nBn_y0_at_c0, F_nBn_y1_at_c0)
    if(density_list[[x]][[1]] == 0){ density_list[[x]][[1]] <- 1e-3 }
    if(density_list[[x]][[2]] > 1e3){ density_list[[x]][[2]] <- 1e3 }
    dens_ratio <- density_list[[x]][[2]]/density_list[[x]][[1]]
    dens_ratio_pred <- density_list[[x]][[2]]/density_list[[x]][[1]]
    ind <- as.numeric(prediction_list[[x]]$test_pred <= quantile_list[[x]])
  }

  out <- data.frame(fold = fold_vec, Y = Y_vec, gn = gn_vec, Fn = F_nBn_vec,
                    f_ratio = dens_ratio, ind = ind)
  out_pred <- data.frame(fold = fold_vec_pred, Y = Y_vec_pred, gn = gn_vec, 
                         Fn = F_nBn_vec_pred, f_ratio = dens_ratio_pred)
  return(list(out = out, out_pred = out_pred))
}

#' Compute the bootstrap-corrected estimator of SCRNP.
#' 
#' This estimator is computed by re-sampling with replacement (i.e., bootstrap
#' sampling) from the data. The SCRNP is computed for the learner trained on the 
#' full data. The SCRNP is then computed for the learner trained on each bootstrap
#' sample. The average difference between the full data-trained learner and 
#' the bootstrap-trained learner is computed to estimate the bias in the full-data-estimated
#' SCRNP. The final estimate of SCRNP is given by the difference in the full-data SCRNP 
#' and the estimated bias.
#' 
#' @param Y A numeric vector of outcomes, assume to equal \code{0} or \code{1}.
#' @param X A \code{data.frame} of variables for prediction.
#' @param B The number of bootstrap samples. 
#' @param learner A wrapper that implements the desired method for building a 
#' prediction algorithm. See \code{?glm_wrapper} or read the package vignette
#' for more information on formatting \code{learner}s.
#' @param sens The sensitivity constraint to use. 
#' @param correct632 A boolean indicating whether to use the .632 correction.
#' @param ... Other options, not currently used. 
#' @return A list with \code{$scrnp} the bootstrap-corrected estimate of SCRNP and
#' \code{$n_valid_boot} as the number of bootstrap of bootstrap samples where \code{learner} 
#' successfully executed.
#' @export
#' @examples 
#' # simulate data
#' X <- data.frame(x1 = rnorm(50))
#' Y <- rbinom(50, 1, plogis(X$x1))
#' # compute bootstrap estimate of scrnp for logistic regression
#' # use small B for fast run
#' boot <- boot_scrnp(Y = Y, X = X, B = 25, learner = "glm_wrapper")
#' @export
boot_scrnp <- function(Y, X, B = 200, learner = "glm_wrapper", 
                       sens = 0.95, correct632 = FALSE, ...){
  n <- length(Y)
  full_fit <- do.call(learner, args=list(train = list(Y = Y, X = X),
                                      test = list(Y = Y, X = X)))
  full_c0 <- quantile(full_fit$test_pred[full_fit$train_y == 1], p = 1 - sens)
  full_est <- mean(full_fit$test_pred <= full_c0)

  all_boot <- replicate(B, one_boot_scrnp(Y = Y, X = X, n = n, 
                                          correct632 = correct632,
                                          learner = learner, 
                                          sens = sens))
  
  if(!correct632){  
    mean_optimism <- mean(all_boot, na.rm = TRUE)
    corrected_est <- full_est - mean_optimism
  }else{
    scrnp_b <- mean(all_boot, na.rm = TRUE)
    # first copy each prediction n times
    long_pred <- rep(full_fit$test_pred, each = n)
    # now copy observed outcomes n times 
    long_y <- rep(Y, n)
    # now compute quantile
    long_c0 <- quantile(long_pred[long_y == 1], p = 1 - sens, na.rm = TRUE)
    g <- mean(long_pred <= long_c0, na.rm = TRUE)
    # relative overfitting rate
    R <- (scrnp_b - full_est)/(g - full_est)
    # 632 weight
    w <- 0.632 / (1 - 0.368*R)
    # weighted estimate
    corrected_est <- (1 - w)*full_est + w * scrnp_b
  }
  return(list(scrnp = corrected_est, n_valid_boot = sum(!is.na(all_boot))))
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
#' @param sens The sensitivity constraint to use. 
#' @param n Number of observations
#' @return If \code{learner} executes successfully, a numeric estimate of AUC
#' on this bootstrap sample. Otherwise the function returns \code{NA}.
one_boot_scrnp <- function(Y, X, n, correct632, learner, sens){
  idx <- sample(seq_len(n), replace = TRUE)
  train_Y <- Y[idx]
  train_X <- X[idx, , drop = FALSE]
  fit <- tryCatch({
    do.call(learner, args=list(train = list(Y = train_Y, X = train_X),
                               test = list(Y = Y, X = X)))
  }, error = function(e){
    return(NA)
  })
  if(!(class(fit) == "logical")){
    if(!correct632){
      train_c0 <- stats::quantile(fit$train_pred[fit$train_y == 1], p = 1 - sens)
      test_c0 <- stats::quantile(fit$test_pred[fit$test_y == 1], p = 1 - sens)
      train_est <- mean(fit$train_pred <= train_c0)
      test_est <- mean(fit$test_pred <= test_c0)
      out <- train_est - test_est
    }else{
      oob_idx <- which(!(1:n %in% idx))
      oob_c0 <- stats::quantile(fit$test_pred[oob_idx][fit$train_y[oob_idx] == 1], 
                                p = 1 - sens)
      out <- mean(fit$test_pred[oob_idx] <= oob_c0)
    }
    return(out)
  }else{
    return(NA)
  }
}
