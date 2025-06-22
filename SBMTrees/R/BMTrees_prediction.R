#' @title Bayesian Trees Mixed-Effects Models for Predicting Longitudinal Outcomes
#'
#' @description Provides predictions for outcomes in longitudinal data using Bayesian Trees 
#' Mixed-Effects Models (BMTrees) and its semiparametric variants. The function predicts values for test 
#' data while accounting for random effects, complex relationships, and potential model misspecification.
#'
#' @param X_train A matrix of covariates in the training set.
#' @param Y_train A numeric or logical vector of outcomes in the training set.
#' @param Z_train A matrix of random predictors in the training set.
#' @param subject_id_train A character vector of subject IDs in the training set.
#' @param X_test A matrix of covariates in the testing set.
#' @param Z_test A matrix of random predictors in the testing set.
#' @param subject_id_test A character vector of subject IDs in the testing set.
#' @param model A character string specifying the predictive model. Options are \code{"BMTrees"}, 
#' \code{"BMTrees_R"}, \code{"BMTrees_RE"}, and \code{"mixedBART"}. Default: \code{"BMTrees"}.
#' @param binary Logical. Indicates whether the outcome is binary (\code{TRUE}) or continuous (\code{FALSE}). 
#' Default: \code{FALSE}.
#' @param nburn An integer specifying the number of burn-in iterations for Gibbs sampler. 
#' Default: \code{3000L}.
#' @param npost An integer specifying the number of posterior samples to collect. Default: \code{4000L}.
#' @param skip An integer indicating the thinning interval for MCMC samples. Default: \code{1L}.
#' @param verbose Logical. If \code{TRUE}, displays MCMC progress. If \code{FALSE}, shows a progress bar. 
#' Default: \code{TRUE}.
#' @param seed An optional integer for setting the random seed to ensure reproducibility. Default: \code{NULL}.
#' @param tol A numeric tolerance value to prevent numerical overflow and underflow in the model. Default: \code{1e-20}.
#' @param resample  An integer specifying the number of resampling steps for the CDP prior. Default: \code{5}. This parameter is only valid for \code{"BMTrees"} and \code{"BMTrees_R"}.
#' @param ntrees An integer specifying the number of trees in BART. Default: \code{200}.
#' @param pi_CDP A value between 0 and 1 for calculating the empirical prior in the CDP prior. Default: \code{0.99}.
#'
#' @return A list containing posterior samples and predictions:
#' \describe{
#'   \item{post_tree_train}{Posterior samples of the fixed-effects from BART on training data.}
#'   \item{post_Sigma}{Posterior samples of covariance matrices in random effects.}
#'   \item{post_lambda_F}{Posterior samples of lambda parameter in CDP normal mixture on random errors.}
#'   \item{post_lambda_G}{Posterior samples of lambda parameter in CDP normal mixture on random-effects.}
#'   \item{post_B}{Posterior samples of the coefficients in random effects.}
#'   \item{post_random_effect_train}{Posterior samples of random effects for training data.}
#'   \item{post_sigma}{Posterior samples of error deviation.}
#'   \item{post_expectation_y_train}{Posterior expectations of training data outcomes, equal to fixed-effects + random effects.}
#'   \item{post_expectation_y_test}{Posterior expectations of testing data outcomes, equal to fixed-effects + random effects.}
#'   \item{post_predictive_y_train}{Posterior predictive distributions for training outcomes, equal to fixed-effects + random effects + predictive residual.}
#'   \item{post_predictive_y_test}{Posterior predictive distributions for testing outcomes, equal to fixed-effects + random effects + predictive residual.}
#'   \item{post_eta}{Posterior samples of location parameters in CDP normal mixture on random errors.}
#'   \item{post_mu}{Posterior samples of location parameters in CDP normal mixture on random effects.}
#' }
#'
#'
#' @examples
#' \donttest{
#' data = simulation_prediction(n_subject = 100, seed = 1234, nonlinear = TRUE, 
#' nonrandeff = TRUE, nonresidual = TRUE) 
#'
#' # To make it faster to compile and check, we only run 30 iterations for burn-in 
#' # and 40 for posterior sampling phases.
#' # Please increase to 3000 and 4000 iterations, respectively, when running the model.
#' model = BMTrees_prediction(data$X_train, data$Y_train, data$Z_train, 
#' data$subject_id_train, data$X_test, data$Z_test, data$subject_id_test, model = "BMTrees", 
#' binary = FALSE, nburn = 30L, npost = 40L, skip = 1L, verbose = TRUE, seed = 1234)
#' model$post_predictive_y_test
#' model$post_sigma
#' }
#' @rdname BMTrees_prediction
#' @note
#' This function utilizes modified C++ code originally derived from the 
#' BART3 package (Bayesian Additive Regression Trees). The original package 
#' was developed by Rodney Sparapani and is licensed 
#' under GPL-2. Modifications were made by Jungang Zou, 2024.
#' @references
#' For more information about the original BART3 package, see:
#' https://github.com/rsparapa/bnptools/tree/master/BART3
#' @export
#' @useDynLib SBMTrees, .registration = TRUE
#' @importFrom Rcpp sourceCpp

BMTrees_prediction = function(X_train, Y_train, Z_train, subject_id_train, X_test, Z_test, subject_id_test, model = c("BMTrees", "BMTrees_R", "BMTrees_RE", "mixedBART"), binary = FALSE, nburn = 3000L, npost = 4000L, skip = 1L, verbose = TRUE, seed = NULL, tol = 1e-20, resample = 5, ntrees = 200, pi_CDP = 0.99){
  if(!is.null(seed))
    set.seed(seed)
  n_train = dim(X_train)[1]
  n_test = dim(X_test)[1]
  
  X = rbind(X_train, X_test)
  Y = c(Y_train, rep(0, n_test))
  Z = rbind(Z_train, Z_test)
  subject_id = c(subject_id_train, subject_id_test)
  obs_ind = c(rep(TRUE, n_train), rep(FALSE, n_test))
  if(model == "BMTrees")
    model = BMTrees_mcmc(X, Y, Z, subject_id, obs_ind, binary, nburn, npost, verbose, TRUE, TRUE, seed, tol, ntrees, resample, pi_CDP)
  else if(model == "BMTrees_R")
    model = BMTrees_mcmc(X, Y, Z, subject_id, obs_ind, binary, nburn, npost, verbose, TRUE, FALSE, seed, tol, ntrees, resample, pi_CDP)
  else if(model == "BMTrees_RE")
    model = BMTrees_mcmc(X, Y, Z, subject_id, obs_ind, binary, nburn, npost, verbose, FALSE, TRUE, seed, tol, ntrees, resample, pi_CDP)
  else if(model == "mixedBART")
    model = BMTrees_mcmc(X, Y, Z, subject_id, obs_ind, binary, nburn, npost, verbose, FALSE, FALSE, seed, tol, ntrees, resample, pi_CDP)
  else
    model = BMTrees_mcmc(X, Y, Z, subject_id, obs_ind, binary, nburn, npost, verbose, TRUE, TRUE, seed, tol, ntrees, resample, pi_CDP)
  return(list(post_tree_train = model$post_x_hat, post_Sigma = model$post_Sigma, post_lambda_F = model$post_lambda, post_lambda_G = model$post_B_lambda, post_B = model$post_B, post_random_effect_train = model$post_random_effect, post_sigma = model$post_sigma, post_expectation_y_train = model$post_y_expectation, post_expectation_y_test = model$post_y_expectation_test, post_predictive_y_train = model$post_y_sample, post_predictive_y_test = model$post_y_sample_test, post_eta = model$post_tau_samples, post_mu = model$post_B_tau_samples))
}
