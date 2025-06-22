#' Impute Missing Values Using LOCF and NOCB
#'
#' Imputes missing values in a matrix by applying Last Observation Carried Forward (LOCF) followed by 
#' Next Observation Carried Backward (NOCB) for each subject.
#'
#' @param X A matrix where rows represent observations and columns represent variables.
#' @param subject_id A vector of subject IDs corresponding to the rows of \code{X}.
#'
#' @return A matrix with missing values imputed using LOCF and NOCB.
#'
#' @examples
#' X <- matrix(c(NA, 2, NA, 4, 5, NA, 7, 8, NA, NA), nrow = 5, byrow = TRUE)
#' subject_id <- c(1, 1, 1, 2, 2)
#' apply_locf_nocb(X, subject_id)
#'
#' @export
apply_locf_nocb <- function(X, subject_id) {
  locf <- function(x) {
    # Fill missing values by carrying forward the last non-missing value
    for (i in 2:length(x)) {
      if (is.na(x[i])) {
        x[i] <- x[i - 1]
      }
    }
    return(x)
  }
  
  nocb <- function(x) {
    # Fill missing values by carrying backward the next non-missing value
    for (i in (length(x) - 1):1) {
      if (is.na(x[i])) {
        x[i] <- x[i + 1]
      }
    }
    return(x)
  }
  
  apply_locf <- function(X, subject_id) {
    unique_ids <- unique(subject_id)
    X_imputed <- as.matrix(X)  # Copy of the matrix to store imputed values
    
    # Loop over each individual
    for (id in unique_ids) {
      # Get the rows corresponding to the current individual
      rows <- which(subject_id == id)
      
      # Apply LOCF for each covariate (column) for this individual
      for (col in 1:ncol(X)) {
        X_imputed[rows, col] <- locf(X[rows, col])
      }
    }
    
    return(X_imputed)
  }
  
  apply_nocb <- function(X, subject_id) {
    unique_ids <- unique(subject_id)
    X_imputed <- as.matrix(X)  # Copy of the matrix to store imputed values
    
    # Loop over each individual
    for (id in unique_ids) {
      # Get the rows corresponding to the current individual
      rows <- which(subject_id == id)
      
      # Apply NOCB for each covariate (column) for this individual
      for (col in 1:ncol(X)) {
        X_imputed[rows, col] <- nocb(X[rows, col])
      }
    }
    
    return(X_imputed)
  }
  
  X_locf <- apply_locf(X, subject_id)  # Apply LOCF first
  X_locf_nocb <- apply_nocb(X_locf, subject_id)  # Then apply NOCB
  return(X_locf_nocb)
}



#' @title Sequential Imputation for Missing Data
#' @description Implements sequential imputation for missing covariates and outcomes in longitudinal data. 
#' The function uses a Bayesian non-parametric framework with mixed-effects models to handle both 
#' normal and non-normal random effects and errors. It sequentially imputes missing values by constructing 
#' univariate models in a fixed order, ensuring simplicity and consistency with a valid joint distribution.
#'
#' @param X A matrix of missing covariates.
#' @param Y A vector of missing outcomes (numeric or logical).
#' @param Z A matrix of complete random predictors.
#' @param subject_id A vector of subject IDs corresponding to the rows of \code{X} and \code{Y}. Can be both integer or character
#' @param type A logical vector indicating whether each covariate in \code{X} is binary (1) or continuous (0).
#' @param binary_outcome A logical value indicating whether the outcome \code{Y} is binary (1) or continuous (0). Default: \code{0}.
#' @param model A character vector specifying the imputation model. Options are \code{"BMTrees"}, 
#' \code{"BMTrees_R"}, \code{"BMTrees_RE"}, and \code{"mixedBART"}. Default: \code{"BMTrees"}.
#' @param nburn An integer specifying the number of burn-in iterations. Default: \code{0}.
#' @param npost An integer specifying the number of sampling iterations. Default: \code{3}.
#' @param skip An integer specifying the interval for keeping samples in the sampling phase. Default: \code{1}.
#' @param verbose A logical value indicating whether to display progress and MCMC information. Default: \code{TRUE}.
#' @param seed A random seed for reproducibility. Default: \code{NULL}.
#' @param tol A small numerical tolerance to prevent numerical overflow or underflow in the model. Default: \code{1e-20}.
#' @param resample An integer specifying the number of resampling steps for the CDP prior. Default: \code{5}. This parameter is only valid for \code{"BMTrees"} and \code{"BMTrees_R"}.
#' @param ntrees An integer specifying the number of trees in BART. Default: \code{200}.
#' @param reordering A logical value indicating whether to apply a reordering strategy for sorting covariates. Default: \code{TRUE}.
#' @param pi_CDP A value between 0 and 1 for calculating the empirical prior in the CDP prior. Default: \code{0.99}.
#'
#' @return A three-dimensional array of imputed data with dimensions \code{(npost / skip, N, p + 1)}, where:
#' - \code{N} is the number of observations.
#' - \code{p} is the number of covariates in \code{X}.
#' The array includes imputed covariates and outcomes.
#'
#' @details The function builds on the Bayesian Trees Mixed-Effects Model (BMTrees), which extends Mixed-Effects 
#' BART by using centralized Dirichlet Process (CDP) Normal Mixture priors. This framework handles non-normal 
#' random effects and errors, addresses model misspecification, and captures complex relationships. The function 
#' employs a Metropolis-Hastings MCMC method to sequentially impute missing values.
#'
#' @examples
#' \donttest{
#' data <- simulation_imputation(n_subject = 100, seed = 1234, nonrandeff = TRUE, 
#'         nonresidual = TRUE, alligned = FALSE) 
#' 
#' # To make it faster to compile and check, we only run 30 iterations for burn-in 
#' # and 40 for posterior sampling phases.
#' # Please increase to 3000 and 4000 iterations, respectively, when running the model.
#' model <- sequential_imputation(data$X_mis, data$Y_mis, data$Z, data$subject_id, 
#'         rep(0, 9), binary_outcome = FALSE, model = "BMTrees", nburn = 30L, 
#'         npost = 40L, skip = 2L, verbose = TRUE, seed = 1234)
#' model$imputed_data
#' }
#' @rdname sequential_imputation
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
sequential_imputation <- function(X, Y,  Z = NULL, subject_id, type, binary_outcome = FALSE, model = c("BMTrees", "BMTrees_R", "BMTrees_RE", "mixedBART"), nburn = 0L, npost = 3L, skip = 1L, verbose = TRUE, seed = NULL, tol = 1e-20, resample = 5, ntrees = 200, reordering = TRUE, pi_CDP = 0.99) {
  model = match.arg(model)
  if(is.null(dim(X))){
    stop("More than one covariate is needed!")
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  omit_sub = sapply(unique(subject_id), function(sub){
    t = sum(subject_id == sub)
    X_sub = cbind(X[subject_id == sub,], Y[subject_id == sub])
    if (sum(colSums(is.na(X_sub)) == t) > 0){
      return(sub)
    }
    return(NA)
  })
  omit_sub = omit_sub[!is.na(omit_sub)]
  if(length(omit_sub) > 0){
    warning(paste0("Some variables of ", length(omit_sub), " subjects are missing at all time points, we delete data from these ", length(omit_sub), " subjects:\n", paste(omit_sub, collapse = ", ")))
    X = X[!subject_id %in% omit_sub,]
    Y = Y[!subject_id %in% omit_sub]
    if (!is.null(Z)){
      Z = Z[!subject_id %in% omit_sub,]
    }
    subject_id = subject_id[!subject_id %in% omit_sub]
  }
  trajectory = c(sapply(table(subject_id), function(i) 1:i))
  subject_id = as.factor(subject_id)
  p = dim(X)[2]
  
  mis_num = colSums(is.na(X))
  if(reordering == TRUE){
    mis_num[mis_num ==0] = -Inf
    mis_num[mis_num > 0 & type == 1] = mis_num[mis_num > 0 & type == 1] - base::max(mis_num)
    mis_order = order(mis_num)
    re_order = seq(ncol(is.na(X)))
    type = type[mis_order]
    X[,re_order] = X[,mis_order]
    colnames(X) = colnames(X)[mis_order]
    message("reordering: new covariates order is ", paste(colnames(X), collape = " "), "\n")
  }
  

  R = is.na(X)
  R = cbind(R, "Y" = is.na(Y))
  message("Start to initialize imputed missing data by LOCF and NOCB. ")
  X_locf_nocb <- apply_locf_nocb(cbind(X, Y), subject_id)
  Y = X_locf_nocb[,dim(X_locf_nocb)[2]]
  X = X_locf_nocb[,1:(dim(X_locf_nocb)[2] - 1)]

  
  if(sum(mis_num == -Inf) == 0){
    X = cbind("intercept" = 1, X)
    type = c(0, type)
    R = cbind(0, R)
  }
  
  message("Completed.\n")
 
  
  message("Start to impute using Longitudinal Sequential Imputation with: ")
 
  if(model == "BMTrees_R"){
    message("BMTrees_R\n")
    imputation_X_DP = sequential_imputation_cpp(as.matrix(X), as.numeric(Y), as.logical(type), as.matrix(Z), as.character(subject_id), as.matrix(R), binary_outcome = binary_outcome, nburn = nburn, npost = npost, skip = skip, verbose = verbose, CDP_residual = TRUE, CDP_re = FALSE, seed = seed, ncores = 0, ntrees = ntrees, fit_loss = FALSE, resample = resample, pi_CDP = pi_CDP)
  }
  else if(model == "BMTrees_RE"){
    message("BMTrees_RE\n")
    imputation_X_DP = sequential_imputation_cpp(as.matrix(X), as.numeric(Y), as.logical(type), as.matrix(Z), as.character(subject_id), as.matrix(R), binary_outcome = binary_outcome, nburn = nburn, npost = npost, skip = skip, verbose = verbose, CDP_residual = FALSE, CDP_re = TRUE, seed = seed, ncores = 0, ntrees = ntrees, fit_loss = FALSE, resample = resample, pi_CDP = pi_CDP)
  }
  else if(model == "BMTrees"){
    message("BMTrees\n")
    imputation_X_DP = sequential_imputation_cpp(as.matrix(X), as.numeric(Y), as.logical(type), as.matrix(Z), as.character(subject_id), as.matrix(R), binary_outcome = binary_outcome, nburn = nburn, npost = npost, skip = skip, verbose = verbose, CDP_residual = TRUE, CDP_re = TRUE, seed = seed, ncores = 0, ntrees = ntrees, fit_loss = FALSE, resample = resample, pi_CDP = pi_CDP)
  }
  else if(model == "mixedBART"){
    message("mixedBART\n")
    imputation_X_DP = sequential_imputation_cpp(as.matrix(X), as.numeric(Y), as.logical(type), as.matrix(Z), as.character(subject_id), as.matrix(R), binary_outcome = binary_outcome, nburn = nburn, npost = npost, skip = skip, verbose = verbose, CDP_residual = FALSE, CDP_re = FALSE, seed = seed, ncores = 0,  ntrees = ntrees, fit_loss = FALSE, resample = resample, pi_CDP = pi_CDP)
  }
  else{
    message("mixedBART\n")
    imputation_X_DP = sequential_imputation_cpp(as.matrix(X), as.numeric(Y), as.logical(type), as.matrix(Z), as.character(subject_id), as.matrix(R), binary_outcome = binary_outcome, nburn = nburn, npost = npost, skip = skip, verbose = verbose, CDP_residual = TRUE, CDP_re = TRUE, seed = seed, ncores = 0, ntrees = ntrees, fit_loss = FALSE, resample = resample, pi_CDP = pi_CDP)
  }
  
  imputation_Y = t(do.call(cbind, imputation_X_DP$imputation_Y_DP))
  if(reordering == TRUE){
    imputation_X_DP = lapply(imputation_X_DP$imputation_X_DP, function(x){
      if(sum(mis_num == -Inf) == 0){
        x = x[,-1]
      }
      x[,mis_order] = x[,re_order]
      colnames(x)[mis_order] = colnames(x)[re_order]
      x
    })
  }else{
    imputation_X_DP = lapply(imputation_X_DP$imputation_X_DP, function(x){
      if(sum(mis_num == -Inf) == 0){
        x = x[,-1]
      }
      x
    })
  }
  
  imputation_X = array(NA, dim = c(length(imputation_X_DP), dim(imputation_X_DP[[1]])[1], dim(imputation_X_DP[[1]])[2] + 1))
  for (i in 1:length(imputation_X_DP)) {
    imputation_X[i,,] = cbind(imputation_X_DP[[i]], imputation_Y[i,])
  }
  message("\n")
  message("Finish imputation with ", length(imputation_X_DP), " imputed sets\n")
  return(list(imputed_data = imputation_X))
}
