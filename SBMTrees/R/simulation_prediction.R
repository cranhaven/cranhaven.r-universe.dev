#' @title Simulate Longitudinal Data for Prediction
#' @description Generates a fixed population longitudinal dataset, with random seeds to generate different 
#' training and testing sets. The function supports customization of linear/nonlinear associations, 
#' normal/non-normal random effects, and random errors. It splits the data into training and testing sets, 
#' with the testing set comprising approximately 40% of the data.
#'
#' @param n_subject Number of subjects in the dataset. Each subject has multiple observations across 6 follow-up time points. Default: \code{800}.
#' @param seed Random seed for reproducibility. Ensures different training-testing splits. Default: \code{123}.
#' @param nonlinear Logical value indicating whether the outcome model includes nonlinear associations. Default: \code{FALSE}.
#' @param nonrandeff Logical value indicating whether the random effects are non-normal. Default: \code{FALSE}.
#' @param nonresidual Logical value indicating whether the residuals are non-normal. Default: \code{FALSE}.
#' @return A list containing:
#' \describe{
#'   \item{\code{Y_test_true}}{True values of the vector of outcomes in the testing set.}
#'   \item{\code{X_train}}{Matrix of covariates in the training set.}
#'   \item{\code{Y_train}}{Vector of outcomes in the training set.}
#'   \item{\code{Z_train}}{Matrix of random predictors in the training set.}
#'   \item{\code{subject_id_train}}{Vector of subject IDs in the training set.}
#'   \item{\code{time_train}}{Vector of time point in the training set.}
#'   \item{\code{X_test}}{Matrix of covariates in the testing set.}
#'   \item{\code{Y_test}}{Vector of outcomes in the testing set.}
#'   \item{\code{Z_test}}{Matrix of random predictors in the testing set.}
#'   \item{\code{subject_id_test}}{Vector of subject IDs in the testing set.}
#'   \item{\code{time_test}}{Vector of time point in the testing set.}
#' }
#'
#' @details The function creates a dataset with individuals observed at 6 follow-up time points. It allows 
#' users to specify whether the associations are linear or nonlinear and whether random effects and residuals 
#' follow normal or non-normal distributions. Approximately 40% of the data is randomly chosen to form the 
#' testing set, while the remaining 60% constitutes the training set.
#'
#' @examples 
#'   # Generate data with nonlinear associations and non-normal random effects and residuals
#'   data <- simulation_prediction(
#'     n_subject = 800,
#'     seed = 123,
#'     nonlinear = TRUE,
#'     nonrandeff = TRUE,
#'     nonresidual = TRUE
#'   )
#'   # Access training and testing data
#'   X_train <- data$X_train
#'   Y_train <- data$Y_train
#'   Z_train <- data$Z_train
#'   subject_id_train <- data$subject_id_train
#'
#'   X_test <- data$X_test
#'   Y_test <- data$Y_test
#'   Z_test <- data$Z_test
#'   subject_id_test <- data$subject_id_test
#'   
#'   Y_test_true = data$Y_test_true
#'
#' @seealso 
#'  \code{\link[mvtnorm]{Mvnorm}}
#'  \code{\link[stats]{Chisquare}}
#'  \code{\link[mice]{ampute}}
#' @rdname simulation_prediction
#' @export 
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rchisq
#' @importFrom mice ampute
simulation_prediction = function(n_subject = 800, seed = NULL, nonlinear = FALSE, nonrandeff = FALSE, nonresidual = FALSE){
  set.seed(123)
  n_obs_per_sub = 6
  n_obs_per_sub = sapply(1:n_subject, function(x) n_obs_per_sub)
  subject_id = c(unlist(sapply(1:n_subject, function(x) rep(x, n_obs_per_sub[x]))))
  n_obs = length(subject_id)
  Z = c(unlist(sapply(1:n_subject, function(x) 1:n_obs_per_sub[x])))
  trajectory = cbind(Z)
  Z = cbind(Z, Z^2)
  Z = cbind(rep(1, length(subject_id)), Z)
  Z = apply(Z[,-1], 2, scale)
  Z = cbind(1, Z)
  Z_O = cbind(Z)
  n = dim(Z)[1]

  
  X = mvtnorm::rmvnorm(n_obs, c(0, 2, 5, -10, -3, -8, 9))
  
  X[,1] = apply(cbind(X[,1]), 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  
  X[,2] = apply(cbind(X[,2]), 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X[,3] = apply(cbind(X[,3]), 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X[,4] = apply(cbind(X[,4]), 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X[,5] = apply(cbind(X[,5]), 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X[,6] = apply(cbind(X[,6]), 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X[,7] = apply(cbind(X[,7]), 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  Y = 5 + X[,1] - X[,2] + 3 * X[,3] - 2 * X[,4] + 0.5 * X[,5] - X[,6] + 0.5 * X[,7] 
  if(nonlinear)
    Y = 1 * (X[,3] * X[,4]) +        # Interaction1 (scaled down)
    1 * (X[,7] * X[,1]) +        # Interaction2 (scaled down)
    1 * log(3 * (X[,5] + X[,6])^2 + 0.5) +        # Simple addition interaction (scaled down)
    0.5 * X[,2]^2
  
  if(nonresidual){
    Y = Y + (-1)^stats::rbinom(n_obs, 1, prob = 0.5) * stats::rchisq(n_obs, 10)#stats::rnormmix(n_obs, mu = c(-8, -3, 2, 4, 4.5), sigma = c(1, 1, 1, 1, 1), lambda = c(1/5, 1/5, 0.15, 0.25, 1/5))
  }else{
    Y = Y + stats::rnorm(n_obs,sd = 5)
  }

  if (nonrandeff){
    
    # Define enhanced parameters
    xi <- c(0, 0, 0)  # Location vector
    
    
    Omega <- matrix(c(1.0, 0.5, 0.3,
                      0.5, 1.0, 0.4,
                      0.3, 0.4, 1.0),
                    nrow = 3, byrow = TRUE)
    alpha <- c(1, 1, 1) 
    Bi <- sn::rmst(n = n_subject, xi = xi, Omega = Omega, alpha = alpha, nu = 5)
  }else{
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
  }
  re = sapply(1:length(subject_id), function(x){
    Z[x,] %*% Bi[subject_id[x],]
  })
  Y = Y + re

  
  Y_copy = as.numeric(Y)
  
  
  data_O = dplyr::tibble(
    subject_id = subject_id,
    time = c(unlist(sapply(1:n_subject, function(x) 1:n_obs_per_sub[x]))),
    X1 = X[,1],
    X2 = X[,2],
    X3 = X[,3],
    X4 = X[,4],
    X5 = X[,5],
    X6 = X[,6],
    X7 = X[,7],
    Y = Y_copy
  )
  
  data = data_O %>% 
    tidyr::pivot_wider(id_cols = "subject_id", names_from = "time", values_from = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "Y"))
  
  
  
  
  patterns = matrix(1, nrow = n_obs_per_sub[1], ncol = n_obs_per_sub[1] * 8)
  patterns[1, 43] = 0
  patterns[2, 44] = 0
  patterns[3, 45] = 0
  patterns[4, 46] = 0
  patterns[5, 47] = 0
  patterns[6, 48] = 0
  
  for (i in 43:47) {
    for (j in (i + 1):48) {
      a = rep(1, n_obs_per_sub[1] * 8)
      a[i] = 0
      a[j] = 0
      patterns = rbind(patterns, a)
    }
  }
  
  for (i in 43:46) {
    for (j in (i + 1):47) {
      for (k in (j + 1):48) {
        a = rep(1, n_obs_per_sub[1] * 8)
        a[i] = 0
        a[j] = 0
        a[k] = 0
        patterns = rbind(patterns, a)
      }
    }
  }
  
  freq = rep(0.1, dim(patterns)[1])
  if (!is.null(seed))
    set.seed(seed)
  a = mice::ampute(data[,-1], mech = "MCAR", patterns = patterns, prop = 0.9999, freq = freq)
 
  
  
  

  
  b = dplyr::as_tibble(a$amp) %>%
    dplyr::mutate(subject_id = data$subject_id) %>% 
    tidyr::pivot_longer(cols = c(tidyr::starts_with("X"), tidyr::starts_with("Y")), names_prefix = "_", values_to = "value", names_to = "Variable") %>%
    tidyr::separate(col = "Variable", into = c("Variable", "Time"), sep = "_") %>% 
    tidyr::pivot_wider(id_cols = c("subject_id", "Time"), names_from = "Variable", values_from = "value") %>% 
    dplyr::mutate(followup = ifelse(
      is.na("X1") & is.na("X2") & is.na("X3") & is.na("X4") & is.na("X5") & is.na("X6") & is.na("X7") & is.na("Y"),
      0,
      1
    ))
  
  
  subject_id = b$subject_id
  trajectory = b$Time
  Y = unlist(b[,10])
  Y_O = Y_copy
  subject_id = data_O$subject_id
  
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
    Y_copy = Y_copy[!subject_id %in% omit_sub]
    
    Z = Z[!subject_id %in% omit_sub,]
    Z_O = Z_O[!subject_id %in% omit_sub,]
    
    trajectory = trajectory[!subject_id %in% omit_sub]
    Bi = Bi[-omit_sub,]
    re = re[!subject_id %in% omit_sub]
    subject_id = subject_id[!subject_id %in% omit_sub]
  }
  training_indicator = !is.na(Y)
  X_train = X[training_indicator,]
  Y_train = Y[training_indicator]
  Z_train = Z_O[training_indicator,]
  subject_id_train = subject_id[training_indicator]
  time_train = trajectory[training_indicator]
  
  X_test = X[!training_indicator,]
  Y_test = Y[!training_indicator]
  Z_test = Z_O[!training_indicator,]
  subject_id_test = subject_id[!training_indicator]
  time_test = trajectory[!training_indicator]
  
  Y_test_true = Y_O[!training_indicator]
  
  return(list(Y_test_true = Y_test_true, X_train = X_train, Y_train = Y_train, Z_train = Z_train, subject_id_train = subject_id_train, time_train = time_train, X_test = X_test, Y_test = Y_test, Z_test = Z_test, subject_id_test = subject_id_test, time_test = time_test))
}



