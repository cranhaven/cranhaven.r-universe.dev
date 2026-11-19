
#######################################
##### Code - Efficient Estimators #####
#######################################

# Load necessary libraries

utils::globalVariables(c("tau", "Type"))
#########################
#### FINAL FUNCTIONS ####
#########################

# Identity = eligibility to treatment
#' @title Two-Step GMM Estimation of Heterogeneous Peer Effects When Identity Equals Eligibility
#' @importFrom dplyr mutate filter select group_by
#' @importFrom purrr map map_dbl map2
#' @importFrom stats as.formula glm binomial lm optim rbinom predict pt
#' @importFrom dplyr %>%
#' @import mvtnorm
#' @param YE :  average outcome for eligible individuals within a group (vector)
#' @param YN  average outcome for non-eligible individuals within a group (vector)
#' @param D group binary treatment indicator (vector)
#' @param s share of eligible individuals in the group (vector)
#' @param n_param : number of parameters to estimate
#' @param tol : convergency criteria of optimization
#' @return A list with three elements:
#'   \item{estimates}{data frame with the estimated values of the direct
#'     effect (delta), intra-group effect (theta_within), inter-group effect
#'     (theta_between) and their standard errors.}
#'   \item{table}{a \code{flextable} object formatting \code{estimates} for
#'     display.}
#'   \item{plot}{a \code{ggplot} object showing the treatment effects on
#'     eligible and non-eligible individuals across values of \code{s}.}
#' @examples
#' \donttest{
#' # Parameters
#'delta = -3
#'thetaW = 0.7
#'thetaB = 0.3
#'beta_NE = -2
#'beta_E = -1
#'G <- 1000
#'# Generate the mean and sd of the random vector alpha
#'mu_alphaX <- c(8, 4, -3, 2)
#'Sigma_alphaX <- matrix(c(4, 1, 0, 0, 1, 4, 0, 0, 0, 0, 4, -4.2, 0, 0, -4.2, 9), nrow = 4)
#'# Create a logistic function to correlate D with s
#'logistic <- function(x) {
#' 1 / (1 + exp(-x))
#'}
#'# Generate the alpha vector
#'alphaX <- mvtnorm::rmvnorm(n = G, mean = mu_alphaX, sigma = Sigma_alphaX)
#'# Scale s to control correlation strength (optional)
#'s <- runif(n = G)
#'scaled_s <- (s - mean(s)) / sd(s)  # Standardize s for better control
#'# Compute probabilities for D based on scaled s
#'prob_D <- logistic(scaled_s)  # Adjust scaling for desired correlation
#'# Generate D based on the probabilities
#'D <- rbinom(n = G, size = 1, prob = prob_D)
#'# Generate YE and YNE 
#'YN <- ((1 - thetaW * s ) * (alphaX[,1] + beta_NE * alphaX[,3])  +
#'thetaB * s * (alphaX[,2] + beta_E * alphaX[,4]) +
#'delta * thetaB * s)

#'YE <- ((1 - thetaW * s) * (1 - s) * thetaB * (alphaX[,1] + beta_NE * alphaX[,3]) +
#'(1 + thetaW * (s * (1 - s) * thetaW - 1)) * (alphaX[,2] + beta_E * alphaX[,4]) +
#' delta * (1 + thetaW * (s * (1 - s) * thetaW - 1)) * D) /
#'  ((1 - thetaW * s) * (1 - thetaW + s * (1 - s) * (thetaW^2 - thetaB^2)))

#'result_simple_5param <- heter_endo_gmm(YE, YN, D, s)
#'# Access the estimates data frame:
#'result_simple_5param$estimates
#'# Display the formatted table:
#'# print(result_simple_5param$table)
#'# Display the treatment effect plot:
#'# print(result_simple_5param$plot)
#'}
#' @name heter_endo_gmm
#'@export
heter_endo_gmm <- function(YE, YN, D, s, n_param = 5, tol = 1e-6) {
  
  # Inputs:
  # YE: average outcome for eligible individuals within a group (vector)
  # YN: average outcome for non-eligible individuals within a group (vector)
  # D: group binary treatment indicator (vector)
  # s: share of eligible individuals in the group (vector)
  # n_param : number of parameters to estimate
  
  check_vectors(YE, YN, D, s)
  
  if (n_param == 5) {
    
    check_vectors(YE, YN, D, s)
    return(unknown_prop_score_5param_gmm(YE, YN, D, s, tol = 1e-6))
    
  } else if (n_param == 3) {
    
    check_vectors(YE, YN, D, s)
    return(unknown_prop_score_3param_gmm(YE, YN, D, s, tol = 1e-6))
  }
  
  else{
    stop("Error: n_param can only take as values 3 or 5")
  }
}

#' @title Two-Step GMM Estimation of Heterogeneous Peer Effects with Orthogonal Identity-Based Eligibility
#' @importFrom dplyr mutate filter select group_by
#' @importFrom purrr map map_dbl map2
#' @importFrom stats as.formula glm binomial lm optim rbinom predict pt
#' @importFrom dplyr %>%
#'@import mvtnorm
#' @param YM :  average outcome for "male" individuals within a group (vector)
#' @param YF  average outcome for "female" individuals within a group (vector)
#' @param D group binary treatment indicator (vector)
#' @param sM share of "male" individuals in the group (vector)
#' @param sEM share of eligible "male" individuals in the group (vector)
#' @param sEF share of eligible "female" individuals in the group (vector)
#' @param tol : convergency criteria of optimization
#' @return A list with two elements:
#'   \item{estimates}{data frame with the estimated values of the direct
#'     effect (delta), within effect among male (theta_within_M), within
#'     effect among female (theta_within_F), between effect from male to
#'     female (theta_between_F_M), between effect from female to male
#'     (theta_between_M_F) and their standard errors.}
#'   \item{table}{a \code{flextable} object formatting \code{estimates} for
#'     display.}
#' @examples
#' \donttest{
#'delta = -3
#'thetaWM = 0.6
#'thetaWW = 0.2
#'thetaBWM = -0.5
#'thetaBMW = 0.5
#'beta_W = -2
#'beta_M = -1
#'G <- 500
#'# Generate the mean and sd of the random vector alpha
#'mu_alphaX <- c(8, 4, -3, 2)
#'Sigma_alphaX <- matrix(c(4, 1, 0, 0, 1, 4, 0, 0, 0, 0, 4, -4.2, 0, 0, -4.2, 9), nrow = 4)
#'# Create a logistic function to correlate D with s
#'logistic <- function(x) {
#'  1 / (1 + exp(-x))
#'}
#'# Generate the alpha vector
#'alphaX <- mvtnorm::rmvnorm(n = G, mean = mu_alphaX, sigma = Sigma_alphaX)
#'# Generate the vectors (s, T)
#'sM <- runif(n = G)
#'sWE <- runif(n = G)
#'sME <- runif(n = G)
#'D <- rbinom(n = G, size = 1, prob = 0.5)

#'# Generate YE and YNE 


#'YW <- ((1 - thetaWM * sM) * (alphaX[,2] + beta_M * alphaX[,4]) +
#'thetaBWM * sM * (alphaX[,1] + beta_W * alphaX[,3]) +
#'  D * delta * (sWE * (1 - thetaWM * sM) + sME * thetaBWM * sM)) /
#'  (1 - thetaWW * (1 - sM) - thetaWM * sM +
#'     sM * (1 - sM) * (thetaWW * thetaWM - thetaBWM * thetaBMW))

#'YM <- ((1 - thetaWW * (1 - sM)) * (alphaX[,1] + beta_M * alphaX[,3]) +
#'thetaBMW * (1 - sM) * (alphaX[,2] + beta_W * alphaX[,4]) +
#'  D * delta * (sWE * thetaBMW * (1 - sM) +
#'                 sME * (1 - thetaWW * (1 - sM)))) /
#'  (1 - thetaWW * (1 - sM) - thetaWM * sM +
#'     sM * (1 - sM) * (thetaWW * thetaWM - thetaBWM * thetaBMW))



#'# Resultats analytique
#'result_ortho <- ortho_heter_endo_gmm(YM, YW, D, sM, sME, sWE)
#'result_ortho$estimates
#'# print(result_ortho$table)
#'}
#'@name ortho_heter_endo_gmm
#'@export
ortho_heter_endo_gmm <- function(YM, YF, D, sM, sEM, sEF, tol = 1e-6) {
  
  # Inputs:
  # YM: average outcome for "male" individuals within a group (vector)
  # YF: average outcome for "female" individuals within a group (vector)
  # D: group binary treatment indicator (vector)
  # sM: share of "male" individuals in the group (vector)
  # sEM: share of eligible "male" individuals in the group (vector)
  # sEF: share of eligible "female" individuals in the group (vector)
  
  check_vectors(YM, YF, D, sM, sEM, sEF)
  return(ortho_unknown_prop_score_5param_gmm(YM, YF, D, sM, sEM, sEF, tol = 1e-6))
  
}

# Identity is orthogonal to treatment eligibility


################################
#### INTERMEDIATE FUNCTIONS ####
################################

# Function to check if the vectors are well-defined
#'@noRd
check_vectors <- function(...) {
  # Capture all vectors passed as arguments
  vectors <- list(...)
  names(vectors) <- paste0("vector", seq_along(vectors)) # Name vectors
  
  # 1. Check if all vectors have the same length
  lengths <- sapply(vectors, length)
  if (length(unique(lengths)) != 1) {
    stop("Error: Vectors do not have the same length. Lengths are: ", 
         paste(names(vectors), lengths, sep = "=", collapse = ", "))
  }
  
  # 2. Check if there are any NAs in the vectors
  na_check <- sapply(vectors, anyNA)
  if (any(na_check)) {
    stop("Error: The following vectors contain NAs: ", 
         paste(names(vectors)[na_check], collapse = ", "))
  }
  
}


# Case with 5 parameters, identity = eligibility, unknown propensity score
#'@noRd
unknown_prop_score_5param_gmm <- function(YE, YN, D, s, tol = 1e-6) {
  
  # Inputs:
  # YE: Outcome variable for eligible (vector)
  # YN: Outcome variable for non-eligible (vector)
  # D: Binary treatment indicator (vector)
  # s: Share of eligible  (vector)
  
  G <- length(s)
  
  # Etape 0 : Divide the sample into 2
  cross_fit_indic <- rbinom(G, 1, 0.5)
  G_1 <- sum(cross_fit_indic == 1)
  G_0 <- sum(cross_fit_indic == 0)
  
  # Look at the 2 sub-samples
  YE_part_1 <- YE[cross_fit_indic == 1]
  YN_part_1 <- YN[cross_fit_indic == 1]
  D_part_1 <- D[cross_fit_indic == 1]
  s_part_1 <- s[cross_fit_indic == 1]
  
  YE_part_0 <- YE[cross_fit_indic == 0]
  YN_part_0 <- YN[cross_fit_indic == 0]
  D_part_0 <- D[cross_fit_indic == 0]
  s_part_0 <- s[cross_fit_indic == 0]
  
  df_part_1 <- data.frame(YE = YE_part_1, YN = YN_part_1, D = D_part_1, s = s_part_1)
  df_part_0 <- data.frame(YE = YE_part_0, YN = YN_part_0, D = D_part_0, s = s_part_0)
  
  max_degree <- 5
  k_folds <- 4
  
  # Create folds for cross-validation
  folds_1 <- caret::createFolds(df_part_1$D, k = k_folds, list = TRUE)
  folds1_1 <- caret::createFolds(df_part_1$YE[which(df_part_1$D == 1)], k = k_folds, list = TRUE)
  folds0_1 <- caret::createFolds(df_part_1$YE[which(df_part_1$D == 0)], k = k_folds, list = TRUE)
  
  folds_0 <- caret::createFolds(df_part_0$D, k = k_folds, list = TRUE)
  folds1_0 <- caret::createFolds(df_part_0$YE[which(df_part_0$D == 1)], k = k_folds, list = TRUE)
  folds0_0 <- caret::createFolds(df_part_0$YE[which(df_part_0$D == 0)], k = k_folds, list = TRUE)
  
  # Function to compute cross-validation error for a given degree
  #'@noRd
  cv_error <- function(degree) {
    
    errors_D_1 <- c()
    errors_YE0_1 <- c()
    errors_YE1_1 <- c()
    errors_YN0_1 <- c()
    errors_YN1_1 <- c()
    
    errors_D_0 <- c()
    errors_YE0_0 <- c()
    errors_YE1_0 <- c()
    errors_YN0_0 <- c()
    errors_YN1_0 <- c()
    
    for (i in 1:k_folds) {
      # Split into training and validation sets
      train_index_1 <- unlist(folds_1[-i])
      test_index_1 <- unlist(folds_1[i])
      train_data_1 <- df_part_1[train_index_1, ]
      test_data_1 <- df_part_1[test_index_1, ]
      train_index0_1 <- unlist(folds0_1[-i])
      test_index0_1 <- unlist(folds0_1[i])
      train_data0_1 <- df_part_1[train_index0_1, ]
      test_data0_1 <- df_part_1[test_index0_1, ]
      
      train_index1_1 <-unlist(folds1_1[-i])
      test_index1_1 <- unlist(folds1_1[i])
      train_data1_1 <- df_part_1[train_index1_1, ]
      test_data1_1 <- df_part_1[test_index1_1, ]
      train_index_0 <- unlist(folds_0[-i])
      test_index_0 <- unlist(folds_0[i])
      train_data_0 <- df_part_0[train_index_0, ]
      test_data_0 <- df_part_0[test_index_0, ]
      
      train_index0_0 <- unlist(folds0_0[-i])
      test_index0_0 <- unlist(folds0_0[i])
      train_data0_0 <- df_part_0[train_index0_0, ]
      test_data0_0 <- df_part_0[test_index0_0, ]
      
      train_index1_0 <-unlist(folds1_0[-i])
      test_index1_0 <- unlist(folds1_0[i])
      train_data1_0 <- df_part_0[train_index1_0, ]
      test_data1_0 <- df_part_0[test_index1_0, ]
      
      # Fit polynomial regression
      if (degree == 0) {
        formula_D <- as.formula("D ~ 1")  # Model with only an intercept
        formula_YE <- as.formula("YE ~ 1")
        formula_YN <- as.formula("YN ~ 1")
        
        model_D_1 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_1)
        model_YE0_1 <- lm(formula_YE, data = train_data0_1)
        model_YE1_1 <- lm(formula_YE, data = train_data1_1)
        model_YN0_1 <- lm(formula_YN, data = train_data0_1)
        model_YN1_1 <- lm(formula_YN, data = train_data1_1)
        
        model_D_0 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_0)
        model_YE0_0 <- lm(formula_YE, data = train_data0_0)
        model_YE1_0 <- lm(formula_YE, data = train_data1_0)
        model_YN0_0 <- lm(formula_YN, data = train_data0_0)
        model_YN1_0 <- lm(formula_YN, data = train_data1_0)
        
      } else {
        formula_D <- as.formula(paste("D ~ poly(s,", degree, ", raw=TRUE)"))
        formula_YE <- as.formula(paste("YE ~ poly(s,", degree, ", raw=TRUE)"))
        formula_YN <- as.formula(paste("YN ~ poly(s,", degree, ", raw=TRUE)"))
        
        model_D_1 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_1)
        model_YE0_1 <- lm(formula_YE, data = train_data0_1)
        model_YE1_1 <- lm(formula_YE, data = train_data1_1)
        model_YN0_1 <- lm(formula_YN, data = train_data0_1)
        model_YN1_1 <- lm(formula_YN, data = train_data1_1)
        
        model_D_0 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_0)
        model_YE0_0 <- lm(formula_YE, data = train_data0_0)
        model_YE1_0 <- lm(formula_YE, data = train_data1_0)
        model_YN0_0 <- lm(formula_YN, data = train_data0_0)
        model_YN1_0 <- lm(formula_YN, data = train_data1_0)
      }
      
      # Predict on test set
      preds_D_1 <- predict(model_D_1, type = "response", newdata = test_data_1)
      preds_YE0_1 <- predict(model_YE0_1, newdata = test_data0_1)
      preds_YE1_1 <- predict(model_YE1_1, newdata = test_data1_1)
      preds_YN0_1 <- predict(model_YN0_1, newdata = test_data0_1)
      preds_YN1_1 <- predict(model_YN1_1, newdata = test_data1_1)
      
      # Predict on test set
      preds_D_0 <- predict(model_D_0, type = "response", newdata = test_data_0)
      preds_YE0_0 <- predict(model_YE0_0, newdata = test_data0_0)
      preds_YE1_0 <- predict(model_YE1_0, newdata = test_data1_0)
      preds_YN0_0 <- predict(model_YN0_0, newdata = test_data0_0)
      preds_YN1_0 <- predict(model_YN1_0, newdata = test_data1_0)
      
      # Compute mean squared error
      errors_D_1 <- c(errors_D_1, mean((preds_D_1 - test_data_1$D)^2))
      errors_YE0_1 <- c(errors_YE0_1, mean((preds_YE0_1 - test_data0_1$YE)^2))
      errors_YE1_1 <- c(errors_YE1_1, mean((preds_YE1_1 - test_data1_1$YE)^2))
      errors_YN0_1 <- c(errors_YN0_1, mean((preds_YN0_1 - test_data0_1$YN)^2))
      errors_YN1_1 <- c(errors_YN1_1, mean((preds_YN1_1 - test_data1_1$YN)^2))
      
      errors_D_0 <- c(errors_D_0, mean((preds_D_0 - test_data_0$D)^2))
      errors_YE0_0 <- c(errors_YE0_0, mean((preds_YE0_0 - test_data0_0$YE)^2))
      errors_YE1_0 <- c(errors_YE1_0, mean((preds_YE1_0 - test_data1_0$YE)^2))
      errors_YN0_0 <- c(errors_YN0_0, mean((preds_YN0_0 - test_data0_0$YN)^2))
      errors_YN1_0 <- c(errors_YN1_0, mean((preds_YN1_0 - test_data1_0$YN)^2))
    }
    
    return(c(mean(errors_D_1), mean(errors_YE0_1), mean(errors_YE1_1), mean(errors_YN0_1), mean(errors_YN1_1),
             mean(errors_D_0), mean(errors_YE0_0), mean(errors_YE1_0), mean(errors_YN0_0), mean(errors_YN1_0)))  # Return average CV error
  }
  #'@noRd  
  # Evaluate cross-validation error for each polynomial degree
  cv_results <- sapply(0:max_degree, cv_error)
  
  best_degree_D_1 <- which.min(cv_results[1,]) - 1
  best_degree_YE0_1 <- which.min(cv_results[2,]) - 1
  best_degree_YE1_1 <- which.min(cv_results[3,]) - 1
  best_degree_YN0_1 <- which.min(cv_results[4,]) - 1
  best_degree_YN1_1 <- which.min(cv_results[5,]) - 1
  
  best_degree_D_0 <- which.min(cv_results[6,]) - 1
  best_degree_YE0_0 <- which.min(cv_results[7,]) - 1
  best_degree_YE1_0 <- which.min(cv_results[8,]) - 1
  best_degree_YN0_0 <- which.min(cv_results[9,]) - 1
  best_degree_YN1_0 <- which.min(cv_results[10,]) - 1
  
  if (best_degree_D_1 == 0) {
    logit_model_1 <- glm(D ~ 1, family = binomial(link = "logit"), data = df_part_1)
  } else {
    formula_D <- as.formula(paste("D ~ poly(s,", best_degree_D_1, ", raw=TRUE)"))
    logit_model_1 <- glm(formula_D, family = binomial(link = "logit"), data = df_part_1)
  }
  
  if (best_degree_D_0 == 0) {
    logit_model_0 <- glm(D ~ 1, family = binomial(link = "logit"), data = df_part_0)
  } else {
    formula_D <- as.formula(paste("D ~ poly(s,", best_degree_D_0, ", raw=TRUE)"))
    logit_model_0 <- glm(formula_D, family = binomial(link = "logit"), data = df_part_0)
  }
  
  h_s_1 <- predict(logit_model_0, type = "response", newdata = df_part_1)
  h_s_0 <- predict(logit_model_1, type = "response", newdata = df_part_0)
  
  #Subset data
  training_data1_1 <- subset(df_part_1, D == 1)
  training_data0_1 <- subset(df_part_1, D == 0)
  
  training_data1_0 <- subset(df_part_0, D == 1)
  training_data0_0 <- subset(df_part_0, D == 0)
  
  if (best_degree_YE0_1 == 0) {
    YE0_model_1 <- lm(YE ~ 1, data = training_data0_1)
  } else {
    YE0_model_1 <- lm(YE ~ poly(s, degree = best_degree_YE0_1), data = training_data0_1)
  }
  
  if (best_degree_YE0_0 == 0) {
    YE0_model_0 <- lm(YE ~ 1, data = training_data0_0)
  } else {
    YE0_model_0 <- lm(YE ~ poly(s, degree = best_degree_YE0_0), data = training_data0_0)
  }
  
  if (best_degree_YE1_1 == 0) {
    YE1_model_1 <- lm(YE ~ 1, data = training_data1_1)
  } else {
    YE1_model_1 <- lm(YE ~ poly(s, degree = best_degree_YE1_1), data = training_data1_1)
  }
  
  if (best_degree_YE1_0 == 0) {
    YE1_model_0 <- lm(YE ~ 1, data = training_data1_0)
  } else {
    YE1_model_0 <- lm(YE ~ poly(s, degree = best_degree_YE1_0), data = training_data1_0)
  }
  
  if (best_degree_YN0_1 == 0) {
    YN0_model_1 <- lm(YN ~ 1, data = training_data0_1)
  } else {
    YN0_model_1 <- lm(YN ~ poly(s, degree = best_degree_YN0_1), data = training_data0_1)
  }
  
  if (best_degree_YN0_0 == 0) {
    YN0_model_0 <- lm(YN ~ 1, data = training_data0_0)
  } else {
    YN0_model_0 <- lm(YN ~ poly(s, degree = best_degree_YN0_0), data = training_data0_0)
  }
  
  if (best_degree_YN1_1 == 0) {
    YN1_model_1 <- lm(YN ~ 1, data = training_data1_1)
  } else {
    YN1_model_1 <- lm(YE ~ poly(s, degree = best_degree_YN1_1), data = training_data1_1)
  }
  
  if (best_degree_YN1_0 == 0) {
    YN1_model_0 <- lm(YN ~ 1, data = training_data1_0)
  } else {
    YN1_model_0 <- lm(YE ~ poly(s, degree = best_degree_YN1_0), data = training_data1_0)
  }
  
  # Use the fitted model to predict values for the whole dataset
  E_YE0_s_1 <- predict(YE0_model_0, newdata = df_part_1)
  E_YE1_s_1 <- predict(YE1_model_0, newdata = df_part_1)
  E_YN0_s_1 <- predict(YN0_model_0, newdata = df_part_1)
  E_YN1_s_1 <- predict(YN1_model_0, newdata = df_part_1)
  
  E_YE0_s_0 <- predict(YE0_model_1, newdata = df_part_0)
  E_YE1_s_0 <- predict(YE1_model_1, newdata = df_part_0)
  E_YN0_s_0 <- predict(YN0_model_1, newdata = df_part_0)
  E_YN1_s_0 <- predict(YN1_model_1, newdata = df_part_0)
  
  ### Etape 1-1: Define the function phi
  #'@noRd
  phi <- function(s, lambda){
    val <- 1 - lambda[2]*s - lambda[3]*(1-s) + s*(1-s)*(lambda[2]*lambda[3] - lambda[4]*lambda[5])
    # Empêche la division par une valeur trop proche de 0
    sign_val <- ifelse(val >= 0, 1, -1)
    ifelse(abs(val) < 1e-6, sign_val * 1e-6, val)
  }
    
  
  ### Etape 1-2: Define the conditional treatment effect functions
  #'@noRd
  Num_condi_TE <- function(s, lambda){
    mE <- lambda[1] * (1 - lambda[3] * (1-s)) * (s > 0)
    mN <- lambda[1] * lambda[4] * s * (s < 1)
    m <- return(cbind(mE, mN))
  }
  
  ### Etape 1-3: Define the final function m (dimension G x 2)
  #'@noRd
  moment_function <- function(lambda, Z) {
    YE <- Z[, 1]
    YN <- Z[, 2]
    D <- Z[, 3]
    s <- Z[, 4]
    h_s <- Z[, 5]
    h_s_clipped <- pmin(pmax(h_s, 1e-4), 1 - 1e-4)  # évite 0 et 1 exacts
    denom_ipw <- h_s_clipped * (1 - h_s_clipped)
    
    IPW_YE <- (((D - h_s_clipped)*YE)/(denom_ipw)) * (s > 0)
    IPW_YN <- (((D - h_s_clipped)*YN)/(denom_ipw)) * (s < 1)
    
    m <- cbind(IPW_YE, IPW_YN) - (Num_condi_TE(s, lambda)/phi(s, lambda))
    
    return(m)
  }
  
  ## Etape 1-3: Define the GMM objective function
  #'@noRd
  gmm1_objective <- function(params, Z) {
    s <- Z[, 4]
    m <- moment_function(params, Z)
    residuals <- cbind(m, s * m, s*s * m, log(s+0.0001) * m, sqrt(s) * m, exp(s) * m, (1./(s+0.1))*m)
    if (!all(is.finite(residuals))) return(1e10)  # pénalité finie, pas de crash
    
    emp_moments <- colMeans(residuals)
    val <- as.numeric(t(emp_moments) %*% emp_moments)
    
    if (!is.finite(val)) return(1e10)
    return(val)
  }
  
  Z_1 <- cbind(YE_part_1, YN_part_1, D_part_1, s_part_1, h_s_1)  # Combine data
  Z_0 <- cbind(YE_part_0, YN_part_0, D_part_0, s_part_0, h_s_0)  # Combine data
  
  lambda_init <- c(0, 0.1, 0.1, 0.1, 0.1)
  
  opt_1 <- optim(
    lambda_init,
    fn = gmm1_objective,
    Z = Z_1,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda1_1 <- opt_1$par
  
  opt_0 <- optim(
    lambda_init,
    fn = gmm1_objective,
    Z = Z_0,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda1_0 <- opt_0$par
  
  lambda1 <- (G_1/G)*lambda1_1 + (G_0/G)*lambda1_0
  
  # Etape 2 : estimation des poids
  
  ## Etape 2-1 : function to have M
  #'@noRd
  Mi <- function(s, lambda){
    
    denom <- phi(s, lambda)
    
    Mi_E1 <- (s > 0) * ((1-lambda[3]*(1-s))/denom)
    Mi_E2 <- (s > 0) * (-1) * lambda[1] * (1 - lambda[3]*(1-s)) * (s*(1-s)*lambda[3] - s)/(denom*denom)
    Mi_E3 <- (s > 0) * lambda[1] * (-1) * ((1 - s) * denom + (1-lambda[3]*(1-s)) * (s*(1-s)*lambda[2] - (1-s)))/(denom*denom)
    Mi_E4 <- (s > 0) * lambda[1] * (1-lambda[3]*(1-s)) * s*(1-s)*lambda[5] /(denom*denom)
    Mi_E5 <- (s > 0) * lambda[1] * (1-lambda[3]*(1-s)) * s*(1-s)*lambda[4] /(denom*denom)
    
    Mi_N1 <- (s < 1) * ((lambda[4] * s)/denom)
    Mi_N2 <- (s < 1) * (-1) * lambda[1] * s * lambda[4] * (s*(1-s)*lambda[3] - s)/(denom*denom)
    Mi_N3 <- (s < 1) * (-1) * lambda[1] * s * lambda[4] * (s*(1-s)*lambda[2] - (1-s))/(denom*denom)
    Mi_N4 <- (s < 1) * lambda[1] * (s * denom + lambda[4] * s * s*(1-s)*lambda[5])/(denom*denom)
    Mi_N5 <- (s < 1) * (lambda[1] * s * lambda[4]) * (s*(1-s)*lambda[4])/(denom*denom)
    
    Mi <- (-1) * cbind(c(Mi_E1, Mi_N1), c(Mi_E2, Mi_N2), c(Mi_E3, Mi_N3), c(Mi_E4, Mi_N4), c(Mi_E5, Mi_N5))
    
    return(Mi)
    
  }
  
  ## Etape 2-1 : function to estimate alpha
  #'@noRd
  alpha_i <- function(s, D, p_score, E_YE1_s, E_YN1_s, E_YE0_s, E_YN0_s){
    
    alpha_1 <- ((E_YE1_s/p_score) +  (E_YE0_s/(1-p_score)))
    alpha_2 <- ((E_YN1_s/p_score) +  (E_YN0_s/(1-p_score)))
    
    alpha <- (-1) * cbind(alpha_1, alpha_2)
    
    return(alpha)
  }
  
  ###### Case with no clustering #####
  
  ## Nonparametric estimation of the variance of residuals
  
  # On recupere les variances de rhoE, rhoN et Cov(rhoE, rhoN)
  rho_1 <- moment_function(lambda1, Z_1)
  rho_0 <- moment_function(lambda1, Z_0)
  
  df_part_1$rho_trho_E <- rho_1[,1]*rho_1[,1]
  df_part_1$rho_trho_NE <- rho_1[,1]*rho_1[,2]
  df_part_1$rho_trho_N <- rho_1[,2]*rho_1[,2]
  
  df_part_0$rho_trho_E <- rho_0[,1]*rho_0[,1]
  df_part_0$rho_trho_NE <- rho_0[,1]*rho_0[,2]
  df_part_0$rho_trho_N <- rho_0[,2]*rho_0[,2]
  
  # Create folds for cross-validation
  folds_Sigma_1 <- caret::createFolds(df_part_1$s, k = k_folds, list = TRUE)
  folds_Sigma_0 <- caret::createFolds(df_part_0$s, k = k_folds, list = TRUE)
  
  # Function to compute cross-validation error for a given degree
  #'@noRd
  cv_error_Sigma <- function(degree) {
    
    errors_1 <- c()
    errors_0 <- c()
    
    for (i in 1:k_folds) {
      # Split into training and validation sets
      train_idx_0 <- unlist(folds_Sigma_0[-i])
      test_idx_0 <- unlist(folds_Sigma_0[i])
      
      train_data_0 <- df_part_0[train_idx_0, ]
      test_data_0 <- df_part_0[test_idx_0, ]
      
      train_idx_1 <- unlist(folds_Sigma_1[-i])
      test_idx_1 <- unlist(folds_Sigma_1[i])
      
      train_data_1 <- df_part_1[train_idx_1, ]
      test_data_1 <- df_part_1[test_idx_1, ]
      
      # Define formula based on polynomial degree
      if (degree == 0) {
        formula <- as.formula("cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ 1")  # Model with only an intercept
      } else {
        formula <- as.formula(paste("cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ poly(s,", degree, ", raw=TRUE)"))
      }
      
      # Fit regression model
      model_0 <- lm(formula, data = train_data_0)
      model_1 <- lm(formula, data = train_data_1)
      
      # Predict on test set
      preds_0 <- predict(model_0, newdata = test_data_0)
      preds_1 <- predict(model_1, newdata = test_data_1)
      
      # Compute Mean Squared Error (MSE) for each outcome
      mseE_1  <- mean((preds_1[, 1] - test_data_1$rho_trho_E)^2)
      mseNE_1 <- mean((preds_1[, 2] - test_data_1$rho_trho_NE)^2)
      mseN_1  <- mean((preds_1[, 3] - test_data_1$rho_trho_N)^2)
      
      mseE_0  <- mean((preds_0[, 1] - test_data_0$rho_trho_E)^2)
      mseNE_0 <- mean((preds_0[, 2] - test_data_0$rho_trho_NE)^2)
      mseN_0  <- mean((preds_0[, 3] - test_data_0$rho_trho_N)^2)
      
      # Store the average MSE across outcomes
      errors_1 <- c(errors_1, mean(c(mseE_1, mseNE_1, mseN_1)))
      errors_0 <- c(errors_0, mean(c(mseE_0, mseNE_0, mseN_0)))
    }
    
    return(c(mean(errors_1), mean(errors_0)))  # Return average CV error
  }
  
  # Evaluate cross-validation error for each polynomial degree (including 0)
  cv_results_Sigma <- sapply(0:20, cv_error_Sigma)
  
  # Find the best polynomial degree (minimizing CV error)
  best_degree_Sigma_1 <- which.min(cv_results[1,]) - 1  # Adjust index to match degree
  best_degree_Sigma_0 <- which.min(cv_results[2,]) - 1  # Adjust index to match degree
  
  if (best_degree_Sigma_0 == 0) {
    model_V_s_0 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ 1, data = df_part_0)
  } else {
    model_V_s_0 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ poly(s, degree = best_degree_Sigma_0), data = df_part_0)
  }
  
  if (best_degree_Sigma_1 == 0) {
    model_V_s_1 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ 1, data = df_part_1)
  } else {
    model_V_s_1 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ poly(s, degree = best_degree_Sigma_1), data = df_part_1)
  }
  
  #Use the fitted model to predict values for the whole dataset
  predict_V_s_1 <- predict(model_V_s_0, newdata = df_part_1)
  predict_V_s_0 <- predict(model_V_s_1, newdata = df_part_0)
  
  alpha_1 <- alpha_i(s_part_1, D_part_1, h_s_1, E_YE1_s_1, E_YN1_s_1, E_YE0_s_1, E_YN0_s_1)
  alpha_0 <- alpha_i(s_part_0, D_part_0, h_s_0, E_YE1_s_0, E_YN1_s_0, E_YE0_s_0, E_YN0_s_0)
  
  # Apply the function to each row
  w_hat_list_1 <- lapply(1:G_1, function(i) t(Mi(s_part_1[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_1[i,1], predict_V_s_1[i,2], predict_V_s_1[i,2], predict_V_s_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol))
  w_hat_list_0 <- lapply(1:G_0, function(i) t(Mi(s_part_0[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_0[i,1], predict_V_s_0[i,2], predict_V_s_0[i,2], predict_V_s_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol))
  
  # Combine results into a 3D array
  w_hat_array_1 <- array(unlist(w_hat_list_1), dim = c(5, 2, G_1))
  w_hat_array_0 <- array(unlist(w_hat_list_0), dim = c(5, 2, G_0))
  
  # Apply the function to each row
  V_ind_list_1 <- lapply(1:G_1, function(i) t(Mi(s_part_1[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_1[i,1], predict_V_s_1[i,2], predict_V_s_1[i,2], predict_V_s_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol) %*% Mi(s_part_1[i], lambda1))
  V_ind_list_0 <- lapply(1:G_0, function(i) t(Mi(s_part_0[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_0[i,1], predict_V_s_0[i,2], predict_V_s_0[i,2], predict_V_s_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol) %*% Mi(s_part_0[i], lambda1))
  
  # Combine results into a 3D array
  V_ind_array_1 <- array(unlist(V_ind_list_1), dim = c(5, 5, G_1))
  V_ind_array_0 <- array(unlist(V_ind_list_0), dim = c(5, 5, G_0))
  
  # Sum the matrices
  V_a_1 <- apply(V_ind_array_1, c(1, 2), mean)
  V_a_0 <- apply(V_ind_array_0, c(1, 2), mean)
  
  # Step 3: Define the GMM objective function
  #'@noRd
  gmm2_objective <- function(params, Z, w, V, G) {
    # Calcul des rho + mise sous forme de vecteurs
    residuals <- moment_function(params, Z)
    if (!all(is.finite(residuals))) return(1e10)
    residuals_vector_list <- lapply(1:G, function(i) matrix(residuals[i, ], ncol = 1))
    
    
    # Step 3: Multiply each 5x2 matrix with the corresponding 2x1 vector
    w_rho_list <- mapply(function(M, v) M %*% v, w, residuals_vector_list, SIMPLIFY = FALSE)
    
    
    # Step 4: Combine results into an G x 5 matrix
    w_rho_matrix <- do.call(rbind, lapply(w_rho_list, as.vector))
    if (!all(is.finite(w_rho_matrix))) return(1e10)
    
    # Element-wise multiplication and summation
    Vinv  <- tryCatch(solve(V), error = function(e) MASS::ginv(V))
    moments <- t(colMeans(w_rho_matrix)) %*% Vinv %*% colMeans(w_rho_matrix)
    if (!is.finite(moments)) return(1e10)
    return(moments)
  }
  
  
  opt2_1 <- optim(
    par = lambda1,
    fn = gmm2_objective,
    Z = Z_1,
    G = G_1,
    V = V_a_1,
    w = w_hat_list_1,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  
  lambda2_1 <- opt2_1$par
  
  
  opt2_0 <- optim(
    par = lambda1,
    fn = gmm2_objective,
    Z = Z_0,
    G = G_0,
    V = V_a_0,
    w = w_hat_list_0,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  
  lambda2_0 <- opt2_0$par
  
  lambda2 <- (G_1/G)*lambda2_1 + (G_0/G)*lambda2_0
  
  # Computing standard errors
  
  # On recupere les variances de rhoE, rhoN et Cov(rhoE, rhoN)
  rho_2_1 <- moment_function(lambda2, Z_1)
  df_part_1$rho_trho_E2 <- rho_2_1[,1]*rho_2_1[,1]
  df_part_1$rho_trho_NE2 <- rho_2_1[,1]*rho_2_1[,2]
  df_part_1$rho_trho_N2 <- rho_2_1[,2]*rho_2_1[,2]
  
  rho_2_0 <- moment_function(lambda2, Z_0)
  df_part_0$rho_trho_E2 <- rho_2_0[,1]*rho_2_0[,1]
  df_part_0$rho_trho_NE2 <- rho_2_0[,1]*rho_2_0[,2]
  df_part_0$rho_trho_N2 <- rho_2_0[,2]*rho_2_0[,2]
  
  if (best_degree_Sigma_0 == 0) {
    predict_V_s2_1 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ 1, data = df_part_0), newdata = df_part_1)
  } else {
    predict_V_s2_1 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ poly(s, degree = best_degree_Sigma_0), data = df_part_0), newdata = df_part_1)
  }
  
  if (best_degree_Sigma_1 == 0) {
    predict_V_s2_0 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ 1, data = df_part_1), newdata = df_part_0)
  } else {
    predict_V_s2_0 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ poly(s, degree = best_degree_Sigma_1), data = df_part_1), newdata = df_part_0)
  }
  
  # Apply the function to each row
  V_ind_list_1 <- lapply(1:G_1, function(i) t(Mi(s_part_1[i], lambda2)) %*% MASS::ginv(matrix(c(predict_V_s2_1[i,1], predict_V_s2_1[i,2], predict_V_s2_1[i,2], predict_V_s2_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol) %*% Mi(s_part_1[i], lambda2))
  V_ind_list_0 <- lapply(1:G_0, function(i) t(Mi(s_part_0[i], lambda2)) %*% MASS::ginv(matrix(c(predict_V_s2_0[i,1], predict_V_s2_0[i,2], predict_V_s2_0[i,2], predict_V_s2_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol) %*% Mi(s_part_0[i], lambda2))
  
  
  # Combine results into a 3D array
  V_ind_array_1 <- array(unlist(V_ind_list_1), dim = c(5, 5, G_1))
  V_ind_array_0 <- array(unlist(V_ind_list_0), dim = c(5, 5, G_0))
  
  # Sum the matrices
  V_a_1 <- apply(V_ind_array_1, c(1, 2), mean)
  V_a_0 <- apply(V_ind_array_0, c(1, 2), mean)
  
  V_a <- (G_1/G)^2 * V_a_1 + (G_0/G)^2 * V_a_0
  nb_groups <- G
  Vainv <- tryCatch(solve(V_a), error = function(e) MASS::ginv(V_a))
  standard_errors <- sqrt(diag(Vainv)/nb_groups)
  
  t_stat <- lambda2 / standard_errors
  df <- nb_groups - 5 - 1
  
  #bilateral test
  
  p_value <- 2 * (1 - pt(abs(t_stat), df))
  # Construction du dataframe
  result_df <- data.frame(
    Statistic = c("Estimate", "Standard Error", "t_test","p_value"),
    `delta` = c(round(lambda2[1],3), round(standard_errors[1],3),round(t_stat[1],3),round(p_value[1],3)),
    `thetaWE` = c(round(lambda2[2],3), round(standard_errors[2],3),round(t_stat[2],3),round(p_value[2],3)),
    `thetaWN` = c(round(lambda2[3],3), round(standard_errors[3],3),round(t_stat[3],3),round(p_value[3],3)),
    `thetaBNE` = c(round(lambda2[4],3), round(standard_errors[4],3),round(t_stat[4],3),round(p_value[4],3)),
    `thetaBEN` = c(round(lambda2[5],3), round(standard_errors[5],3),round(t_stat[5],3),round(p_value[5],3))
  )
  
  ft <- flextable::flextable(result_df) 
  ft <- flextable::set_header_labels(ft,
      delta = "Delta (\u0394)",
      thetaWE = "ThetaW (\u03B8wE)",
      thetaWN = "ThetaB (\u03B8wN)",
      thetaBNE = "ThetaW (\u03B8bNE)",
      thetaBEN = "ThetaB (\u03B8bEN)"
    )
  
  # Fonction treatment effect
  #'@noRd
  tau_E <- function(s, lambda2) {
    return(lambda2[1] * (1 + s * ((lambda2[2] - (1 - s) * (lambda2[2] * lambda2[3] - lambda2[4] * lambda2[5])) /
                                    (1 - s * lambda2[2] - (1-s) * lambda2[3] + s * (1 - s) * (lambda2[2]*lambda2[3] - lambda2[4]*lambda2[5])))))
  }
  
  #'@noRd
  tau_N <- function(s, lambda2){
    return (lambda2[1] * lambda2[4] * s / (1 - lambda2[2] * s - (1-s) * lambda2[3] + s * (1 - s) * (lambda2[2]*lambda2[3] - lambda2[4]*lambda2[5])))
  }
  
  s_values <- seq(0, 1, by = 0.01)
  tau_E_values <- sapply(s_values, tau_E,lambda2=lambda2)
  tau_N_values <- sapply(s_values, tau_N,lambda2=lambda2)
  tau_pop_values <- s_values * tau_E_values + (1 - s_values) * tau_N_values
  
  df <- data.frame(
    s = rep(s_values, 3),
    tau = c(tau_E_values, tau_N_values, tau_pop_values),
    Type = rep(c("Eligible", "Non-eligible", "Average"), each = length(s_values))
  )
  
  # Tracer avec ggplot2
  p <-ggplot2::ggplot(df, ggplot2::aes(x = s, y = tau, color = Type))+
          ggplot2::geom_line(linewidth = 1.2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "Treatment effects on eligibles and non-eligibles", 
               x = "s", y = "Total Effect") +
          ggplot2::scale_color_manual(values = c("black", "blue", "red")) +
          #xlim(0, 1) +
          #ylim(-1, 4) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
            axis.title = ggplot2::element_text(size = 14),
            axis.text = ggplot2::element_text(size = 12)
          )
  
  
 return(list(estimates = result_df, table = ft, plot = p))
}

# Case 2 : unknown propensity score with 
#'@noRd
unknown_prop_score_3param_gmm <- function(YE, YN, D, s, cluster = NULL, tol = 1e-6) {
  
  # Inputs:
  # YE: Outcome variable for eligible (vector)
  # YN: Outcome variable for non-eligible (vector)
  # D: Binary treatment indicator (vector)
  # s: Share of eligible  (vector)
  # degree1: degree for polynomial estimation of the propensity score
  # degree2 : degree for polynomial estimation of conditional variance
  
  # Recover the length of different vectors
  
  G <- length(s)
  
  # Etape 0 : Divide the sample into 2
  cross_fit_indic <- rbinom(G, 1, 0.5)
  G_1 <- sum(cross_fit_indic == 1)
  G_0 <- sum(cross_fit_indic == 0)
  
  # Look at the 2 sub-samples
  YE_part_1 <- YE[cross_fit_indic == 1]
  YN_part_1 <- YN[cross_fit_indic == 1]
  D_part_1 <- D[cross_fit_indic == 1]
  s_part_1 <- s[cross_fit_indic == 1]
  
  YE_part_0 <- YE[cross_fit_indic == 0]
  YN_part_0 <- YN[cross_fit_indic == 0]
  D_part_0 <- D[cross_fit_indic == 0]
  s_part_0 <- s[cross_fit_indic == 0]
  
  df_part_1 <- data.frame(YE = YE_part_1, YN = YN_part_1, D = D_part_1, s = s_part_1)
  df_part_0 <- data.frame(YE = YE_part_0, YN = YN_part_0, D = D_part_0, s = s_part_0)
  
  max_degree <- 10
  k_folds <- 3
  
  # Create folds for cross-validation
  folds_1 <- caret::createFolds(df_part_1$D, k = k_folds, list = TRUE)
  folds1_1 <- caret::createFolds(df_part_1$YE[which(df_part_1$D == 1)], k = k_folds, list = TRUE)
  folds0_1 <- caret::createFolds(df_part_1$YE[which(df_part_1$D == 0)], k = k_folds, list = TRUE)
  
  folds_0 <- caret::createFolds(df_part_0$D, k = k_folds, list = TRUE)
  folds1_0 <- caret::createFolds(df_part_0$YE[which(df_part_0$D == 1)], k = k_folds, list = TRUE)
  folds0_0 <- caret::createFolds(df_part_0$YE[which(df_part_0$D == 0)], k = k_folds, list = TRUE)
  
  # Function to compute cross-validation error for a given degree
  #'@noRd
  cv_error <- function(degree) {
    
    errors_D_1 <- c()
    errors_YE0_1 <- c()
    errors_YE1_1 <- c()
    errors_YN0_1 <- c()
    errors_YN1_1 <- c()
    
    errors_D_0 <- c()
    errors_YE0_0 <- c()
    errors_YE1_0 <- c()
    errors_YN0_0 <- c()
    errors_YN1_0 <- c()
    
    for (i in 1:k_folds) {
      # Split into training and validation sets
      train_index_1 <- unlist(folds_1[-i])
      test_index_1 <- unlist(folds_1[i])
      train_data_1 <- df_part_1[train_index_1, ]
      test_data_1 <- df_part_1[test_index_1, ]
      train_index0_1 <- unlist(folds0_1[-i])
      test_index0_1 <- unlist(folds0_1[i])
      train_data0_1 <- df_part_1[train_index0_1, ]
      test_data0_1 <- df_part_1[test_index0_1, ]
      
      train_index1_1 <-unlist(folds1_1[-i])
      test_index1_1 <- unlist(folds1_1[i])
      train_data1_1 <- df_part_1[train_index1_1, ]
      test_data1_1 <- df_part_1[test_index1_1, ]
      train_index_0 <- unlist(folds_0[-i])
      test_index_0 <- unlist(folds_0[i])
      train_data_0 <- df_part_0[train_index_0, ]
      test_data_0 <- df_part_0[test_index_0, ]
      
      train_index0_0 <- unlist(folds0_0[-i])
      test_index0_0 <- unlist(folds0_0[i])
      train_data0_0 <- df_part_0[train_index0_0, ]
      test_data0_0 <- df_part_0[test_index0_0, ]
      
      train_index1_0 <-unlist(folds1_0[-i])
      test_index1_0 <- unlist(folds1_0[i])
      train_data1_0 <- df_part_0[train_index1_0, ]
      test_data1_0 <- df_part_0[test_index1_0, ]
      
      # Fit polynomial regression
      if (degree == 0) {
        formula_D <- as.formula("D ~ 1")  # Model with only an intercept
        formula_YE <- as.formula("YE ~ 1")
        formula_YN <- as.formula("YN ~ 1")
        
        model_D_1 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_1)
        model_YE0_1 <- lm(formula_YE, data = train_data0_1)
        model_YE1_1 <- lm(formula_YE, data = train_data1_1)
        model_YN0_1 <- lm(formula_YN, data = train_data0_1)
        model_YN1_1 <- lm(formula_YN, data = train_data1_1)
        
        model_D_0 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_0)
        model_YE0_0 <- lm(formula_YE, data = train_data0_0)
        model_YE1_0 <- lm(formula_YE, data = train_data1_0)
        model_YN0_0 <- lm(formula_YN, data = train_data0_0)
        model_YN1_0 <- lm(formula_YN, data = train_data1_0)
        
      } else {
        formula_D <- as.formula(paste("D ~ poly(s,", degree, ", raw=TRUE)"))
        formula_YE <- as.formula(paste("YE ~ poly(s,", degree, ", raw=TRUE)"))
        formula_YN <- as.formula(paste("YN ~ poly(s,", degree, ", raw=TRUE)"))
        
        model_D_1 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_1)
        model_YE0_1 <- lm(formula_YE, data = train_data0_1)
        model_YE1_1 <- lm(formula_YE, data = train_data1_1)
        model_YN0_1 <- lm(formula_YN, data = train_data0_1)
        model_YN1_1 <- lm(formula_YN, data = train_data1_1)
        
        model_D_0 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_0)
        model_YE0_0 <- lm(formula_YE, data = train_data0_0)
        model_YE1_0 <- lm(formula_YE, data = train_data1_0)
        model_YN0_0 <- lm(formula_YN, data = train_data0_0)
        model_YN1_0 <- lm(formula_YN, data = train_data1_0)
      }
      
      # Predict on test set
      preds_D_1 <- predict(model_D_1, type = "response", newdata = test_data_1)
      preds_YE0_1 <- predict(model_YE0_1, newdata = test_data0_1)
      preds_YE1_1 <- predict(model_YE1_1, newdata = test_data1_1)
      preds_YN0_1 <- predict(model_YN0_1, newdata = test_data0_1)
      preds_YN1_1 <- predict(model_YN1_1, newdata = test_data1_1)
      
      # Predict on test set
      preds_D_0 <- predict(model_D_0, type = "response", newdata = test_data_0)
      preds_YE0_0 <- predict(model_YE0_0, newdata = test_data0_0)
      preds_YE1_0 <- predict(model_YE1_0, newdata = test_data1_0)
      preds_YN0_0 <- predict(model_YN0_0, newdata = test_data0_0)
      preds_YN1_0 <- predict(model_YN1_0, newdata = test_data1_0)
      
      # Compute mean squared error
      errors_D_1 <- c(errors_D_1, mean((preds_D_1 - test_data_1$D)^2))
      errors_YE0_1 <- c(errors_YE0_1, mean((preds_YE0_1 - test_data0_1$YE)^2))
      errors_YE1_1 <- c(errors_YE1_1, mean((preds_YE1_1 - test_data1_1$YE)^2))
      errors_YN0_1 <- c(errors_YN0_1, mean((preds_YN0_1 - test_data0_1$YN)^2))
      errors_YN1_1 <- c(errors_YN1_1, mean((preds_YN1_1 - test_data1_1$YN)^2))
      
      errors_D_0 <- c(errors_D_0, mean((preds_D_0 - test_data_0$D)^2))
      errors_YE0_0 <- c(errors_YE0_0, mean((preds_YE0_0 - test_data0_0$YE)^2))
      errors_YE1_0 <- c(errors_YE1_0, mean((preds_YE1_0 - test_data1_0$YE)^2))
      errors_YN0_0 <- c(errors_YN0_0, mean((preds_YN0_0 - test_data0_0$YN)^2))
      errors_YN1_0 <- c(errors_YN1_0, mean((preds_YN1_0 - test_data1_0$YN)^2))
    }
    
    return(c(mean(errors_D_1), mean(errors_YE0_1), mean(errors_YE1_1), mean(errors_YN0_1), mean(errors_YN1_1),
             mean(errors_D_0), mean(errors_YE0_0), mean(errors_YE1_0), mean(errors_YN0_0), mean(errors_YN1_0)))  # Return average CV error
  }
  
  # Evaluate cross-validation error for each polynomial degree
  cv_results <- sapply(0:max_degree, cv_error)
  
  best_degree_D_1 <- which.min(cv_results[1,]) - 1
  best_degree_YE0_1 <- which.min(cv_results[2,]) - 1
  best_degree_YE1_1 <- which.min(cv_results[3,]) - 1
  best_degree_YN0_1 <- which.min(cv_results[4,]) - 1
  best_degree_YN1_1 <- which.min(cv_results[5,]) - 1
  
  best_degree_D_0 <- which.min(cv_results[6,]) - 1
  best_degree_YE0_0 <- which.min(cv_results[7,]) - 1
  best_degree_YE1_0 <- which.min(cv_results[8,]) - 1
  best_degree_YN0_0 <- which.min(cv_results[9,]) - 1
  best_degree_YN1_0 <- which.min(cv_results[10,]) - 1
  
  if (best_degree_D_1 == 0) {
    logit_model_1 <- glm(D ~ 1, family = binomial(link = "logit"), data = df_part_1)
  } else {
    formula_D <- as.formula(paste("D ~ poly(s,", best_degree_D_1, ", raw=TRUE)"))
    logit_model_1 <- glm(formula_D, family = binomial(link = "logit"), data = df_part_1)
  }
  
  if (best_degree_D_0 == 0) {
    logit_model_0 <- glm(D ~ 1, family = binomial(link = "logit"), data = df_part_0)
  } else {
    formula_D <- as.formula(paste("D ~ poly(s,", best_degree_D_0, ", raw=TRUE)"))
    logit_model_0 <- glm(formula_D, family = binomial(link = "logit"), data = df_part_0)
  }
  
  h_s_1 <- predict(logit_model_0, type = "response", newdata = df_part_1)
  h_s_0 <- predict(logit_model_1, type = "response", newdata = df_part_0)
  
  #Subset data
  training_data1_1 <- subset(df_part_1, D == 1)
  training_data0_1 <- subset(df_part_1, D == 0)
  
  training_data1_0 <- subset(df_part_0, D == 1)
  training_data0_0 <- subset(df_part_0, D == 0)
  
  if (best_degree_YE0_1 == 0) {
    YE0_model_1 <- lm(YE ~ 1, data = training_data0_1)
  } else {
    YE0_model_1 <- lm(YE ~ poly(s, degree = best_degree_YE0_1), data = training_data0_1)
  }
  
  if (best_degree_YE0_0 == 0) {
    YE0_model_0 <- lm(YE ~ 1, data = training_data0_0)
  } else {
    YE0_model_0 <- lm(YE ~ poly(s, degree = best_degree_YE0_0), data = training_data0_0)
  }
  
  if (best_degree_YE1_1 == 0) {
    YE1_model_1 <- lm(YE ~ 1, data = training_data1_1)
  } else {
    YE1_model_1 <- lm(YE ~ poly(s, degree = best_degree_YE1_1), data = training_data1_1)
  }
  
  if (best_degree_YE1_0 == 0) {
    YE1_model_0 <- lm(YE ~ 1, data = training_data1_0)
  } else {
    YE1_model_0 <- lm(YE ~ poly(s, degree = best_degree_YE1_0), data = training_data1_0)
  }
  
  if (best_degree_YN0_1 == 0) {
    YN0_model_1 <- lm(YN ~ 1, data = training_data0_1)
  } else {
    YN0_model_1 <- lm(YN ~ poly(s, degree = best_degree_YN0_1), data = training_data0_1)
  }
  
  if (best_degree_YN0_0 == 0) {
    YN0_model_0 <- lm(YN ~ 1, data = training_data0_0)
  } else {
    YN0_model_0 <- lm(YN ~ poly(s, degree = best_degree_YN0_0), data = training_data0_0)
  }
  
  if (best_degree_YN1_1 == 0) {
    YN1_model_1 <- lm(YN ~ 1, data = training_data1_1)
  } else {
    YN1_model_1 <- lm(YE ~ poly(s, degree = best_degree_YN1_1), data = training_data1_1)
  }
  
  if (best_degree_YN1_0 == 0) {
    YN1_model_0 <- lm(YN ~ 1, data = training_data1_0)
  } else {
    YN1_model_0 <- lm(YE ~ poly(s, degree = best_degree_YN1_0), data = training_data1_0)
  }
  
  # Use the fitted model to predict values for the whole dataset
  E_YE0_s_1 <- predict(YE0_model_0, newdata = df_part_1)
  E_YE1_s_1 <- predict(YE1_model_0, newdata = df_part_1)
  E_YN0_s_1 <- predict(YN0_model_0, newdata = df_part_1)
  E_YN1_s_1 <- predict(YN1_model_0, newdata = df_part_1)
  
  E_YE0_s_0 <- predict(YE0_model_1, newdata = df_part_0)
  E_YE1_s_0 <- predict(YE1_model_1, newdata = df_part_0)
  E_YN0_s_0 <- predict(YN0_model_1, newdata = df_part_0)
  E_YN1_s_0 <- predict(YN1_model_1, newdata = df_part_0)
  
  ## Etape 1: Define the function m
  
  ### Etape 1-1: Define the function phi
  #'@noRd
  phi <- function(s, lambda){
    val <- 1 - lambda[2] + s*(1-s)*(lambda[2]*lambda[2] - lambda[3]*lambda[3])
    # Empêche la division par une valeur trop proche de 0
    sign_val <- ifelse(val >= 0, 1, -1)
    ifelse(abs(val) < 1e-6, sign_val * 1e-6, val)
  }
  ### Etape 1-2: Define the conditional treatment effect functions
  #'@noRd
  Num_condi_TE <- function(s, lambda){
    mE <- lambda[1] * (1 - lambda[2] * (1-s)) * (s > 0)
    mN <- lambda[1] * lambda[3] * s * (s < 1)
    m <- return(cbind(mE, mN))
  }
  
  ### Etape 1-3: Define the final function m (dimension G x 2)
  #'@noRd
  moment_function <- function(lambda, Z) {
    YE <- Z[, 1]
    YN <- Z[, 2]
    D <- Z[, 3]
    s <- Z[, 4]
    h_s <- Z[, 5]
    h_s_clipped <- pmin(pmax(h_s, 1e-4), 1 - 1e-4)  # évite 0 et 1 exacts
    denom_ipw <- h_s_clipped * (1 - h_s_clipped)
    
    
    IPW_YE <- (((D - h_s_clipped)*YE)/(denom_ipw)) * (s > 0)
    IPW_YN <- (((D - h_s_clipped)*YN)/(denom_ipw)) * (s < 1)
    
    m <- cbind(IPW_YE, IPW_YN) - (Num_condi_TE(s, lambda)/phi(s, lambda))
    return(m)
  }
  
  ## Etape 1-3: Define the GMM objective function
  #'@noRd
  gmm1_objective <- function(params, Z) {
    # We recover m
    m <- moment_function(params, Z)
    
    # We create 4 empirical moment conditions
    s <- Z[, 4]
    residuals <- cbind(m, s * m, s*s * m, log(s+0.0001) * m, sqrt(s) * m, exp(s) * m, (1./(s+0.1))*m)
    if (!all(is.finite(residuals))) return(1e10)  # pénalité finie, pas de crash
    emp_moments <- colMeans(residuals)
    val <- as.numeric(t(emp_moments) %*% emp_moments)
    
    if (!is.finite(val)) return(1e10)
    val
  }
  
  Z_1 <- cbind(YE_part_1, YN_part_1, D_part_1, s_part_1, h_s_1)  # Combine data
  Z_0 <- cbind(YE_part_0, YN_part_0, D_part_0, s_part_0, h_s_0)  # Combine data
  
  lambda_init <- c(0, 0.1, 0.1)
  
  opt_1 <- optim(
    lambda_init,
    fn = gmm1_objective,
    Z = Z_1,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda1_1 <- opt_1$par
  
  opt_0 <- optim(
    lambda_init,
    fn = gmm1_objective,
    Z = Z_0,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda1_0 <- opt_0$par
  
  lambda1 <- (G_1/G)*lambda1_1 + (G_0/G)*lambda1_0
  
  # Etape 2 : estimation des poids
  
  ## Etape 2-1 : function to have M
  #'@noRd
  Mi <- function(s, lambda){
    
    denom <- phi(s, lambda)
    
    Mi_E1 <- (s > 0) * ((1-lambda[2]*(1-s))/denom)
    Mi_E2 <- (s > 0) * (((-1) * lambda[1] * ((1-s)*denom + (1 - lambda[2] * (1 - s)) * (2 * s * (1-s) * lambda[2] - 1)))/(denom*denom))
    Mi_E3 <- (s > 0) * ((lambda[1] * (1 - lambda[2]*(1-s)) * 2 * s * (1-s) * lambda[3])/(denom*denom))
    
    Mi_N1 <- (s < 1) * ((lambda[3] * s)/denom)
    Mi_N2 <- (s < 1) * (((-1) * lambda[1] * lambda[3] * s * (2 * s * (1-s) * lambda[2] - 1))/(denom*denom))
    Mi_N3 <- (s < 1) * ((lambda[1] * s * denom + lambda[1] * s * s * (1-s) * 2 * lambda[3] * lambda[3])/(denom * denom))
    
    Mi <- (-1) * cbind(c(Mi_E1, Mi_N1), c(Mi_E2, Mi_N2), c(Mi_E3, Mi_N3))
    
    return(Mi)
    
  }
  
  ## Etape 2-1 : function to estimate alpha
  #'@noRd
  alpha_i <- function(s, D, p_score, E_YE1_s, E_YN1_s, E_YE0_s, E_YN0_s){
    
    alpha_1 <- ((E_YE1_s/p_score) +  (E_YE0_s/(1-p_score)))
    alpha_2 <- ((E_YN1_s/p_score) +  (E_YN0_s/(1-p_score)))
    
    alpha <- (-1) * cbind(alpha_1, alpha_2)
    
    return(alpha)
  }
  
  ###### Case with no clustering #####
  
  ## Nonparametric estimation of the variance of residuals
  
  # On recupere les variances de rhoE, rhoN et Cov(rhoE, rhoN)
  rho_1 <- moment_function(lambda1, Z_1)
  rho_0 <- moment_function(lambda1, Z_0)
  
  df_part_1$rho_trho_E <- rho_1[,1]*rho_1[,1]
  df_part_1$rho_trho_NE <- rho_1[,1]*rho_1[,2]
  df_part_1$rho_trho_N <- rho_1[,2]*rho_1[,2]
  
  df_part_0$rho_trho_E <- rho_0[,1]*rho_0[,1]
  df_part_0$rho_trho_NE <- rho_0[,1]*rho_0[,2]
  df_part_0$rho_trho_N <- rho_0[,2]*rho_0[,2]
  
  # Create folds for cross-validation
  folds_Sigma_1 <- caret::createFolds(df_part_1$s, k = k_folds, list = TRUE)
  folds_Sigma_0 <- caret::createFolds(df_part_0$s, k = k_folds, list = TRUE)
  
  # Function to compute cross-validation error for a given degree
  #'@noRd
  cv_error_Sigma <- function(degree) {
    
    errors_1 <- c()
    errors_0 <- c()
    
    for (i in 1:k_folds) {
      # Split into training and validation sets
      train_idx_0 <- unlist(folds_Sigma_0[-i])
      test_idx_0 <- unlist(folds_Sigma_0[i])
      
      train_data_0 <- df_part_0[train_idx_0, ]
      test_data_0 <- df_part_0[test_idx_0, ]
      
      train_idx_1 <- unlist(folds_Sigma_1[-i])
      test_idx_1 <- unlist(folds_Sigma_1[i])
      
      train_data_1 <- df_part_1[train_idx_1, ]
      test_data_1 <- df_part_1[test_idx_1, ]
      
      # Define formula based on polynomial degree
      if (degree == 0) {
        formula <- as.formula("cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ 1")  # Model with only an intercept
      } else {
        formula <- as.formula(paste("cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ poly(s,", degree, ", raw=TRUE)"))
      }
      
      # Fit regression model
      model_0 <- lm(formula, data = train_data_0)
      model_1 <- lm(formula, data = train_data_1)
      
      # Predict on test set
      preds_0 <- predict(model_0, newdata = test_data_0)
      preds_1 <- predict(model_1, newdata = test_data_1)
      
      # Compute Mean Squared Error (MSE) for each outcome
      mseE_1  <- mean((preds_1[, 1] - test_data_1$rho_trho_E)^2)
      mseNE_1 <- mean((preds_1[, 2] - test_data_1$rho_trho_NE)^2)
      mseN_1  <- mean((preds_1[, 3] - test_data_1$rho_trho_N)^2)
      
      mseE_0  <- mean((preds_0[, 1] - test_data_0$rho_trho_E)^2)
      mseNE_0 <- mean((preds_0[, 2] - test_data_0$rho_trho_NE)^2)
      mseN_0  <- mean((preds_0[, 3] - test_data_0$rho_trho_N)^2)
      
      # Store the average MSE across outcomes
      errors_1 <- c(errors_1, mean(c(mseE_1, mseNE_1, mseN_1)))
      errors_0 <- c(errors_0, mean(c(mseE_0, mseNE_0, mseN_0)))
    }
    
    return(c(mean(errors_1), mean(errors_0)))  # Return average CV error
  }
  
  # Evaluate cross-validation error for each polynomial degree (including 0)
  cv_results_Sigma <- sapply(0:20, cv_error_Sigma)
  
  # Find the best polynomial degree (minimizing CV error)
  best_degree_Sigma_1 <- which.min(cv_results[1,]) - 1  # Adjust index to match degree
  best_degree_Sigma_0 <- which.min(cv_results[2,]) - 1  # Adjust index to match degree
  
  if (best_degree_Sigma_0 == 0) {
    model_V_s_0 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ 1, data = df_part_0)
  } else {
    model_V_s_0 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ poly(s, degree = best_degree_Sigma_0), data = df_part_0)
  }
  
  if (best_degree_Sigma_1 == 0) {
    model_V_s_1 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ 1, data = df_part_1)
  } else {
    model_V_s_1 <- lm(cbind(rho_trho_E, rho_trho_NE, rho_trho_N) ~ poly(s, degree = best_degree_Sigma_1), data = df_part_1)
  }
  
  #Use the fitted model to predict values for the whole dataset
  predict_V_s_1 <- predict(model_V_s_0, newdata = df_part_1)
  predict_V_s_0 <- predict(model_V_s_1, newdata = df_part_0)
  
  alpha_1 <- alpha_i(s_part_1, D_part_1, h_s_1, E_YE1_s_1, E_YN1_s_1, E_YE0_s_1, E_YN0_s_1)
  alpha_0 <- alpha_i(s_part_0, D_part_0, h_s_0, E_YE1_s_0, E_YN1_s_0, E_YE0_s_0, E_YN0_s_0)
  
  # Apply the function to each row
  w_hat_list_1 <- lapply(1:G_1, function(i) t(Mi(s_part_1[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_1[i,1], predict_V_s_1[i,2], predict_V_s_1[i,2], predict_V_s_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol))
  w_hat_list_0 <- lapply(1:G_0, function(i) t(Mi(s_part_0[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_0[i,1], predict_V_s_0[i,2], predict_V_s_0[i,2], predict_V_s_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol))
  
  # Combine results into a 3D array
  w_hat_array_1 <- array(unlist(w_hat_list_1), dim = c(3, 2, G_1))
  w_hat_array_0 <- array(unlist(w_hat_list_0), dim = c(3, 2, G_0))
  
  # Apply the function to each row
  V_ind_list_1 <- lapply(1:G_1, function(i) t(Mi(s_part_1[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_1[i,1], predict_V_s_1[i,2], predict_V_s_1[i,2], predict_V_s_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol) %*% Mi(s_part_1[i], lambda1))
  V_ind_list_0 <- lapply(1:G_0, function(i) t(Mi(s_part_0[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_0[i,1], predict_V_s_0[i,2], predict_V_s_0[i,2], predict_V_s_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol) %*% Mi(s_part_0[i], lambda1))
  
  # Combine results into a 3D array
  V_ind_array_1 <- array(unlist(V_ind_list_1), dim = c(3, 3, G_1))
  V_ind_array_0 <- array(unlist(V_ind_list_0), dim = c(3, 3, G_0))
  
  # Sum the matrices
  V_a_1 <- apply(V_ind_array_1, c(1, 2), mean)
  V_a_0 <- apply(V_ind_array_0, c(1, 2), mean)
  
  # Step 3: Define the GMM objective function
  #'@noRd
  gmm2_objective <- function(params, Z, w, V, G) {
    # Calcul des rho + mise sous forme de vecteurs
    residuals <- moment_function(params, Z)
    if (!all(is.finite(residuals))) return(1e10)
    
    residuals_vector_list <- lapply(1:G, function(i) matrix(residuals[i, ], ncol = 1))
    
    
    # Step 3: Multiply each 5x2 matrix with the corresponding 2x1 vector
    w_rho_list <- mapply(function(M, v) M %*% v, w, residuals_vector_list, SIMPLIFY = FALSE)
    
    
    # Step 4: Combine results into an G x 5 matrix
    w_rho_matrix <- do.call(rbind, lapply(w_rho_list, as.vector))
    if (!all(is.finite(w_rho_matrix))) return(1e10)
    
    # Element-wise multiplication and summation
    Vinv <- tryCatch(solve(V), error = function(e) MASS::ginv(V))
    moments <- t(colMeans(w_rho_matrix)) %*% Vinv %*% colMeans(w_rho_matrix)
    if (!is.finite(moments)) return(1e10)
    return(moments)
  }
  
  
  opt2_1 <- optim(
    par = lambda1,
    fn = gmm2_objective,
    Z = Z_1,
    G = G_1,
    w = w_hat_list_1,
    V = V_a_1,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  
  lambda2_1 <- opt2_1$par
  
  
  opt2_0 <- optim(
    par = lambda1,
    fn = gmm2_objective,
    Z = Z_0,
    G = G_0,
    V = V_a_0,
    w = w_hat_list_0,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda2_0 <- opt2_0$par
  
  lambda2 <- (G_1/G)*lambda2_1 + (G_0/G)*lambda2_0
  
  # Computing standard errors
  
  # On recupere les variances de rhoE, rhoN et Cov(rhoE, rhoN)
  rho_2_1 <- moment_function(lambda2, Z_1)
  df_part_1$rho_trho_E2 <- rho_2_1[,1]*rho_2_1[,1]
  df_part_1$rho_trho_NE2 <- rho_2_1[,1]*rho_2_1[,2]
  df_part_1$rho_trho_N2 <- rho_2_1[,2]*rho_2_1[,2]
  
  rho_2_0 <- moment_function(lambda2, Z_0)
  df_part_0$rho_trho_E2 <- rho_2_0[,1]*rho_2_0[,1]
  df_part_0$rho_trho_NE2 <- rho_2_0[,1]*rho_2_0[,2]
  df_part_0$rho_trho_N2 <- rho_2_0[,2]*rho_2_0[,2]
  
  if (best_degree_Sigma_0 == 0) {
    predict_V_s2_1 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ 1, data = df_part_0), newdata = df_part_1)
  } else {
    predict_V_s2_1 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ poly(s, degree = best_degree_Sigma_0), data = df_part_0), newdata = df_part_1)
  }
  
  if (best_degree_Sigma_1 == 0) {
    predict_V_s2_0 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ 1, data = df_part_1), newdata = df_part_0)
  } else {
    predict_V_s2_0 <- predict(lm(cbind(rho_trho_E2, rho_trho_NE2, rho_trho_N2) ~ poly(s, degree = best_degree_Sigma_1), data = df_part_1), newdata = df_part_0)
  }
  
  # Apply the function to each row
  V_ind_list_1 <- lapply(1:G_1, function(i) t(Mi(s_part_1[i], lambda2)) %*% MASS::ginv(matrix(c(predict_V_s2_1[i,1], predict_V_s2_1[i,2], predict_V_s2_1[i,2], predict_V_s2_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol) %*% Mi(s_part_1[i], lambda2))
  V_ind_list_0 <- lapply(1:G_0, function(i) t(Mi(s_part_0[i], lambda2)) %*% MASS::ginv(matrix(c(predict_V_s2_0[i,1], predict_V_s2_0[i,2], predict_V_s2_0[i,2], predict_V_s2_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol) %*% Mi(s_part_0[i], lambda2))
  
  
  # Combine results into a 3D array
  V_ind_array_1 <- array(unlist(V_ind_list_1), dim = c(3, 3, G_1))
  V_ind_array_0 <- array(unlist(V_ind_list_0), dim = c(3, 3, G_0))
  
  # Sum the matrices
  V_a_1 <- apply(V_ind_array_1, c(1, 2), mean)
  V_a_0 <- apply(V_ind_array_0, c(1, 2), mean)
  
  V_a <- (G_1/G)^2 * V_a_1 + (G_0/G)^2 * V_a_0
  nb_groups <- G
  Vainv <- tryCatch(solve(V_a), error = function(e) MASS::ginv(V_a))
  standard_errors <- sqrt(Vainv/nb_groups)
  
  t_stat <- lambda2 / standard_errors
  df <- nb_groups - 3 - 1
  
  #bilateral test
  
  p_value <- 2 * (1 - pt(abs(t_stat), df))
  # Construction du dataframe
  result_df <- data.frame(
    Statistic = c("Estimate", "Standard Error", "t_test","p_value"),
    `delta` = c(round(lambda2[1],3), round(standard_errors[1],3),round(t_stat[1],3),round(p_value[1],3)),
    `thetaW` = c(round(lambda2[2],3), round(standard_errors[2],3),round(t_stat[2],3),round(p_value[2],3)),
    `thetaB` = c(round(lambda2[3],3), round(standard_errors[3],3),round(t_stat[3],3),round(p_value[3],3))
  )
  
  ft <- flextable::flextable(result_df) 
  ft <- flextable::set_header_labels(ft,
      delta = "Delta (\u0394)",
      thetaW = "ThetaW (\u03B8w)",
      thetaB = "ThetaB (\u03B8b)"
    )
  
  # Fonction treatment effect
  #'@noRd
  tau_E <- function(s, lambda2) {
    return(lambda2[1] * (1 + s * ((lambda2[2] - (1 - s) * (lambda2[2]^2 - lambda2[3]^2)) /
                                    (1 - lambda2[2] + s * (1 - s) * (lambda2[2]^2 - lambda2[3]^2)))))
  }
  
  #'@noRd
  tau_N <- function(s, lambda2){
    return (lambda2[1] * lambda2[3] * s / (1 - lambda2[2] + s * (1 - s) * (lambda2[2]^2 - lambda2[3]^2)))
  }
  
  s_values <- seq(0, 1, by = 0.01)
  tau_E_values <- sapply(s_values, tau_E,lambda2=lambda2)
  tau_N_values <- sapply(s_values, tau_N,lambda2=lambda2)
  tau_pop_values <- s_values * tau_E_values + (1 - s_values) * tau_N_values
  
  df <- data.frame(
    s = rep(s_values, 3),
    tau = c(tau_E_values, tau_N_values, tau_pop_values),
    Type = rep(c("Eligible", "Non-eligible", "Average"), each = length(s_values))
  )
  
  # Tracer avec ggplot2
  p <-ggplot2::ggplot(df, ggplot2::aes(x = s, y = tau, color = Type))+
          ggplot2::geom_line(linewidth = 1.2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "Treatment effects on eligibles and non-eligibles", x = "s", y = "Total Effect") +
          ggplot2::scale_color_manual(values = c("black", "blue", "red")) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
            axis.title = ggplot2::element_text(size = 14),
            axis.text = ggplot2::element_text(size = 12)
          )
  
  
  return(list(estimates = result_df, table = ft, plot = p))
}


# Case 3 : identity orthogonal to eligibility, 5 parameters, 
#'@noRd
ortho_unknown_prop_score_5param_gmm <- function(YM, YF, D, sM, sEM, sEF,  tol = 1e-6) {
  
  G <- length(sM)
  
  # Etape 0 : Divide the sample into 2
  cross_fit_indic <- rbinom(G, 1, 0.5)
  G_1 <- sum(cross_fit_indic == 1)
  G_0 <- sum(cross_fit_indic == 0)
  
  # Look at the 2 sub-samples
  YM_part_1 <- YM[cross_fit_indic == 1]
  YF_part_1 <- YF[cross_fit_indic == 1]
  D_part_1 <- D[cross_fit_indic == 1]
  sM_part_1 <- sM[cross_fit_indic == 1]
  sEM_part_1 <- sEM[cross_fit_indic == 1]
  sEF_part_1 <- sEF[cross_fit_indic == 1]
  
  YM_part_0 <- YM[cross_fit_indic == 0]
  YF_part_0 <- YF[cross_fit_indic == 0]
  D_part_0 <- D[cross_fit_indic == 0]
  sM_part_0 <- sM[cross_fit_indic == 0]
  sEM_part_0 <- sEM[cross_fit_indic == 0]
  sEF_part_0 <- sEF[cross_fit_indic == 0]
  
  df_part_1 <- data.frame(YM = YM_part_1, YF = YF_part_1, D = D_part_1, sM = sM_part_1, sEM = sEM_part_1, sEF = sEF_part_1)
  df_part_0 <- data.frame(YM = YM_part_0, YF = YF_part_0, D = D_part_0, sM = sM_part_0, sEM = sEM_part_0, sEF = sEF_part_0)
  
  max_degree <- 5
  k_folds <- 4
  
  # Create folds for cross-validation
  folds_1 <- caret::createFolds(df_part_1$D, k = k_folds, list = TRUE)
  folds1_1 <- caret::createFolds(df_part_1$YM[which(df_part_1$D == 1)], k = k_folds, list = TRUE)
  folds0_1 <- caret::createFolds(df_part_1$YM[which(df_part_1$D == 0)], k = k_folds, list = TRUE)
  
  folds_0 <- caret::createFolds(df_part_0$D, k = k_folds, list = TRUE)
  folds1_0 <- caret::createFolds(df_part_0$YM[which(df_part_0$D == 1)], k = k_folds, list = TRUE)
  folds0_0 <- caret::createFolds(df_part_0$YM[which(df_part_0$D == 0)], k = k_folds, list = TRUE)
  
  # Function to compute cross-validation error for a given degree
  #'@noRd
  cv_error <- function(degree) {
    
    errors_D_1 <- c()
    errors_YM0_1 <- c()
    errors_YM1_1 <- c()
    errors_YF0_1 <- c()
    errors_YF1_1 <- c()
    
    errors_D_0 <- c()
    errors_YM0_0 <- c()
    errors_YM1_0 <- c()
    errors_YF0_0 <- c()
    errors_YF1_0 <- c()
    
    for (i in 1:k_folds) {
      # Split into training and validation sets
      train_index_1 <- unlist(folds_1[-i])
      test_index_1 <- unlist(folds_1[i])
      train_data_1 <- df_part_1[train_index_1, ]
      test_data_1 <- df_part_1[test_index_1, ]
      train_index0_1 <- unlist(folds0_1[-i])
      test_index0_1 <- unlist(folds0_1[i])
      train_data0_1 <- df_part_1[train_index0_1, ]
      test_data0_1 <- df_part_1[test_index0_1, ]
      
      train_index1_1 <-unlist(folds1_1[-i])
      test_index1_1 <- unlist(folds1_1[i])
      train_data1_1 <- df_part_1[train_index1_1, ]
      test_data1_1 <- df_part_1[test_index1_1, ]
      train_index_0 <- unlist(folds_0[-i])
      test_index_0 <- unlist(folds_0[i])
      train_data_0 <- df_part_0[train_index_0, ]
      test_data_0 <- df_part_0[test_index_0, ]
      
      train_index0_0 <- unlist(folds0_0[-i])
      test_index0_0 <- unlist(folds0_0[i])
      train_data0_0 <- df_part_0[train_index0_0, ]
      test_data0_0 <- df_part_0[test_index0_0, ]
      
      train_index1_0 <-unlist(folds1_0[-i])
      test_index1_0 <- unlist(folds1_0[i])
      train_data1_0 <- df_part_0[train_index1_0, ]
      test_data1_0 <- df_part_0[test_index1_0, ]
      
      # Fit polynomial regression
      if (degree == 0) {
        formula_D <- as.formula("D ~ 1")  # Model with only an intercept
        formula_YM <- as.formula("YM ~ 1")
        formula_YF <- as.formula("YF ~ 1")
        
        model_D_1 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_1)
        model_YM0_1 <- lm(formula_YM, data = train_data0_1)
        model_YM1_1 <- lm(formula_YM, data = train_data1_1)
        model_YF0_1 <- lm(formula_YF, data = train_data0_1)
        model_YF1_1 <- lm(formula_YF, data = train_data1_1)
        
        model_D_0 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_0)
        model_YM0_0 <- lm(formula_YM, data = train_data0_0)
        model_YM1_0 <- lm(formula_YM, data = train_data1_0)
        model_YF0_0 <- lm(formula_YF, data = train_data0_0)
        model_YF1_0 <- lm(formula_YF, data = train_data1_0)
        
      } else {
        formula_D <- as.formula(paste("D ~ (poly(sM, ", degree, ", raw = TRUE) +",
                                      "poly(sEM, ", degree, ", raw = TRUE) +",
                                      "poly(sEF, ", degree, ", raw = TRUE))^2"))
        formula_YM <- as.formula(paste("YM ~ (poly(sM, ", degree, ", raw = TRUE) +",
                                       "poly(sEM, ", degree, ", raw = TRUE) +",
                                       "poly(sEF, ", degree, ", raw = TRUE))^2"))
        formula_YF <- as.formula(paste("YF ~ (poly(sM, ", degree, ", raw = TRUE) +",
                                       "poly(sEM, ", degree, ", raw = TRUE) +",
                                       "poly(sEF, ", degree, ", raw = TRUE))^2"))
        
        model_D_1 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_1)
        model_YM0_1 <- lm(formula_YM, data = train_data0_1)
        model_YM1_1 <- lm(formula_YM, data = train_data1_1)
        model_YF0_1 <- lm(formula_YF, data = train_data0_1)
        model_YF1_1 <- lm(formula_YF, data = train_data1_1)
        
        model_D_0 <- glm(formula_D, family = binomial(link = "logit") , data = train_data_0)
        model_YM0_0 <- lm(formula_YM, data = train_data0_0)
        model_YM1_0 <- lm(formula_YM, data = train_data1_0)
        model_YF0_0 <- lm(formula_YF, data = train_data0_0)
        model_YF1_0 <- lm(formula_YF, data = train_data1_0)
      }
      
      # Predict on test set
      preds_D_1 <- predict(model_D_1, type = "response", newdata = test_data_1)
      preds_YM0_1 <- predict(model_YM0_1, newdata = test_data0_1)
      preds_YM1_1 <- predict(model_YM1_1, newdata = test_data1_1)
      preds_YF0_1 <- predict(model_YF0_1, newdata = test_data0_1)
      preds_YF1_1 <- predict(model_YF1_1, newdata = test_data1_1)
      
      # Predict on test set
      preds_D_0 <- predict(model_D_0, type = "response", newdata = test_data_0)
      preds_YM0_0 <- predict(model_YM0_0, newdata = test_data0_0)
      preds_YM1_0 <- predict(model_YM1_0, newdata = test_data1_0)
      preds_YF0_0 <- predict(model_YF0_0, newdata = test_data0_0)
      preds_YF1_0 <- predict(model_YF1_0, newdata = test_data1_0)
      
      # Compute mean squared error
      errors_D_1 <- c(errors_D_1, mean((preds_D_1 - test_data_1$D)^2))
      errors_YM0_1 <- c(errors_YM0_1, mean((preds_YM0_1 - test_data0_1$YM)^2))
      errors_YM1_1 <- c(errors_YM1_1, mean((preds_YM1_1 - test_data1_1$YM)^2))
      errors_YF0_1 <- c(errors_YF0_1, mean((preds_YF0_1 - test_data0_1$YF)^2))
      errors_YF1_1 <- c(errors_YF1_1, mean((preds_YF1_1 - test_data1_1$YF)^2))
      
      errors_D_0 <- c(errors_D_0, mean((preds_D_0 - test_data_0$D)^2))
      errors_YM0_0 <- c(errors_YM0_0, mean((preds_YM0_0 - test_data0_0$YM)^2))
      errors_YM1_0 <- c(errors_YM1_0, mean((preds_YM1_0 - test_data1_0$YM)^2))
      errors_YF0_0 <- c(errors_YF0_0, mean((preds_YF0_0 - test_data0_0$YF)^2))
      errors_YF1_0 <- c(errors_YF1_0, mean((preds_YF1_0 - test_data1_0$YF)^2))
    }
    
    return(c(mean(errors_D_1), mean(errors_YM0_1), mean(errors_YM1_1), mean(errors_YF0_1), mean(errors_YF1_1),
             mean(errors_D_0), mean(errors_YM0_0), mean(errors_YM1_0), mean(errors_YF0_0), mean(errors_YF1_0)))  # Return average CV error
  }
  
  # Evaluate cross-validation error for each polynomial degree
  cv_results <- sapply(0:max_degree, cv_error)
  
  best_degree_D_1 <- which.min(cv_results[1,]) - 1
  best_degree_YM0_1 <- which.min(cv_results[2,]) - 1
  best_degree_YM1_1 <- which.min(cv_results[3,]) - 1
  best_degree_YF0_1 <- which.min(cv_results[4,]) - 1
  best_degree_YF1_1 <- which.min(cv_results[5,]) - 1
  
  best_degree_D_0 <- which.min(cv_results[6,]) - 1
  best_degree_YM0_0 <- which.min(cv_results[7,]) - 1
  best_degree_YM1_0 <- which.min(cv_results[8,]) - 1
  best_degree_YF0_0 <- which.min(cv_results[9,]) - 1
  best_degree_YF1_0 <- which.min(cv_results[10,]) - 1
  
  if (best_degree_D_1 == 0) {
    logit_model_1 <- glm(D ~ 1, family = binomial(link = "logit"), data = df_part_1)
  } else {
    formula_D <- as.formula(paste("D ~ (poly(sM, ", best_degree_D_1, ", raw = TRUE) +",
                                  "poly(sEM, ", best_degree_D_1, ", raw = TRUE) +",
                                  "poly(sEF, ", best_degree_D_1, ", raw = TRUE))^2"))
    logit_model_1 <- glm(formula_D, family = binomial(link = "logit"), data = df_part_1)
  }
  
  if (best_degree_D_0 == 0) {
    logit_model_0 <- glm(D ~ 1, family = binomial(link = "logit"), data = df_part_0)
  } else {
    formula_D <- as.formula(paste("D ~ (poly(sM, ", best_degree_D_0, ", raw = TRUE) +",
                                  "poly(sEM, ", best_degree_D_0, ", raw = TRUE) +",
                                  "poly(sEF, ", best_degree_D_0, ", raw = TRUE))^2"))
    logit_model_0 <- glm(formula_D, family = binomial(link = "logit"), data = df_part_0)
  }
  
  h_s_1 <- predict(logit_model_0, type = "response", newdata = df_part_1)
  h_s_0 <- predict(logit_model_1, type = "response", newdata = df_part_0)
  
  #Subset data
  training_data1_1 <- subset(df_part_1, D == 1)
  training_data0_1 <- subset(df_part_1, D == 0)
  
  training_data1_0 <- subset(df_part_0, D == 1)
  training_data0_0 <- subset(df_part_0, D == 0)
  
  if (best_degree_YM0_1 == 0) {
    YM0_model_1 <- lm(YM ~ 1, data = training_data0_1)
  } else {
    formula_YM0_1 <- as.formula(paste("YM ~ (poly(sM, ", best_degree_YM0_1, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YM0_1, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YM0_1, ", raw = TRUE))^2"))
    YM0_model_1 <- lm(formula_YM0_1, data = training_data0_1)
  }
  
  if (best_degree_YM0_0 == 0) {
    YM0_model_0 <- lm(YM ~ 1, data = training_data0_0)
  } else {
    formula_YM0_0 <- as.formula(paste("YM ~ (poly(sM, ", best_degree_YM0_0, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YM0_0, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YM0_0, ", raw = TRUE))^2"))
    YM0_model_0 <- lm(formula_YM0_0, data = training_data0_0)
  }
  
  if (best_degree_YM1_1 == 0) {
    YM1_model_1 <- lm(YM ~ 1, data = training_data1_1)
  } else {
    formula_YM1_1 <- as.formula(paste("YM ~ (poly(sM, ", best_degree_YM1_1, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YM1_1, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YM1_1, ", raw = TRUE))^2"))
    YM1_model_1 <- lm(formula_YM1_1, data = training_data1_1)
  }
  
  if (best_degree_YM1_0 == 0) {
    YM1_model_0 <- lm(YM ~ 1, data = training_data1_0)
  } else {
    formula_YM1_0 <- as.formula(paste("YM ~ (poly(sM, ", best_degree_YM1_0, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YM1_0, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YM1_0, ", raw = TRUE))^2"))
    YM1_model_0 <- lm(formula_YM1_0, data = training_data1_0)
  }
  
  if (best_degree_YF0_1 == 0) {
    YF0_model_1 <- lm(YF ~ 1, data = training_data0_1)
  } else {
    formula_YF0_1 <- as.formula(paste("YF ~ (poly(sM, ", best_degree_YF0_1, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YF0_1, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YF0_1, ", raw = TRUE))^2"))
    YF0_model_1 <- lm(formula_YF0_1, data = training_data0_1)
  }
  
  if (best_degree_YF0_0 == 0) {
    YF0_model_0 <- lm(YF ~ 1, data = training_data0_0)
  } else {
    formula_YF0_0 <- as.formula(paste("YF ~ (poly(sM, ", best_degree_YF0_0, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YF0_0, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YF0_0, ", raw = TRUE))^2"))
    YF0_model_0 <- lm(formula_YF0_0, data = training_data0_0)
  }
  
  if (best_degree_YF1_1 == 0) {
    YF1_model_1 <- lm(YF ~ 1, data = training_data1_1)
  } else {
    formula_YF1_1 <- as.formula(paste("YF ~ (poly(sM, ", best_degree_YF1_1, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YF1_1, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YF1_1, ", raw = TRUE))^2"))
    YF1_model_1 <- lm(formula_YF1_1, data = training_data1_1)
  }
  
  if (best_degree_YF1_0 == 0) {
    YF1_model_0 <- lm(YF ~ 1, data = training_data1_0)
  } else {
    formula_YF1_0 <- as.formula(paste("YF ~ (poly(sM, ", best_degree_YF1_0, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_YF1_0, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_YF1_0, ", raw = TRUE))^2"))
    YF1_model_0 <- lm(formula_YF1_0, data = training_data1_0)
  }
  
  # Use the fitted model to predict values for the whole dataset
  E_YM0_s_1 <- predict(YM0_model_0, newdata = df_part_1)
  E_YM1_s_1 <- predict(YM1_model_0, newdata = df_part_1)
  E_YF0_s_1 <- predict(YF0_model_0, newdata = df_part_1)
  E_YF1_s_1 <- predict(YF1_model_0, newdata = df_part_1)
  
  E_YM0_s_0 <- predict(YM0_model_1, newdata = df_part_0)
  E_YM1_s_0 <- predict(YM1_model_1, newdata = df_part_0)
  E_YF0_s_0 <- predict(YF0_model_1, newdata = df_part_0)
  E_YF1_s_0 <- predict(YF1_model_1, newdata = df_part_0)
  
  
  ## Etape 1: Define the function m
  
  ### Etape 1-1: Define the function phi
  #'@noRd
  phi <- function(s, lambda){
    val <- 1 - lambda[2]*s - lambda[3]*(1-s) + s*(1-s)*(lambda[2]*lambda[3] - lambda[4]*lambda[5])
    # Empêche la division par une valeur trop proche de 0
    sign_val <- ifelse(val >= 0, 1, -1)
    ifelse(abs(val) < 1e-6, sign_val * 1e-6, val)
  }
  
  ### Etape 1-2: Define the conditional treatment effect functions
  #'@noRd
  Num_condi_TE <- function(sM, sEM, sEF, lambda){
    mM <- lambda[1] * (sEF * lambda[5] * (1-sM) + sEM * (1 - lambda[3]*(1-sM))) * (sM > 0)
    mF <- lambda[1] * (sEF * (1 - lambda[2] * sM) + sEM * lambda[4] * sM) * (sM < 1)
    m <- return(cbind(mM, mF))
  }
  
  ### Etape 1-3: Define the final function m (dimension G x 2)
  #'@noRd
  moment_function <- function(lambda, Z) {
    YM <- Z[, 1]
    YF <- Z[, 2]
    D <- Z[, 3]
    sM <- Z[, 4]
    sEM <- Z[, 5]
    sEF <- Z[, 6]
    h_s <- Z[, 7]
    
    h_s_clipped <- pmin(pmax(h_s, 1e-4), 1 - 1e-4)  # évite 0 et 1 exacts
    denom_ipw <- h_s_clipped * (1 - h_s_clipped)
    
    IPW_YM <- (((D - h_s_clipped)*YM)/(denom_ipw)) * (sM > 0)
    IPW_YF <- (((D - h_s_clipped)*YF)/(denom_ipw)) * (sM < 1)
    
    m <- cbind(IPW_YM, IPW_YF) - (Num_condi_TE(sM, sEM, sEF, lambda)/phi(sM, lambda))
    return(m)
  }
  
  ## Etape 1-3: Define the GMM objective function
  #'@noRd
  gmm1_objective <- function(params, Z) {
    sM <- Z[, 4]
    sEM <- Z[, 5]
    sEF <- Z[, 6]
    m <- moment_function(params, Z)
    residuals <- cbind(m, sM * m, sEM * m, sEF * m, sEM * sM * m, sEF * sM * m, sEF * sEM * sM * m)
    if (!all(is.finite(residuals))) return(1e10)  # pénalité finie, pas de crash
    
    emp_moments <- colMeans(residuals)
    val <- as.numeric(t(emp_moments) %*% emp_moments)
    
    if (!is.finite(val)) return(1e10)
    return(val)
  }
  
  Z_1 <- cbind(YM_part_1, YF_part_1, D_part_1, sM_part_1, sEM_part_1, sEF_part_1, h_s_1)  # Combine data
  Z_0 <- cbind(YM_part_0, YF_part_0, D_part_0, sM_part_0, sEM_part_0, sEF_part_0, h_s_0)  # Combine data
  lambda_init <- c(0, 0.1, 0.1, 0.1, 0.1)
  
  opt_0 <- optim(
    lambda_init,
    fn = gmm1_objective,
    Z = Z_0,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999, -0.999999,-0.999999), upper=c(Inf,0.999999,0.999999, 0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda1_0 <- opt_0$par
  
  opt_1 <- optim(
    lambda_init,
    fn = gmm1_objective,
    Z = Z_1,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999, -0.999999,-0.999999), upper=c(Inf,0.999999,0.999999, 0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda1_1 <- opt_1$par
  
  lambda1 <- (G_1/G)*lambda1_1 + (G_0/G)*lambda1_0
  
  # Etape 2 : estimation des poids
  
  ## Etape 2-1 : function to have M
  #'@noRd
  Mi <- function(sM, sEM, sEF, lambda){
    
    denom <- phi(sM, lambda)
    
    Mi_M1 <- (sM > 0) * (sEF * lambda[5] * (1-sM) + sEM * (1 - lambda[3]*(1-sM)))/denom
    Mi_M2 <- (sM > 0) * (-1) * lambda[1] * (((sEF * lambda[5] * (1-sM) + sEM * (1 - lambda[3] * (1 - sM))) * (sM * (1-sM) - sM)))/(denom*denom)
    Mi_M3 <- (sM > 0) * (-1) * lambda[1] * (sEM * (1 - sM) * denom + (sEF * lambda[5] * (1 - sM) + sEM * (1 - lambda[3]*(1-sM))) * (sM * (1-sM) * lambda[2] - (1 - sM)))/(denom*denom)
    Mi_M4 <- (sM > 0) * lambda[1] * ((sEF * lambda[5] * (1-sM) + sEM * (1-lambda[3]*(1-sM))) * sEM * (1- sEM) * lambda[5])/(denom*denom)
    Mi_M5 <- (sM > 0) * lambda[1] * (sEF * (1 - sM) * denom + (sEF * lambda[5] * (1 - sM) + sEM * (1 - lambda[3] * (1 - sM))) * sM * (1 - sM) * lambda[4])/ (denom * denom)
    
    Mi_F1 <- (sM < 1) * (sEF * (1 - lambda[2] * sM) + sEM * lambda[4] * sM) / denom
    Mi_F2 <- (sM < 1) * (-1) * lambda[1] * (sEF * sM * denom + (sEF * (1 - lambda[2] * sM) + sEM * lambda[4] * sM)*(sM * (1-sM) - sM))/(denom*denom)
    Mi_F3 <- (sM < 1) * (-1) * lambda[1] * ((sEF * (1 - lambda[2] * sM) + sEM * lambda[4] * sM) * (sM * (1-sM) * lambda[2] - (1-sM)))/(denom*denom)
    Mi_F4 <- (sM < 1) * lambda[1] * (sEM * sM * denom + sM * (1-sM) * lambda[5] * (sEF * (1 - lambda[2] * sM) + sEM * sM * lambda[4])) / (denom * denom)
    Mi_F5 <- (sM < 1) * lambda[1] * ((sEF * (1 - lambda[2] * sM) + sEM * lambda[4] * sM) * sM * (1 - sM) * lambda[4])/(denom * denom)
    
    Mi <- (-1) * cbind(c(Mi_M1, Mi_F1), c(Mi_M2, Mi_F2), c(Mi_M3, Mi_F3), c(Mi_M4, Mi_F4), c(Mi_M5, Mi_F5))
    
    return(Mi)
    
  }
  
  ## Etape 2-1 : function to estimate alpha
  #'@noRd
  alpha_i <- function(p_score, E_YM1_s, E_YF1_s, E_YM0_s, E_YF0_s){
    
    alpha_1 <- ((E_YM1_s/p_score) +  (E_YM0_s/(1-p_score)))
    alpha_2 <- ((E_YF1_s/p_score) +  (E_YF0_s/(1-p_score)))
    
    alpha <- (-1) * cbind(alpha_1, alpha_2)
    
    return(alpha)
  }
  
  
  ###### Case with no clustering #####
  
  ## Nonparametric estimation of the variance of residuals
  
  # On recupere les variances de rhoE, rhoN et Cov(rhoE, rhoN)
  rho_1 <- moment_function(lambda1, Z_1)
  rho_0 <- moment_function(lambda1, Z_0)
  
  df_part_1$rho_trho_M <- rho_1[,1]*rho_1[,1]
  df_part_1$rho_trho_MF <- rho_1[,1]*rho_1[,2]
  df_part_1$rho_trho_F <- rho_1[,2]*rho_1[,2]
  
  df_part_0$rho_trho_M <- rho_0[,1]*rho_0[,1]
  df_part_0$rho_trho_MF <- rho_0[,1]*rho_0[,2]
  df_part_0$rho_trho_F <- rho_0[,2]*rho_0[,2]
  
  # Create folds for cross-validation
  folds_Sigma_1 <- caret::createFolds(df_part_1$sM, k = k_folds, list = TRUE)
  folds_Sigma_0 <- caret::createFolds(df_part_0$sM, k = k_folds, list = TRUE)
  
  # Function to compute cross-validation error for a given degree
  #'@noRd
  cv_error_Sigma <- function(degree) {
    
    errors_1 <- c()
    errors_0 <- c()
    
    for (i in 1:k_folds) {
      # Split into training and validation sets
      train_idx_0 <- unlist(folds_Sigma_0[-i])
      test_idx_0 <- unlist(folds_Sigma_0[i])
      
      train_data_0 <- df_part_0[train_idx_0, ]
      test_data_0 <- df_part_0[test_idx_0, ]
      
      train_idx_1 <- unlist(folds_Sigma_1[-i])
      test_idx_1 <- unlist(folds_Sigma_1[i])
      
      train_data_1 <- df_part_1[train_idx_1, ]
      test_data_1 <- df_part_1[test_idx_1, ]
      
      # Define formula based on polynomial degree
      if (degree == 0) {
        formula <- as.formula("cbind(rho_trho_M, rho_trho_MF, rho_trho_F) ~ 1")  # Model with only an intercept
      } else {
        formula <- as.formula(paste("cbind(rho_trho_M, rho_trho_MF, rho_trho_F) ~ (poly(sM, ", degree, ", raw = TRUE) +",
                                    "poly(sEM, ", degree, ", raw = TRUE) +",
                                    "poly(sEF, ", degree, ", raw = TRUE))^2"))
      }
      
      # Fit regression model
      model_0 <- lm(formula, data = train_data_0)
      model_1 <- lm(formula, data = train_data_1)
      
      # Predict on test set
      preds_0 <- predict(model_0, newdata = test_data_0)
      preds_1 <- predict(model_1, newdata = test_data_1)
      
      # Compute Mean Squared Error (MSE) for each outcome
      mseM_1  <- mean((preds_1[, 1] - test_data_1$rho_trho_M)^2)
      mseMF_1 <- mean((preds_1[, 2] - test_data_1$rho_trho_MF)^2)
      mseF_1  <- mean((preds_1[, 3] - test_data_1$rho_trho_F)^2)
      
      mseM_0  <- mean((preds_0[, 1] - test_data_0$rho_trho_M)^2)
      mseMF_0 <- mean((preds_0[, 2] - test_data_0$rho_trho_MF)^2)
      mseF_0  <- mean((preds_0[, 3] - test_data_0$rho_trho_F)^2)
      
      # Store the average MSE across outcomes
      errors_1 <- c(errors_1, mean(c(mseM_1, mseMF_1, mseF_1)))
      errors_0 <- c(errors_0, mean(c(mseM_0, mseMF_0, mseF_0)))
    }
    
    return(c(mean(errors_1), mean(errors_0)))  # Return average CV error
  }
  
  # Evaluate cross-validation error for each polynomial degree (including 0)
  cv_results_Sigma <- sapply(0:15, cv_error_Sigma)
  
  # Find the best polynomial degree (minimizing CV error)
  best_degree_Sigma_1 <- which.min(cv_results[1,]) - 1  # Adjust index to match degree
  best_degree_Sigma_0 <- which.min(cv_results[2,]) - 1  # Adjust index to match degree
  
  if (best_degree_Sigma_0 == 0) {
    model_V_s_0 <- lm(cbind(rho_trho_M, rho_trho_MF, rho_trho_F) ~ 1, data = df_part_0)
  } else {
    formula_V_s_0 <- as.formula(paste("cbind(rho_trho_M, rho_trho_MF, rho_trho_F) ~ (poly(sM, ", best_degree_Sigma_0, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_Sigma_0, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_Sigma_0, ", raw = TRUE))^2"))
    model_V_s_0 <- lm(formula_V_s_0, data = df_part_0)
  }
  
  if (best_degree_Sigma_1 == 0) {
    model_V_s_1 <- lm(cbind(rho_trho_M, rho_trho_MF, rho_trho_F) ~ 1, data = df_part_1)
  } else {
    formula_V_s_1 <- as.formula(paste("cbind(rho_trho_M, rho_trho_MF, rho_trho_F) ~ (poly(sM, ", best_degree_Sigma_0, ", raw = TRUE) +",
                                      "poly(sEM, ", best_degree_Sigma_1, ", raw = TRUE) +",
                                      "poly(sEF, ", best_degree_Sigma_1, ", raw = TRUE))^2"))
    model_V_s_1 <- lm(formula_V_s_1, data = df_part_1)
  }
  
  #Use the fitted model to predict values for the whole dataset
  predict_V_s_1 <- predict(model_V_s_0, newdata = df_part_1)
  predict_V_s_0 <- predict(model_V_s_1, newdata = df_part_0)
  
  alpha_1 <- alpha_i(h_s_1, E_YM1_s_1, E_YF1_s_1, E_YM0_s_1, E_YF0_s_1)
  alpha_0 <- alpha_i(h_s_0, E_YM1_s_0, E_YF1_s_0, E_YM0_s_0, E_YF0_s_0)
  
  # Apply the function to each row
  w_hat_list_1 <- lapply(1:G_1, function(i) t(Mi(sM_part_1[i], sEM_part_1[i], sEF_part_1[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_1[i,1], predict_V_s_1[i,2], predict_V_s_1[i,2], predict_V_s_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol))
  w_hat_list_0 <- lapply(1:G_0, function(i) t(Mi(sM_part_0[i], sEM_part_0[i], sEF_part_0[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_0[i,1], predict_V_s_0[i,2], predict_V_s_0[i,2], predict_V_s_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol))
  
  # Combine results into a 3D array
  w_hat_array_1 <- array(unlist(w_hat_list_1), dim = c(5, 2, G_1))
  w_hat_array_0 <- array(unlist(w_hat_list_0), dim = c(5, 2, G_0))
  
  
  # Apply the function to each row
  V_ind_list_1 <- lapply(1:G_1, function(i) t(Mi(sM_part_1[i], sEM_part_1[i], sEF_part_1[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_1[i,1], predict_V_s_1[i,2], predict_V_s_1[i,2], predict_V_s_1[i,3]), nrow = 2) + h_s_1[i] * (1 - h_s_1[i]) * alpha_1[i,] %*% t(alpha_1[i,]), tol = tol) %*% Mi(sM_part_1[i], sEM_part_1[i], sEF_part_1[i], lambda1))
  V_ind_list_0 <- lapply(1:G_0, function(i) t(Mi(sM_part_0[i], sEM_part_0[i], sEF_part_0[i], lambda1)) %*% MASS::ginv(matrix(c(predict_V_s_0[i,1], predict_V_s_0[i,2], predict_V_s_0[i,2], predict_V_s_0[i,3]), nrow = 2) + h_s_0[i] * (1 - h_s_0[i]) * alpha_0[i,] %*% t(alpha_0[i,]), tol = tol) %*% Mi(sM_part_0[i], sEM_part_0[i], sEF_part_0[i], lambda1))
  
  # Combine results into a 3D array
  V_ind_array_1 <- array(unlist(V_ind_list_1), dim = c(5, 5, G_1))
  V_ind_array_0 <- array(unlist(V_ind_list_0), dim = c(5, 5, G_0))
  
  # Sum the matrices
  V_a_1 <- apply(V_ind_array_1, c(1, 2), mean)
  V_a_0 <- apply(V_ind_array_0, c(1, 2), mean)

  # Step 3: Define the GMM objective function
  #'@noRd
  gmm2_objective <- function(params, Z, w, V, G) {
    # Calcul des rho + mise sous forme de vecteurs
    residuals <- moment_function(params, Z)
    if (!all(is.finite(residuals))) return(1e10)
    residuals_vector_list <- lapply(1:G, function(i) matrix(residuals[i, ], ncol = 1))
    
    # Step 3: Multiply each 3x2 matrix with the corresponding 2x1 vector
    w_rho_list <- mapply(function(M, v) M %*% v, w, residuals_vector_list, SIMPLIFY = FALSE)
    
    # Step 4: Combine results into an G x 3 matrix
    w_rho_matrix <- do.call(rbind, lapply(w_rho_list, as.vector))
    if (!all(is.finite(w_rho_matrix))) return(1e10)
    # Element-wise multiplication and summation
    Vinv <- tryCatch(solve(V), error = function(e) MASS::ginv(V))
    moments <- t(colMeans(w_rho_matrix)) %*% Vinv %*% colMeans(w_rho_matrix)
    if (!is.finite(moments)) return(1e10)
    return(moments)
  }
  
  opt2_1 <- optim(
    par = lambda1,
    fn = gmm2_objective,
    Z = Z_1,
    G = G_1,
    w = w_hat_list_1,
    V = V_a_1, 
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda2_1 <- opt2_1$par
  
  opt2_0 <- optim(
    par = lambda1,
    fn = gmm2_objective,
    Z = Z_0,
    G = G_0,
    w = w_hat_list_0,
    V = V_a_0,
    method = "L-BFGS-B",
    lower=c(-Inf,-0.999999,-0.999999,-0.999999,-0.999999), upper=c(Inf,0.999999,0.999999,0.999999,0.999999) #,
    #method = "BFGS",
    #control = list(reltol = tol)
  )
  
  lambda2_0 <- opt2_0$par
  
  lambda2 <- (G_1/G)*lambda2_1 + (G_0/G)*lambda2_0
  
  # Computing standard errors
  
  # On recupere les variances de rhoE, rhoN et Cov(rhoE, rhoN)
  rho_2_1 <- moment_function(lambda2, Z_1)
  df_part_1$rho_trho_M2 <- rho_2_1[,1]*rho_2_1[,1]
  df_part_1$rho_trho_MF2 <- rho_2_1[,1]*rho_2_1[,2]
  df_part_1$rho_trho_F2 <- rho_2_1[,2]*rho_2_1[,2]
  
  rho_2_0 <- moment_function(lambda2, Z_0)
  df_part_0$rho_trho_M2 <- rho_2_0[,1]*rho_2_0[,1]
  df_part_0$rho_trho_MF2 <- rho_2_0[,1]*rho_2_0[,2]
  df_part_0$rho_trho_F2 <- rho_2_0[,2]*rho_2_0[,2]
  
  if (best_degree_Sigma_0 == 0) {
    predict_V_s2_1 <- predict(lm(cbind(rho_trho_M2, rho_trho_MF2, rho_trho_F2) ~ 1, data = df_part_0), newdata = df_part_1)
  } else {
    formula_V_s2_0 <- as.formula(paste("cbind(rho_trho_M2, rho_trho_MF2, rho_trho_F2) ~ (poly(sM, ", best_degree_Sigma_0, ", raw = TRUE) +",
                                       "poly(sEM, ", best_degree_Sigma_0, ", raw = TRUE) +",
                                       "poly(sEF, ", best_degree_Sigma_0, ", raw = TRUE))^2"))
    predict_V_s2_1 <- predict(lm(formula_V_s2_0, data = df_part_0), newdata = df_part_1)
  }
  
  if (best_degree_Sigma_1 == 0) {
    predict_V_s2_0 <- predict(lm(cbind(rho_trho_M2, rho_trho_MF2, rho_trho_F2) ~ 1, data = df_part_1), newdata = df_part_0)
  } else {
    formula_V_s2_1 <- as.formula(paste("cbind(rho_trho_M2, rho_trho_MF2, rho_trho_F2) ~ (poly(sM, ", best_degree_Sigma_1, ", raw = TRUE) +",
                                       "poly(sEM, ", best_degree_Sigma_1, ", raw = TRUE) +",
                                       "poly(sEF, ", best_degree_Sigma_1, ", raw = TRUE))^2"))
    predict_V_s2_0 <- predict(lm(formula_V_s2_1, data = df_part_1), newdata = df_part_0)
  }
  V_a <- (G_1/G)^2 * V_a_1 + (G_0/G)^2 * V_a_0
  Vainv <- tryCatch(solve(V_a), error = function(e) MASS::ginv(V_a))
  sd <- sqrt(diag(Vainv)/G)
  
  t_stat <- lambda2 / sd
  df <- G - 5 - 1  
  p_value <- 2 * (1 - pt(abs(t_stat), df))
  
  result_df <- data.frame(
    Statistic = c("Estimate", "Standard Error", "t-test", "p-value"),
    `Delta` = c(round(lambda2[1], 3), round(sd[1], 3), round(t_stat[1], 3), round(p_value[1], 3)),
    `Theta Within M` = c(round(lambda2[2], 3), round(sd[2], 3), round(t_stat[2], 3), round(p_value[2], 3)),
    `Theta Within F` = c(round(lambda2[3], 3), round(sd[3], 3), round(t_stat[3], 3), round(p_value[3], 3)),
    `Theta Between Female to Male` = c(round(lambda2[4], 3), round(sd[4], 3), round(t_stat[4], 3), round(p_value[4], 3)),
    `Theta Between Male to Female` = c(round(lambda2[5], 3), round(sd[5], 3), round(t_stat[5], 3), round(p_value[5], 3))
  )
  
  # Affichage avec flextable 
  ft <- flextable::flextable(result_df) 
  ft <- flextable::set_header_labels(ft,
                                     `Delta (<ce><b4>)` = "Delta (\u0394)",
                                     `Theta Within M (<ce><b8>_WM)` = "Theta Within M (\u03B8_WM)",
                                     `Theta Within F (<ce><b8>_WF)` = "Theta Within F (\u03B8_WF)",
                                     `Theta Between Male to Female (<ce><b8>_BFM)` = "Theta Between F<e2><86><92>M (\u03B8_BFM)",
                                     `Theta Between Female to Male (<ce><b8>_BMF)` = "Theta Between M<e2><86><92>F (\u03B8_BMF)")
                                     
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::autofit(ft)    

  return(list(estimates = result_df, table = ft))
  
}

