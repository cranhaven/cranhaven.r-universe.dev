## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SBMTrees)
library(mitml)
library(lme4)

## ----prediction_sim-----------------------------------------------------------
# Simulate data
data <- simulation_prediction(
  n_subject = 100, 
  seed = 1234, 
  nonlinear = TRUE, 
  nonrandeff = TRUE, 
  nonresidual = TRUE
)

# Extract training and testing datasets
X_train <- data$X_train
Y_train <- data$Y_train
Z_train <- data$Z_train
subject_id_train <- data$subject_id_train

X_test <- data$X_test
Z_test <- data$Z_test
subject_id_test <- data$subject_id_test

Y_test_true <- data$Y_test_true


## ----prediction---------------------------------------------------------------
# Fit the predictive model
model <- BMTrees_prediction(
  X_train, Y_train, Z_train, subject_id_train, 
  X_test, Z_test, subject_id_test, 
  model = "BMTrees", 
  binary = FALSE, 
  nburn = 3L, 
  npost = 4L, 
  skip = 1L, 
  verbose = FALSE, 
  seed = 1234
)

# Posterior expectation for the testing dataset
posterior_predictions <- model$post_predictive_y_test
head(colMeans(posterior_predictions))

## ----prediction_evaluation----------------------------------------------------
point_predictions = colMeans(posterior_predictions)

# Compute MAE
mae <- mean(abs(point_predictions - Y_test_true))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Compute MSE
mse <- mean((point_predictions - Y_test_true)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Compute 95% credible intervals
lower_bounds <- apply(posterior_predictions, 2, quantile, probs = 0.025)
upper_bounds <- apply(posterior_predictions, 2, quantile, probs = 0.975)

# Check if true values fall within the intervals
coverage <- mean(Y_test_true >= lower_bounds & Y_test_true <= upper_bounds)
cat("95% Posterior Predictive Interval Coverage:", coverage * 100, "%\n")



plot(Y_test_true, point_predictions, 
     xlab = "True Values", 
     ylab = "Predicted Values", 
     main = "True vs Predicted Values")
abline(0, 1, col = "red") # Add a 45-degree reference line


## ----imputation_sim-----------------------------------------------------------
# Simulate data with missing values
data <- simulation_imputation(
  n_subject = 100, 
  seed = 1234, 
  nonrandeff = TRUE, 
  nonresidual = TRUE, 
  alligned = FALSE
)

# Extract components of the dataset
X_mis <- data$X_mis   # Covariates with missing values
Y_mis <- data$Y_mis   # Outcomes with missing values
Z <- data$Z           # Random predictors
subject_id <- data$subject_id


## ----imputation---------------------------------------------------------------
# Run the sequential imputation
imputed_model <- sequential_imputation(
  X_mis, Y_mis, Z, subject_id, 
  type = rep(0, ncol(X_mis)), 
  binary_outcome = FALSE, 
  model = "BMTrees", 
  nburn = 3L, 
  npost = 40L, 
  skip = 2L, 
  verbose = FALSE, 
  seed = 1234
)

# Extract imputed data
imputed_data <- imputed_model$imputed_data
dim(imputed_data) # Dimensions: posterior samples x observations x variables

## ----imputation_evaluation----------------------------------------------------
# create structure which can be used in mitml
MI_data = list()
for (i in 1:dim(imputed_data)[1]) {
  MI_data[[i]] = cbind(as.data.frame(imputed_data[i,,]), Z, subject_id)
  colnames(MI_data[[i]]) = c(colnames(X_mis), "Y", "Z1", "Z2", "Z3", "subject_id")
}
MI_data <- as.mitml.list(MI_data)  # Replace with actual datasets
# Fit the model on each imputed dataset
lmm_results <- with(MI_data, lmer(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + (0 + Z1 + Z2 + Z3 | subject_id)))

# Pool fixed effects using Rubin's Rules
pooled_results <- testEstimates(lmm_results)

# Print pooled results
print(pooled_results)


