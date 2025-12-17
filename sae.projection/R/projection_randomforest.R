#' Projection RF Function
#' @description
#' This function trains a random forest model and performs domain-level estimation **without bias correction**.
#'
#' @param data_model The training dataset, consisting of auxiliary variables and the target variable.
#' @param target_column The name of the target column in the \code{data_model}.
#' @param predictor_cols A vector of predictor column names.
#' @param data_proj The data for projection (prediction), which needs to be projected using the trained model. It must contain the same auxiliary variables as the \code{data_model}
#' @param domain1 Domain variables for survey estimation (e.g., "province")
#' @param domain2 Domain variables for survey estimation (e.g., "regency")
#' @param psu Primary sampling units, representing the structure of the sampling frame.
#' @param ssu Secondary sampling units, representing the structure of the sampling frame (default is NULL).
#' @param strata Stratification variable, ensuring that specific subgroups are represented (default is NULL).
#' @param weights Weights used for the direct estimation from \code{data_model} and indirect estimation from \code{data_proj}.
#' @param split_ratio Proportion of data used for training (default is 0.8, meaning 80 percent for training and 20 percent for validation).
#' @param feature_selection Selection of predictor variables (default is \code{TRUE})
#'
#' @keywords internal
#' @return
#' A list containing the following elements:
#' \itemize{
#'    \item \code{model} The trained Random Forest model.
#'    \item \code{importance} Feature importance showing which features contributed most to the model's predictions.
#'    \item \code{train_accuracy} Accuracy of the model on the training set.
#'    \item \code{validation_accuracy} Accuracy of the model on the validation set.
#'    \item \code{validation_performance} Confusion matrix for the validation set, showing performance metrics like accuracy, precision, recall, etc.
#'    \item \code{data_proj} The projection data with predicted values.
#'    \item \code{Domain1} Estimations for Domain 1, including estimated values, variance, and relative standard error.
#'    \item \code{Domain2} Estimations for Domain 2, including estimated values, variance, and relative standard error.
#' }
#'
#' @import themis
#' @import caret
#' @importFrom randomForest combine
#' @importFrom ranger importance
#' @importFrom stats as.formula

#'
projection_rf <- function(data_model, target_column, predictor_cols, data_proj,
                          domain1, domain2, psu, ssu = NULL, strata = NULL, weights,
                          split_ratio = 0.8, feature_selection = TRUE) {
  Est_Y <- Estimation <- CI_Lower <- CI_Upper <- Variance <- RSE <- NA

  cli::cli_inform("Starting preprocessing...")

  if (identical(data_model, data_proj)) {
    cli::cli_abort("Model and projection datasets must be different.")
  }

  # Creating a Formula from target_column and predictor_cols
  formula <- stats::reformulate(predictor_cols, response = target_column)

  data_model[[target_column]] <- factor(data_model[[target_column]], levels = c(0, 1), labels = c("No", "Yes"))

  cli::cli_inform("Preprocessing completed. Starting data split...")

  # Split dataset (Training & Validation)
  set.seed(123)
  train_index <- caret::createDataPartition(data_model[[target_column]], p = split_ratio, list = FALSE)
  train_set <- data_model[train_index, ]
  validation_set <- data_model[-train_index, ]

  cli::cli_inform("Data split completed. Checking for missing values...")

  # Check missing values in predictors or target in train set
  missing_cols <- colnames(train_set)[colSums(is.na(train_set[, c(predictor_cols, target_column), drop = FALSE])) > 0]

  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing values detected in the training dataset.")}

  if (feature_selection) {
    cli::cli_inform("Feature selection (RFE) enabled. Starting RFE...")

    control_rfe <- caret::rfeControl(
      functions = caret::rfFuncs,
      method = "cv",
      number = 5,
      verbose = FALSE
    )

    set.seed(123)
    rfe_results <- caret::rfe(
      x = train_set[, predictor_cols, drop = FALSE],
      y = train_set[[target_column]],
      sizes = c(1:10, 15, 20),
      rfeControl = control_rfe
    )

    selected_features <- caret::predictors(rfe_results)

    if (length(selected_features) == 0) {
      cli::cli_abort('RFE did not select any features. Please check the dataset', class = "error")
    }

    cli::cli_inform("RFE completed. Features selected.")
  } else {
    cli::cli_warn("Feature selection (RFE) disabled. Using all predictor columns.")
    selected_features <- predictor_cols
  }

  train_set_selected <- train_set[, c(selected_features, target_column)]
  validation_set_selected <- validation_set[, c(selected_features, target_column)]

  cli::cli_inform("Features selected. Starting hyperparameter tuning...")

  # Hyperparameter tuning
  control_final <- caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    sampling = "smote"
  )

  cli::cli_inform("Hyperparameter tuning completed. Training model...")

  # Training model with tuneLength
  set.seed(123)
  rf_model <- caret::train(
    stats::reformulate(selected_features, response = target_column),
    data = train_set_selected,
    method = "ranger",
    trControl = control_final,
    tuneLength = 10,
    num.trees = 500,
    importance = "impurity",
    replace = FALSE,
    metric = "Accuracy"
  )

  cli::cli_inform("Model trained. Starting model evaluation...")

  # Model evaluation
  train_pred <- stats::predict(rf_model, newdata = train_set_selected)
  train_conf_matrix <- caret::confusionMatrix(train_pred, train_set_selected[[target_column]])
  train_accuracy <- train_conf_matrix$overall["Accuracy"]

  validation_pred <- stats::predict(rf_model, newdata = validation_set_selected)
  validation_conf_matrix <- caret::confusionMatrix(validation_pred, validation_set_selected[[target_column]])
  validation_accuracy <- validation_conf_matrix$overall["Accuracy"]

  cli::cli_inform("Evaluation completed. Starting predictions on new data...")

  for (col in selected_features) {
    if (is.factor(data_model[[col]])) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(data_model[[col]]))
    }
  }

  # Prediction on new data
  data_proj$Est_Y <- stats::predict(rf_model, newdata = data_proj, na.action = stats::na.pass)

  cli::cli_inform("Predictions completed. Starting indirect estimation on domain...")

  data_proj <- dplyr::mutate(data_proj, Est_Y = factor(Est_Y, levels = c("No", "Yes")))

  # Survey design
  if (is.null(psu)) {cli::cli_abort("PSU cannot be NULL.")}

  susenas_design <- survey::svydesign(
    ids = if (!is.null(ssu) && ssu != "") stats::as.formula(paste("~", psu, "+", ssu)) else stats::as.formula(paste("~", psu)),
    strata = if (!is.null(strata)) stats::as.formula(paste("~", strata)) else NULL,
    weights = stats::as.formula(paste("~", weights)),
    data = data_proj,
    nest = TRUE
  )


  # Estimation for domain1
  result_domain1 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", domain1)),
    vartype = c('cvpct', 'var', 'ci'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation = "Est_YYes",
      CI_Lower = "ci_l.Est_YYes",
      CI_Upper = "ci_u.Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation = round(Estimation * 100, 2),
      CI_Lower = round(CI_Lower * 100, 2),
      CI_Upper = round(CI_Upper * 100, 3),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation,
      CI_Lower,
      CI_Upper,
      Variance,
      RSE
    )

  # Estimation for domain2
  result_domain2 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", "interaction(", domain1, ",", domain2, ")")),
    vartype = c('cvpct', 'var', 'ci'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation = "Est_YYes",
      CI_Lower = "ci_l.Est_YYes",
      CI_Upper = "ci_u.Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation = round(Estimation * 100, 2),
      CI_Lower = round(CI_Lower * 100, 2),
      CI_Upper = round(CI_Upper * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation,
      CI_Lower,
      CI_Upper,
      Variance,
      RSE
    )

  cli::cli_inform("Estimation completed. Returning results...")

  # Return results
  return(list(
    model = rf_model,
    importance = caret::varImp(rf_model),
    train_accuracy = train_accuracy,
    validation_accuracy = validation_accuracy,
    validation_performance = validation_conf_matrix,
    data_proj = data_proj,
    Domain1 = result_domain1,
    Domain2 = result_domain2
  ))
}

#' Projection RF with Correction Bias
#'
#' @description
#' This function extends \code{projection_rf} by incorporating **bias correction** for better domain-level estimation.
#'
#' @inheritParams projection_rf
#' @keywords internal
#'
#' @return A list containing the following elements:
#' \itemize{
#'    \item \code{model} The trained Random Forest model.
#'    \item \code{importance} Feature importance showing which features contributed most to the model's predictions.
#'    \item \code{train_accuracy} Accuracy of the model on the training set.
#'    \item \code{validation_accuracy} Accuracy of the model on the validation set.
#'    \item \code{validation_performance} Confusion matrix for the validation set, showing performance metrics like accuracy, precision, recall, etc.
#'    \item \code{data_proj} The projection data with predicted values.
#'    \item \code{Direct} Direct estimations for Domain 1, including estimated values, variance, and relative standard error.
#'    \item \code{Domain1_corrected_bias} Bias-corrected estimations for Domain 1, including estimated values, variance, and relative standard error (RSE).
#'    \item \code{Domain2_corrected_bias} Bias-corrected estimations for Domain 2, including estimated values, variance, and relative standard error (RSE).
#' }
#'
projection_rf_CorrectedBias <- function(data_model, target_column, predictor_cols, data_proj,
                                        domain1, domain2, psu, ssu = NULL, strata = NULL, weights,
                                        split_ratio = 0.8, feature_selection = TRUE) {
  Est_Y <- Est_corrected <-  Estimation_Direct <- Estimation_Domain1 <-  Estimation_Domain2 <- Estimation_Pred <- RSE <- RSE_corrected <- Var_corrected <- Variance <- weight_domain1 <- weight_domain2 <- NA

  cli::cli_inform("Starting preprocessing...")

  same_data <- identical(data_model, data_proj)
  if (same_data) {
    cli::cli_warn(
      "Model and projection datasets are identical.")}

  # Creating a Formula from target_column and predictor_cols
  formula <- stats::reformulate(predictor_cols, response = target_column)

  data_model[[target_column]] <- factor(data_model[[target_column]], levels = c(0, 1), labels = c("No", "Yes"))

  cli::cli_inform("Preprocessing completed. Starting data split...")

  # Split data model
  set.seed(123)
  train_index <- caret::createDataPartition(data_model[[target_column]], p = split_ratio, list = FALSE)
  train_set <- data_model[train_index, ]
  validation_set <- data_model[-train_index, ]

  cli::cli_inform("Data split completed. Checking for missing values...")

  # Check missing values in predictors or target in train set
  missing_cols <- colnames(train_set)[colSums(is.na(train_set[, c(predictor_cols, target_column), drop = FALSE])) > 0]

  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing values detected in the training dataset.")}

  if (feature_selection) {
    cli::cli_inform("Feature selection (RFE) enabled. Starting RFE...")

    control_rfe <- caret::rfeControl(
      functions = caret::rfFuncs,
      method = "cv",
      number = 5,
      verbose = FALSE
    )

    set.seed(123)
    rfe_results <- caret::rfe(
      x = train_set[, predictor_cols, drop = FALSE],
      y = train_set[[target_column]],
      sizes = c(1:10, 15, 20),
      rfeControl = control_rfe
    )

    selected_features <- caret::predictors(rfe_results)

    if (length(selected_features) == 0) {
      cli::cli_abort('RFE did not select any features. Please check the dataset', class = "error")
    }

    cli::cli_inform("RFE completed. Features selected.")
  } else {
    cli::cli_warn("Feature selection (RFE) disabled. Using all predictor columns.")
    selected_features <- predictor_cols
  }

  train_set_selected <- train_set[, c(selected_features, target_column)]
  validation_set_selected <- validation_set[, c(selected_features, target_column)]

  cli::cli_inform("Features selected. Starting hyperparameter tuning...")

  # Hyperparameter tuning
  control_final <- caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    sampling = "smote"
  )

  cli::cli_inform("Hyperparameter tuning completed. Training model...")

  # Training model with selected features
  set.seed(123)
  rf_model <- caret::train(
    stats::reformulate(selected_features, response = target_column),
    data = train_set_selected,
    method = "ranger",
    trControl = control_final,
    tuneLength = 10,
    num.trees = 500,
    importance = "impurity",
    replace = FALSE,
    metric = "Accuracy"
  )

  cli::cli_inform("Model training completed. Starting model evaluation...")

  # Evaluate the model
  data_model$prediction <- stats::predict(rf_model, newdata = data_model)
  data_model$prediction <- factor(data_model$prediction, levels = c("No", "Yes"))

  # Model evaluation
  train_pred <- stats::predict(rf_model, newdata = train_set_selected)
  train_conf_matrix <- caret::confusionMatrix(train_pred, train_set_selected[[target_column]])
  train_accuracy <- train_conf_matrix$overall["Accuracy"]

  validation_pred <- stats::predict(rf_model, newdata = validation_set_selected)
  validation_conf_matrix <- caret::confusionMatrix(validation_pred, validation_set_selected[[target_column]])
  validation_accuracy <- validation_conf_matrix$overall["Accuracy"]

  cli::cli_inform("Evaluation completed. Starting prediction on new data...")

  # Predictions on new data
  for (col in selected_features) {
    if (is.factor(data_model[[col]])) {
      data_proj[[col]] <- factor(data_proj[[col]], levels = levels(data_model[[col]]))
    }
  }
  data_proj$Est_Y <- stats::predict(rf_model, newdata = data_proj, na.action = stats::na.pass)

  data_proj <- dplyr::mutate(data_proj, Est_Y = factor(Est_Y, levels = c("No", "Yes")))

  cli::cli_inform("Prediction completed. Starting indirect estimation for domain...")

  if (is.null(psu)) {cli::cli_abort("PSU cannot be NULL.")}
  susenas_design <- survey::svydesign(
    ids_formula <- if (!is.null(ssu)) as.formula(paste("~", psu, "+", ssu)) else as.formula(paste("~", psu)),
    strata = if (!is.null(strata)) stats::as.formula(paste("~", strata)) else NULL,
    weights = stats::as.formula(paste("~", weights)),
    data = data_proj,
    nest = TRUE
  )

  # Estimation for domain1
  result_domain1 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", domain1)),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation_Domain1 = "Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation_Domain1 = round(Estimation_Domain1 * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Domain1,
      Variance,
      RSE
    )

  # Estimation for domain2
  result_domain2 <- survey::svyby(
    formula = stats::as.formula("~ Est_Y"),
    by = stats::as.formula(paste("~", "interaction(", domain1, ",", domain2, ")")),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = susenas_design
  ) %>%
    dplyr::rename(
      Estimation_Domain2 = "Est_YYes",
      Variance = "var.Est_YYes",
      RSE = "cv%.Est_YYes"
    ) %>%
    dplyr::mutate(
      Estimation_Domain2 = round(Estimation_Domain2 * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Domain2,
      Variance,
      RSE
    )

  cli::cli_inform("Indirect estimation completed. Starting direct estimation...")

  options(survey.adjust.domain.lonely = TRUE, survey.lonely.psu = 'adjust')

  ssn_design_2 <- survey::svydesign(
    ids = if (!is.null(ssu) && ssu != "") stats::as.formula(paste("~", psu, "+", ssu)) else stats::as.formula(paste("~", psu)),
    strata = if (!is.null(strata)) stats::as.formula(paste("~", strata)) else NULL,
    weights = stats::as.formula(paste0("~ ", weights)),
    data = data_model,
    nest = TRUE
  )

  result_direct <- survey::svyby(
    formula = stats::as.formula(paste("~", target_column)),
    by = stats::as.formula(paste0("~ ", domain1)),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = ssn_design_2
  ) %>%
    dplyr::rename(
      Estimation_Direct = paste(target_column, "Yes", sep = ""),
      Variance = paste("var.", target_column, "Yes", sep = ""),
      RSE = paste("cv%.", target_column, "Yes", sep = "")
    ) %>%
    dplyr::mutate(
      Estimation_Direct = round(Estimation_Direct * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Direct,
      Variance,
      RSE
    )

  cli::cli_inform("Direct estimation completed. Starting bias correction estimation...")

  result_pred <- survey::svyby(
    formula = stats::as.formula("~ prediction"),
    by = stats::as.formula(paste0("~ ", domain1)),
    vartype = c('cvpct', 'var'),
    FUN = survey::svymean,
    design = ssn_design_2
  ) %>%
    dplyr::rename(
      Estimation_Pred = paste("predictionYes"),
      Variance = paste("var.predictionYes"),
      RSE = paste("cv%.predictionYes")
    ) %>%
    dplyr::mutate(
      Estimation_Pred = round(Estimation_Pred * 100, 2),
      Variance = round(Variance, 6),
      RSE = round(RSE, 2)
    ) %>%
    dplyr::select(
      Estimation_Pred,
      Variance,
      RSE
    )

  # Calculating weight for projected data
  weight_domain1 <- data_proj %>%
    dplyr::group_by(!!dplyr::sym(domain1)) %>%
    dplyr::summarise(weight_domain1 = sum(!!dplyr::sym(weights), na.rm = TRUE))

  weight_domain2 <- data_proj %>%
    dplyr::group_by(!!dplyr::sym(domain1), !!dplyr::sym(domain2)) %>%
    dplyr::summarise(weight_domain2 = sum(!!dplyr::sym(weights), na.rm = TRUE), .groups = "drop")

  result_wi <- weight_domain2 %>%
    dplyr::mutate(weight_domain1 = weight_domain1$weight_domain1[1]) %>%
    dplyr::mutate(wi = weight_domain2 / weight_domain1)

  # Bias correction calculation for data model
  bias_est <- if (result_domain1$Estimation_Domain1 > result_direct$Estimation_Direct) {
    result_direct$Estimation_Direct - result_pred$Estimation_Pred
  } else {
    result_pred$Estimation_Pred - result_direct$Estimation_Direct
  }

  var_bias_est <- result_pred$Variance + result_direct$Variance

  result_corrected_domain2 <- result_domain2 %>%
    dplyr::mutate(
      Est_corrected = result_domain2$Estimation_Domain2 + bias_est,
      Est_corrected = round(Est_corrected, 2),
      Var_corrected = result_domain2$Variance + var_bias_est,
      Var_corrected = round(Var_corrected, 6),
      RSE_corrected = (sqrt(Var_corrected) / (Est_corrected/100)) * 100,
      RSE_corrected = round(RSE_corrected, 2)
    )

  result_corrected_domain1 <- result_domain1 %>%
    dplyr::mutate(
      Est_corrected = sum(result_corrected_domain2$Est_corrected*result_wi$wi),
      Est_corrected = round(Est_corrected, 2),
      Var_corrected = sum((result_wi$wi)^2 * result_corrected_domain2$Var_corrected),
      Var_corrected = round(Var_corrected, 6),
      RSE_corrected = (sqrt(Var_corrected) / (Est_corrected/100)) * 100,
      RSE_corrected = round(RSE_corrected, 2)
    )

  cli::cli_inform("Bias-corrected estimation completed. Returning results...")

  result_list <- list(
    model = rf_model,
    importance = caret::varImp(rf_model),
    train_accuracy = train_accuracy,
    validation_accuracy = validation_accuracy,
    validation_performance = validation_conf_matrix,
    data_proj = data_proj,
    Direct = result_direct,
    Domain1_corrected_bias = result_corrected_domain1
  )

  if (!same_data) {
    result_list$Domain2_corrected_bias <- result_corrected_domain2
  }

  return(result_list)

}


#' Projection Estimator with Random Forest Algorithm
#'
#' @description
#' **Kim and Rao (2012)**, the synthetic data obtained through the model-assisted projection method can provide a useful tool for efficient domain estimation when the size of the sample in survey B is much larger than the size of sample in survey A.
#'
#' The function projects estimated values from a small survey (survey A) onto an independent large survey (survey B) using the random forest classification algorithm.
#' The two surveys are statistically independent, but the projection relies on shared auxiliary variables.
#' The process includes data preprocessing, feature selection, model training, and domain-specific estimation based on survey design principles "two stages one phase".
#' The function automatically selects standard estimation or bias-corrected estimation based on the parameter \code{bias_correction}.
#'
#' \code{bias_correction = TRUE} can only be used if there is \code{psu, ssu, strata} on the \code{data_model}. If it doesn't, then it will automatically be \code{bias_correction = FALSE}
#'
#' @references
#' \enumerate{
#'    \item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
#' }
#'
#' @inheritParams projection_rf
#' @param bias_correction Logical; if \code{TRUE}, then bias correction is applied, if \code{FALSE}, then bias correction is not applied. Default is \code{FALSE}.
#'
#' @return A list containing the following elements:
#' \itemize{
#'    \item \code{model} The trained Random Forest model.
#'    \item \code{importance} Feature importance showing which features contributed most to the model's predictions.
#'    \item \code{train_accuracy} Accuracy of the model on the training set.
#'    \item \code{validation_accuracy} Accuracy of the model on the validation set.
#'    \item \code{validation_performance} Confusion matrix for the validation set, showing performance metrics like accuracy, precision, recall, etc.
#'    \item \code{data_proj} The projection data with predicted values.
#' }
#'
#' if \code{bias_correction = FALSE}:
#' \itemize{
#'    \item \code{Domain1} Estimations for Domain 1, including estimated values, variance, and relative standard error (RSE).
#'    \item \code{Domain2} Estimations for Domain 2, including estimated values, variance, and relative standard error (RSE).
#' }
#'
#' if \code{bias_correction = TRUE}:
#' \itemize{
#'    \item \code{Direct} Direct estimations for Domain 1, including estimated values, variance, and relative standard error (RSE).
#'    \item \code{Domain1_corrected_bias} Bias-corrected estimations for Domain 1, including estimated values, variance, and relative standard error (RSE).
#'    \item \code{Domain2_corrected_bias} Bias-corrected estimations for Domain 2, including estimated values, variance, and relative standard error (RSE).
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(survey)
#' library(caret)
#' library(dplyr)
#'
#' data_A <- df_svy_A
#' data_B <- df_svy_B
#'
#' # Get predictor variables from data_model
#' x_predictors <- data_A %>% select(5:19) %>% names()
#'
#' # Run projection_randomforest with bias correction
#' rf_proj_corrected <- projection_randomforest(
#'                 data_model = data_A,
#'                 target_column = "Y",
#'                 predictor_cols = x_predictors,
#'                 data_proj = data_B,
#'                 domain1 = "province",
#'                 domain2 = "regency",
#'                 psu = "num",
#'                 ssu = NULL,
#'                 strata = NULL,
#'                 weights = "weight",
#'                 feature_selection = TRUE,
#'                 bias_correction = TRUE)
#'
#' rf_proj_corrected$Direct
#' rf_proj_corrected$Domain1_corrected_bias
#' rf_proj_corrected$Domain2_corrected_bias
#'
#' }
#'@md
projection_randomforest <- function(data_model, target_column, predictor_cols, data_proj,
                                    domain1, domain2, psu, ssu = NULL, strata = NULL, weights,
                                    split_ratio = 0.8, feature_selection = TRUE,
                                    bias_correction = FALSE) {

  # Check if PSU, SSU, and strata exist in data_model when bias correction is enabled
  if (bias_correction) {
    if (!all(c(psu, ssu, strata) %in% names(data_model))) {
      cli::cli_warn("PSU, SSU, or strata not found in data_model. Calculating indirect estimation without bias correction.", class = "custom_warning")
      bias_correction <- FALSE  # Disable bias correction if PSU, SSU, or strata are missing
    }
  }

  # Select the appropriate function based on bias_correction
  if (bias_correction) {
    cli::cli_inform("Info: Bias correction is enabled. Calculating indirect estimation with bias correction.")
    return(projection_rf_CorrectedBias(data_model, target_column, predictor_cols, data_proj,
                                       domain1, domain2, psu, ssu, strata, weights,
                                       split_ratio, feature_selection))
  } else {
    cli::cli_inform("Info: Bias correction is disabled. Calculating indirect estimation without bias correction.")
    return(projection_rf(data_model, target_column, predictor_cols, data_proj,
                         domain1, domain2, psu, ssu, strata, weights,
                         split_ratio, feature_selection))
  }
}
