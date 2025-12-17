#' Projection Estimator with XGBoost Algorithm
#'
#' @description
#' **Kim and Rao (2012)**, proposed a model-assisted projection estimation method for two independent surveys, where the first survey  (**A1**) has a large sample that only collects auxiliary variables, while the second survey (**A1**) has a smaller sample but contains information on both the focal variable and auxiliary variables.
#' This method uses a **Working Model (WM)** to relate the focal variable to the auxiliary variable based on data from  **A2**, and then predicts the value of the focal variable for **A1**. A projection estimator is then obtained from the (**A2**) sample using the resulting synthetic values.
#' This approach produces estimators that are asymptotically unbiased and can improve the efficiency of domain estimation, especially when the sample size in survey 1 is much larger compared to survey 2.
#'
#' This function applies the XGBoost algorithm to project estimated values from a small survey onto an independent larger survey.
#' While the two surveys are statistically independent, the projection is based on common auxiliary variables.
#' The process in this function involves data preprocessing, feature selection, getting the best model with hyperparameter tuning, and performing domain-specific estimation following survey design principles.
#'
#' @references
#' \enumerate{
#'  \item Kim, J. K., & Rao, J. N. (2012). Combining data from two independent surveys: a model-assisted approach. Biometrika, 99(1), 85-100.
#'  \item Kim and Rao (2012), the synthetic data obtained through the model-assisted projection method can provide a useful tool for efficient domain estimation when the size of the sample in survey 1 is much larger than the size of sample in survey 2.
#' }
#'
#' @param target_col The name of the column that contains the target variable in the \code{data_model}.
#' @param data_model A data frame or a data frame extension (e.g., a tibble) representing the training dataset, which consists of auxiliary variables and the target variable. This dataset is characterized by a smaller sample size and provides information on both the variable of interest and the auxiliary variables.
#' @param data_proj A data frame or a data frame extension (e.g., a tibble) representing the projection dataset, which is characterized by a larger sample size that collects only auxiliary information or general-purpose variables. This dataset must contain the same auxiliary variables as the \code{data_model} and is used for making predictions based on the trained model.
#' @param domain1  Domain variables for higher-level survey estimation. (e.g., "province")
#' @param domain2 Domain variables for more granular survey estimation at a lower administrative level. (e.g., "regency")
#' @param id Column name specifying cluster ids from the largest level to the smallest level, where ~0 or ~1 represents a formula indicating the absence of clusters.
#' @param weight The name of the column in \code{data_proj} that represents the survey weight, usually used for the purpose of indirect estimation .
#' @param STRATA The name of the column that specifies the strata; set to NULL if no stratification is required.#' @param test_size Proportion of data used for training (default is 0.8, meaning 80% for training and 20% for validation).
#' @param nfold The number of data partitions used for cross-validation (n-fold validation).
#' @param test_size The proportion of data used for testing, with the remaining data used for training.
#' @param task_type A string that specifies the modeling objective, indicating whether the task is for classification or regression.
#' Use "classification" for tasks where the goal is to categorize data into discrete classes, such as predicting whether an email is spam or not.
#' Use "regression" for tasks where the goal is to predict a continuous outcome, such as forecasting sales revenue or predicting house prices.
#' @param corrected_bias A logical value indicating whether to apply bias correction to the estimation results from the modeling process.
#' When set to TRUE, this parameter ensures that the estimates are adjusted to account for any systematic biases, leading to more accurate and reliable predictions.
#' @param feature_selection Selection of predictor variables (default is \code{TRUE})
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{metadata}}{A list of metadata about the modeling process, including:
#'     \itemize{
#'       \item \code{method}: Description of the method used (e.g., "Projection Estimator With XGBoost Algorithm"),
#'       \item \code{model_type}: The type of model, either "classification" or "regression",
#'       \item \code{feature_selection_used}: Logical, whether feature selection was used,
#'       \item \code{corrected_bias_applied}: Logical, whether bias correction was applied,
#'       \item \code{n_features_used}: Number of predictor variables used,
#'       \item \code{model_params}: The hyperparameters and settings of the final XGBoost model,
#'       \item \code{features_selected} (optional): Names of features selected, if feature selection was applied.
#'     }
#'   }
#'
#'   \item{\code{estimation}}{A list of projection estimation results, including:
#'     \itemize{
#'       \item \code{projected_data}: The dataset used for projection (e.g., kabupaten/kota) with predicted values,
#'       \item \code{domain1_estimation}: Estimated values for domain 1 (e.g., province level), including:
#'         \itemize{
#'           \item \code{Estimation}, \code{RSE}, \code{var}
#'         },
#'       \item \code{domain2_estimation}: Estimated values for domain 2 (e.g., regency level), including:
#'         \itemize{
#'           \item \code{Estimation}, \code{RSE}, \code{var}
#'         }
#'     }
#'   }
#'
#'   \item{\code{performance}}{(Only if applicable) A list of model performance metrics:
#'     \itemize{
#'       \item \code{mean_train_accuracy}, \code{final_accuracy}, \code{confusion_matrix} (for classification),
#'       \item \code{mean_train_rmse}, \code{final_rmse} (for regression).
#'     }
#'   }
#'
#'   \item{\code{bias_correction}}{(Optional) A list of bias correction results, returned only if \code{corrected_bias = TRUE}, including:
#'     \itemize{
#'       \item \code{direct_estimation}: Direct estimation before correction,
#'       \item \code{corrected_domain1}: Bias-corrected estimates for domain 1,
#'       \item \code{corrected_domain2}: Bias-corrected estimates for domain 2.
#'     }
#'   }
#' }
#'
#' @export
#' @import xgboost
#' @import caret
#' @import FSelector
#' @import glmnet
#' @importFrom stats coef
#' @importFrom stats setNames
#'
#' @examples
#' \donttest{
#' library(xgboost)
#' library(caret)
#' library(FSelector)
#' library(glmnet)
#' library(recipes)
#'
#' Data_A <- df_svy_A
#' Data_B <- df_svy_B
#'
#'hasil <- projection_xgboost(
#'                             target_col = "Y",
#'                             data_model = Data_A,
#'                             data_proj = Data_B,
#'                             id = "num",
#'                             STRATA = NULL,
#'                             domain1 = "province",
#'                             domain2 = "regency",
#'                             weight = "weight",
#'                             nfold = 3,
#'                             test_size = 0.2 ,
#'                             task_type = "classification",
#'                             corrected_bias = TRUE,
#'                             feature_selection = TRUE)
#' }
#' @md
projection_xgboost <- function(target_col, data_model, data_proj, id, STRATA = NULL, domain1, domain2, weight, task_type, test_size = 0.2 , nfold = 5, corrected_bias = FALSE, feature_selection = TRUE) {
  set.seed(999)

  # Load Data
  cat("\nLoad data...\n")
  if (!is.data.frame(data_model)) {
    data_model <- as.data.frame(data_model)
  }
  if (!is.data.frame(data_proj)) {
    data_proj <- as.data.frame(data_proj)
  }
  Y <- data_model[[target_col]]

  # Automatically detect Y data type
  detect_task_type <- function(Y, task_type) {
    # First, validate that task_type is either "regression" or "classification"
    valid_task_types <- c("regression", "classification")
    if (!task_type %in% valid_task_types) {
      stop(paste("Error: task_type must be 'regression' or 'classification'. You entered '",
                 task_type, "'", sep = ""))
    }

    # Then, proceed with the existing task type detection
    if (is.numeric(Y) && length(unique(Y)) > 10) {
      suggested_task <- "regression"
    } else if (is.factor(Y) || (is.numeric(Y) && length(unique(Y)) <= 10)) {
      num_classes <- length(unique(Y))
      suggested_task <- ifelse(num_classes > 2, "multiclass", "binary")
    } else {
      stop("Error: Data is not suitable for regression or classification.")
    }

    # Check if the specified task_type is appropriate for the data
    if ((task_type == "regression" && suggested_task != "regression") ||
        (task_type == "classification" && suggested_task == "regression")) {

      cat("Warning: Selected task_type is", task_type,
          "but the data is more suitable for", suggested_task, "\n")

      # confirmation from user
      response <- readline(prompt = "Do you want to continue using your selected task type? (yes/no): ")

      if (tolower(response) == "yes") {
        cat("Proceeding with user-defined task type:", task_type, "\n")
        return(task_type)
      } else {
        stop("Process stopped by user due to task type mismatch.")
      }
    }
    return(suggested_task)
  }
  validated_task_type <- detect_task_type(Y, task_type)
  cat("\nFinal Task Type:", validated_task_type, "\n")

  data_model_original <- data_model
  data_proj_original <- data_proj
  if (base::grepl("\\+", id)) {
    id_components <- base::unlist(base::strsplit(id, "\\+"))
    id_components <- base::trimws(id_components)
  } else {
    id_components <- id
  }

  if (task_type == "classification"){
    unique_classes <- length(unique(Y))
    multi_class <- unique_classes > 2
    num_classes <- unique_classes
  }

  if (feature_selection == TRUE){
    # Handling Missing Values (Preprocessing Step)
    base::cat("\nHandling missing values in preprocessing...\n")
    impute_missing_values <- function(df) {
      for (col in base::names(df)) {
        if (base::is.numeric(df[[col]])) {
          unique_vals <- base::unique(stats::na.omit(df[[col]]))

          if (base::all(unique_vals %in% c(0, 1))) {
            mode_value <- base::as.numeric(base::names(base::which.max(base::table(df[[col]], useNA = "no"))))
            df[[col]][base::is.na(df[[col]])] <- mode_value
          } else {
            df[[col]][base::is.na(df[[col]])] <- stats::median(df[[col]], na.rm = TRUE)
          }
        } else if (base::is.factor(df[[col]]) || base::is.character(df[[col]])) {
          mode_value <- base::names(base::which.max(base::table(df[[col]], useNA = "no")))
          if (base::length(mode_value) > 0) {
            df[[col]][base::is.na(df[[col]])] <- mode_value
          }
        }
      }
      return(df)
    }

    data_model_imputed <- impute_missing_values(data_model)
    data_proj_imputed <- impute_missing_values(data_proj)

    # Feature Selection
    cat("\nPerforming feature selection...\n")
    names(data_model) <- gsub("/", "_", names(data_model_imputed))
    names(data_proj) <- gsub("/", "_", names(data_proj_imputed))
    excluded_columns <- c(id_components, STRATA, weight, domain1, domain2, "no_sample","no_household","weight","ID")
    data_model_filtered <- data_model_imputed[, !(names(data_model_imputed) %in% excluded_columns)]

    if (task_type == "classification"){
      data_model_filtered$Y <- as.factor(Y)
    } else if (task_type == "regression") {
      data_model_filtered$Y <- as.numeric(as.character(Y))
    }
    predictor_names <- setdiff(names(data_model_filtered), c(target_col, "Y"))

    # Feature Selection Methods
    rf_model <- randomForest::randomForest(Y ~ ., data = data_model_filtered, importance = TRUE)
    if (task_type == "classification"){
      rf_imp <- randomForest::importance(rf_model)[, "MeanDecreaseAccuracy"]
    } else if (task_type == "regression") {
      rf_imp <- randomForest::importance(rf_model)[, "IncNodePurity"]
    }
    threshold_rf <- stats::median(rf_imp)
    rf_selected <- names(rf_imp)[rf_imp > threshold_rf]

    variances <- sapply(data_model_filtered[, predictor_names], stats::var)
    threshold_llcfs <- stats::median(variances)
    llcfs_selected <- names(variances)[variances > threshold_llcfs]

    cfs_formula <- stats::as.formula(paste("Y ~", paste(predictor_names, collapse = " + ")))
    cfs_selected <- FSelector::cfs(cfs_formula, data = data_model_filtered)

    y_num <- as.numeric(as.character(Y))
    correlations <- sapply(data_model_filtered[, predictor_names], function(x) abs(stats::cor(x, y_num)))
    threshold_udfs <- stats::median(correlations)
    udfs_selected <- names(correlations)[correlations > threshold_udfs]

    x <- stats::model.matrix(Y ~ ., data = data_model_filtered)[, -1]
    if (task_type == "classification"){
      if (num_classes > 2) {
        # Multi-kelas
        y <- as.numeric(as.factor(Y)) - 1
        family_type <- "multinomial"
      } else {
        # Biner
        y <- as.numeric(as.factor(Y)) - 1
        family_type <- "binomial"
      }
    } else if (task_type == "regression") {
      y <- as.numeric(Y)
      family_type <- "gaussian"
    }

    cv_lasso <- glmnet::cv.glmnet(x, y, alpha = 1, family = family_type, nfold = nfold)
    lasso_coef <- coef(cv_lasso, s = "lambda.min")
    if (is.list(lasso_coef)) {
      lasso_coef_matrix <- do.call(cbind, lasso_coef)
      lasso_selected <- rownames(lasso_coef_matrix)[rowSums(lasso_coef_matrix != 0) > 0]
    } else {
      lasso_selected <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
    }
    lasso_selected <- setdiff(lasso_selected, "(Intercept)")

    selected_list <- list(
      RF = rf_selected,
      LLCFS = llcfs_selected,
      CFS = cfs_selected,
      UDFS = udfs_selected,
      LASSO = lasso_selected
    )

    all_features <- unique(unlist(selected_list))
    votes <- sapply(all_features, function(feat) sum(sapply(selected_list, function(sel) feat %in% sel)))
    final_selected <- names(votes)[votes >= 3]
  }

  data_model <- data_model_original
  data_proj <- data_proj_original
  excluded_columns <- c(id_components, STRATA, weight, domain1, domain2, "no_sample","no_household","weight","ID")
  data_model_filtered <- data_model[, !(names(data_model) %in% excluded_columns)]
  if (task_type == "classification") {
    base::cat("\nApplying SMOTE to balance classes...\n")
    data_model_filtered[[target_col]] <- base::as.factor(data_model_filtered[[target_col]])
    class_counts <- base::table(data_model_filtered[[target_col]])
    max_min_ratio <- base::max(class_counts) / base::min(class_counts)
    smote_ratio <- base::min(max_min_ratio, 1.5)

    recipe_smote <- recipes::recipe(stats::as.formula(base::paste(target_col, "~ .")), data = data_model_filtered) %>%
      themis::step_smote(recipes::all_outcomes(), over_ratio = smote_ratio, neighbors = base::min(5, base::min(class_counts) - 1)) %>%
      recipes::prep() %>%
      recipes::bake(new_data = NULL)

    data_model_filtered <- recipe_smote
  }

  # Prepare Data for XGBoost
  cat("\nPreparing data for XGBoost...\n")
  if(feature_selection == FALSE){
    final_selected <- setdiff(names(data_model_filtered), target_col)
  }

  X <- as.matrix(data_model_filtered[, final_selected, drop = FALSE])
  if (task_type == "classification"){
    y <- as.numeric(as.factor(data_model_filtered[[target_col]])) - 1
    num_class <- length(unique(y))
    if (any(y < 0 | y >= num_class)) {
      stop("Error: Labels are not in the correct format. Please ensure that y values are within the range [0, num_class - 1].")
    }
  } else if (task_type == "regression") {
    y <- as.numeric(data_model_filtered[[target_col]])
  }

  train_index <- base::sample.int(n = nrow(data_model_filtered),
                                  size = round((1 - test_size) * nrow(data_model_filtered)),
                                  replace = FALSE)

  X_train <- X[train_index, , drop = FALSE]
  y_train <- y[train_index]
  X_test <- X[-train_index, , drop = FALSE]
  y_test <- y[-train_index]

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgboost::xgb.DMatrix(data = X_test, label = y_test)

  # Hyperparameter Tuning
  cat("\nPerforming hyperparameter tuning...\n")
  param_grid <- base::expand.grid(
    eta = c(0.1, 0.3),
    max_depth = c(3, 7) ,
    min_child_weight = c(1, 3),
    subsample = c(0.8, 1.0),
    colsample_bytree = c(0.7, 1.0),
    lambda = c(0, 1),
    alpha = c(0, 0.5)
  )

  best_params <- NULL
  best_nrounds <- NULL
  train_accuracies <- c()
  train_rmse_values <- c()

  if (task_type == "classification") {
    if (!base::exists("multi_class")) stop("Error: Variable 'multi_class' is not defined.")
    objective <- if (multi_class) "multi:softprob" else "binary:logistic"
    eval_metric <- if (multi_class) "mlogloss" else "logloss"
    best_score <- -Inf
  } else if (task_type == "regression") {
    objective <- "reg:squarederror"
    eval_metric <- "rmse"
    best_score <- Inf
  }

  for (i in 1:nrow(param_grid)) {
    params <- list(
      objective = objective,
      eval_metric = eval_metric,
      eta = param_grid$eta[i],
      max_depth = param_grid$max_depth[i],
      min_child_weight = param_grid$min_child_weight[i],
      subsample = param_grid$subsample[i],
      colsample_bytree = param_grid$colsample_bytree[i],
      lambda = param_grid$lambda[i],
      alpha = param_grid$alpha[i],
      nthread = parallel::detectCores()
    )

    if (task_type == "classification" && multi_class) {
      params$num_class <- num_classes
    } else {
      params$num_class <- NULL
    }

    cv_model <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 1000,
      nfold = nfold,
      stratified = TRUE,
      verbose = 0,
      early_stopping_rounds = 50
    )

    best_iteration <- cv_model$best_iteration
    trained_model <- xgboost::xgboost(
      params = params,
      data = dtrain,
      nrounds = best_iteration,
      verbose = 0
    )

    # Evaluate on training data
    train_predictions <- stats::predict(trained_model, dtrain)
    if (task_type == "classification"){
      if (multi_class) {
        train_pred_matrix <- base::matrix(train_predictions, nrow = length(y_train), byrow = TRUE)
        train_pred_labels <- base::max.col(train_pred_matrix) - 1
      } else {
        train_pred_labels <- base::ifelse(train_predictions > 0.5, 1, 0)
      }
      cm_train <- caret::confusionMatrix(factor(train_pred_labels), factor(y_train))
      train_accuracy <- cm_train$overall["Accuracy"]
      train_accuracies <- c(train_accuracies, train_accuracy)
    }else if (task_type == "regression") {
      train_rmse <- sqrt(base::mean((train_predictions - y_train) ^ 2))
      train_rmse_values <- c(train_rmse_values, train_rmse)
    }

    if (task_type == "classification"){
      if (train_accuracy > best_score) {
        best_score <- train_accuracy
        best_params <- params
        best_nrounds <- best_iteration
      }
    } else if (task_type == "regression") {
      if (train_rmse < best_score) {
        best_score <- train_rmse
        best_params <- params
        best_nrounds <- best_iteration
      }
    }
  }

  final_model <- xgboost::xgboost(
    params = best_params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )

  cat("\nEvaluating model...\n")
  predictions <- stats::predict(final_model, dtest)
  if (task_type == "classification"){
    if (multi_class) {
      pred_matrix <- base::matrix(predictions, nrow = length(y_test), byrow = TRUE)
      pred_labels <- base::max.col(pred_matrix) - 1
    } else {
      pred_labels <- base::ifelse(predictions > 0.5, 1, 0)
    }
    cm <- caret::confusionMatrix(factor(pred_labels), factor(y_test))
    accuracy <- cm$overall["Accuracy"]
  } else if (task_type == "regression") {
    test_rmse <- sqrt(base::mean((predictions - y_test) ^ 2))
  }

  data_proj_filtered <- data_proj[, final_selected, drop = FALSE]
  if ("P.hat" %in% base::colnames(data_proj_filtered)) {
    data_proj_filtered$P.hat <- NULL
  }
  data_proj_matrix <- base::apply(data_proj_filtered, 2, as.numeric)
  data_proj_matrix <- as.matrix(data_proj_matrix)
  if (!base::identical(base::colnames(data_proj_matrix), base::colnames(X))) {
    stop("Error: Column names of data_proj do not match the training data!")
  }

  ddata_proj <- xgboost::xgb.DMatrix(data = data_proj_matrix)
  pred <- stats::predict(final_model, newdata = ddata_proj)
  if (task_type == "classification") {
    if (multi_class) {
      pred_matrix <- base::matrix(pred, nrow = nrow(data_proj), byrow = TRUE)
      pred_labels <- base::max.col(pred_matrix) - 1
    } else {
      pred_labels <- base::ifelse(pred > 0.5, 1, 0)
    }
  }

  if (task_type == "classification" && base::length(pred_labels) == nrow(data_proj)) {
    data_proj$P.hat <- pred_labels
  } else if (task_type == "regression" && base::length(pred) == nrow(data_proj)){
    data_proj$P.hat <- pred
  } else {
    stop("Error: Prediction size does not match the number of rows in data_proj!")
  }

  cat("\nPerforming survey estimation...\n")
  options(survey.adjust.domain.lonely = TRUE, survey.lonely.psu = "adjust")
  id_vars <- unlist(strsplit(id, "\\+"))
  id_formula <- stats::as.formula(paste("~", paste(id_vars, collapse = " + ")))
  strata_formula <- if (!is.null(STRATA)) stats::as.formula(paste("~", STRATA)) else NULL
  data_design <- survey::svydesign(
    id = id_formula,
    strata = strata_formula,
    weights = stats::as.formula(paste("~", weight)),
    data = data_proj,
    nest = TRUE
  )

  Phat.proj.domain1 <- survey::svyby(
    formula = stats::as.formula("~P.hat"),
    by = stats::as.formula(paste("~", domain1)),
    design = data_design,
    FUN = survey::svymean,
    vartype = c("cvpct","var")
  ) %>% dplyr::rename(
    Estimation = P.hat,
    Variance = 'var',
    RSE = 'cv%'
  ) %>% dplyr::mutate(
    Estimation = round(Estimation * 100, 2),
    RSE = round(RSE, 2)
  )

  Phat.proj.domain2 <- survey::svyby(
    formula = stats::as.formula("~P.hat"),
    by = stats::as.formula(paste("~", domain1, "+", domain2)),
    design = data_design,
    FUN = survey::svymean,
    vartype = c("cvpct","var")
  ) %>% dplyr::rename(
    Estimation = P.hat,
    Variance = 'var',
    RSE = 'cv%'
  ) %>% dplyr::mutate(
    Estimation = round(Estimation * 100, 2),
    RSE = round(RSE, 2)
  )

  if (corrected_bias == TRUE){
    base::cat("\nComputing Corrected Bias...\n")
    base::cat("\nComputing Direct Estimate...\n")
    data_direct <- data_model_original

    # Ensure PSU, SSU, and STRATA exist in the data
    required_vars <- base::c(id_vars, STRATA)
    missing_vars <- base::setdiff(required_vars, base::colnames(data_direct))

    if (base::length(missing_vars) > 0) {
      base::stop(base::paste("Error: The following required variables are missing:",
                             base::paste(missing_vars, collapse = ", "),
                             "\nPlease provide these variables in the dataset."))
    } else {
      base::cat("\nPSU, SSU, and STRATA are available. Proceeding with the data...\n")
    }

    data_design_dir <- survey::svydesign(
      id = id_formula,
      strata = strata_formula,
      weights = stats::as.formula(base::paste("~", weight)),
      data = data_direct,
      nest = TRUE
    )

    direct.estimate <- survey::svyby(
      formula = stats::as.formula(paste("~", target_col)),
      by = stats::as.formula(paste("~", domain1)),
      design = data_design_dir,
      FUN = survey::svymean,
      vartype = base::c("cvpct","var")
    ) %>% dplyr::rename(
      Estimation = dplyr::all_of(target_col),
      Variance = 'var',
      RSE = 'cv%'
    ) %>% dplyr::mutate(
      Estimation = base::round(Estimation * 100, 2),
      RSE = base::round(RSE, 2)
    )

    base::cat("\nCorrected Bias...\n")
    data_matrix <- base::as.matrix(data_direct[, final_selected])
    predict <- stats::predict(final_model, data_matrix)
    if (task_type == "classification") {
      if (multi_class) {
        pred_matrix <- base::matrix(predict, nrow = nrow(data_direct), byrow = TRUE)
        pred_label <- base::max.col(pred_matrix) - 1
      } else {
        pred_label <- base::ifelse(predict > 0.5, 1, 0)
      }
      data_direct$predict <- pred_label
    } else if (task_type=="regression"){
      data_direct$predict <- predict
    }

    data_design_predict <- survey::svydesign(
      id = id_formula,
      strata = strata_formula,
      weights = stats::as.formula(base::paste("~", weight)),
      data = data_direct,
      nest = TRUE
    )

    predict.estimate <- survey::svyby(
      formula = ~predict,
      by = stats::as.formula(paste("~", domain1)),
      design = data_design_predict,
      FUN = survey::svymean,
      vartype = base::c("cvpct","var")
    ) %>% dplyr::rename(
      Estimation = predict,
      Variance = 'var',
      RSE = 'cv%'
    ) %>% dplyr::mutate(
      Estimation = base::round(Estimation * 100, 2),
      RSE = base::round(RSE, 2)
    )

    base::cat("\nWeight Index...\n")
    weight_domain1 <- data_proj %>%
      dplyr::group_by(!!rlang::sym(domain1)) %>%
      dplyr::summarise(weight_domain1 = sum(as.numeric(as.character(!!rlang::sym(weight))), na.rm = TRUE), .groups = "drop")

    weight_domain2 <- data_proj %>%
      dplyr::group_by(!!rlang::sym(domain1), !!rlang::sym(domain2)) %>%
      dplyr::summarise(weight_domain2 = sum(as.numeric(as.character(!!rlang::sym(weight))), na.rm = TRUE), .groups = "drop")

    result_wi <- weight_domain2 %>%
      dplyr::left_join(weight_domain1, by = domain1) %>%
      dplyr::mutate(wi = weight_domain2 / weight_domain1)

    base::cat("\nBias...\n")
    bias_est <- ifelse(
      Phat.proj.domain1$Estimation > direct.estimate$Estimation,
      direct.estimate$Estimation - predict.estimate$Estimation,
      predict.estimate$Estimation - direct.estimate$Estimation
    )
    var_bias_est <- predict.estimate$Variance + direct.estimate$Variance

    bias_lookup <- data.frame(
      domain1_val = predict.estimate[[domain1]],
      bias_est = bias_est,
      var_bias_est = var_bias_est
    ) %>%
      stats::setNames(c(domain1, "bias_est", "var_bias_est"))

    domain2_corrected <- Phat.proj.domain2 %>%
      dplyr::left_join(bias_lookup, by = domain1) %>%
      dplyr::mutate(
        Est_corrected = Estimation + bias_est,
        Est_corrected = round(Est_corrected, 2),
        Var_corrected = Variance + var_bias_est,
        Var_corrected = round(Var_corrected, 6),
        RSE_corrected = (sqrt(Var_corrected) / (Est_corrected / 100)) * 100,
        RSE_corrected = round(RSE_corrected, 2)
      )

    result_corrected_domain2 <- domain2_corrected %>%
      dplyr::left_join(result_wi, by = c(domain1, domain2))

    domain1_corrected <- result_corrected_domain2 %>%
      dplyr::group_by(!!rlang::sym(domain1)) %>%
      dplyr::summarise(
        Est_corrected = sum(Est_corrected * wi, na.rm = TRUE),
        Var_corrected = sum((wi^2) * Var_corrected, na.rm = TRUE),
        .groups = "drop"
      ) %>% dplyr::mutate(
        Est_corrected = round(Est_corrected, 2),
        Var_corrected = round(Var_corrected, 6),
        RSE_corrected = (sqrt(Var_corrected) / (Est_corrected / 100)) * 100,
        RSE_corrected = round(RSE_corrected, 2)
      )
    cat("\nCorrected Bias Calculation Completed...\n")
  }

  return_list <- list(
    metadata = list(
      method = "Projection Estimator With XGBoost Algorithm",
      model_type = task_type,
      feature_selection_used = feature_selection,
      corrected_bias_applied = corrected_bias,
      n_features_used = length(final_model$feature_names),
      model_params = final_model$params
    ),

    estimation = list(
      projected_data = data_proj,
      domain1_estimation = Phat.proj.domain1,
      domain2_estimation = Phat.proj.domain2
    )
  )

  if (feature_selection) {
    return_list$metadata$features_selected <- final_selected
  }

  if (task_type == "classification") {
    return_list$performance <- list(
      mean_train_accuracy = mean(train_accuracies),
      final_accuracy = accuracy,
      confusion_matrix = cm
    )
  }

  if (task_type == "regression") {
    return_list$performance <- list(
      mean_train_rmse = mean(train_rmse_values),
      final_rmse = test_rmse
    )
  }

  if (corrected_bias) {
    return_list$bias_correction <- list(
      direct_estimation = direct.estimate,
      corrected_domain1 = domain1_corrected,
      corrected_domain2 = domain2_corrected
    )
  }
  return(return_list)
}

utils::globalVariables(c(
  "P.hat", "Estimation","Variance", "RSE", "Est_corrected",
  "Var_corrected", "RSE_corrected", "wi"
))
