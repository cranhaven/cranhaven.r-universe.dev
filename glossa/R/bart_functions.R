#' Fit a BART Model Using Environmental Covariate Layers
#'
#' This function fits a Bayesian Additive Regression Trees (BART) model using
#' presence/absence data and environmental covariate layers.
#'
#' @param y A numeric vector indicating presence (1) or absence (0).
#' @param x A data frame with the same number of rows as the length of the vector `y`, containing the covariate values.
#' @param seed An optional integer value for setting the random seed for reproducibility.
#' @param ... Additional arguments passed to `dbarts::bart()`.
#'
#' @return A BART model object.
#'
#' @export
fit_bart_model <- function(y, x, seed = NULL, ...) {
  # Input validation
  if (!is.numeric(y) || !all(y %in% c(0, 1))) {
    stop("Argument 'y' must be a numeric vector containing only 0s and 1s.")
  }

  if (!is.data.frame(x)) {
    stop("Argument 'x' must be a data frame.")
  }

  if (length(y) != nrow(x)) {
    stop("Length of 'y' must match the number of rows in 'x'.")
  }

  # Set random seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Fit the BART model
  bart_model <- dbarts::bart(x.train = x,
                             y.train = y,
                             keeptrees = TRUE,
                             verbose = FALSE,
                             ...)

  # Make the model fit state invisible so we can export it and use it to predict
  invisible(bart_model$fit$state)

  # Return the fitted BART model object
  return(bart_model)
}

#' Make Predictions Using a BART Model
#'
#' This function makes predictions using a Bayesian Additive Regression Trees (BART) model
#' on a stack of environmental covariates ('SpatRaster').
#'
#' @param bart_model A BART model object obtained from fitting BART using the 'dbarts' package.
#' @param layers A SpatRaster object containing environmental covariates for prediction.
#' @param cutoff An optional numeric cutoff value for determining potential presences. If NULL, potential presences and absences will not be computed.
#'
#' @return A SpatRaster containing the mean, median, standard deviation, and quantiles
#' of the posterior predictive distribution, as well as a potential presences layer if cutoff is provided.
#'
#' @export
predict_bart <- function(bart_model, layers, cutoff = NULL) {
  predict.bart <- utils::getFromNamespace("predict.bart", "dbarts")
  tryCatch({
    # Define quantiles for posterior predictive distribution
    quantiles <- c(0.025, 0.975)

    # Convert raster stack to matrix (data.frame to handle categorical variables)
    input_matrix <- terra::as.matrix(layers)
    input_matrix <- as.data.frame(input_matrix)
    categorical_cols <- names(layers)[terra::is.factor(layers)]
    for (col in categorical_cols) {
      if (!is.null(terra::levels(layers[[col]])[[1]][, 2])) {
        fact_labels <- terra::levels(layers[[col]])[[1]][, 2]
      } else {
        fact_labels <- terra::levels(layers[[col]])[[1]][, 1]
      }
      input_matrix[[col]] <- factor(input_matrix[[col]], levels = terra::levels(layers[[col]])[[1]][, 1], labels = fact_labels)
    }

    # Initialize output data frame for predictions
    blank_output <- data.frame(matrix(ncol = (4 + length(quantiles) + ifelse(!is.null(cutoff), 1, 0)),
                                      nrow = terra::ncell(layers[[1]])))

    # Get indices of non-NA values in the input matrix
    which_vals <- which(complete.cases(input_matrix))

    # Remove NA values from the input matrix
    input_matrix <- input_matrix[complete.cases(input_matrix), , drop = FALSE]

    # Make predictions using the BART model
    pred <- predict.bart(bart_model, input_matrix)

    # Calculate summary statistics of predictions
    pred_data <- cbind(
      data.frame(matrixStats::colMeans2(pred)),
      data.frame(matrixStats::colMedians(pred)),
      data.frame(matrixStats::colSds(pred)),
      data.frame(matrixStats::colQuantiles(pred, probs = quantiles))
    )

    # Calculate the difference between upper and lower quantiles
    pred_data$diff <- pred_data$X97.5. - pred_data$X2.5.

    # Rename columns
    names(pred_data) <- c("mean", "median", "sd", "q0.025", "q0.975", "diff")

    # Calculate potential presences
    if (!is.null(cutoff)) {
      pred_data$potential_presences <- ifelse(pred_data$mean >= cutoff, 1, 0)
    }

    # Fill in the blank output with predictions
    blank_output[which_vals, ] <- as.matrix(pred_data)

    # Reshape output to SpatRaster format
    out_list <- lapply(seq_len(ncol(blank_output)), function(x) {
      output_matrix <- t(matrix(blank_output[, x], nrow = ncol(layers), ncol = nrow(layers)))
      return(terra::rast(output_matrix, extent = terra::ext(layers)))
    })

    # Convert list of matrices to raster stack
    out_list <- terra::rast(out_list)

    # Set names for the raster stack
    names(out_list) <- names(pred_data)

    # Set CRS
    terra::crs(out_list) <- terra::crs(layers)

    return(out_list)
  }, error = function(err) {
    message("Prediction failed: ", conditionMessage(err))
    return(NULL) # Return NULL if prediction fails
  })
}


#' Calculate Response Curve Using BART Model
#'
#' This function calculates the response curve (functional responses) using a Bayesian Additive Regression Trees (BART) model.
#'
#' @param bart_model A BART model object obtained from fitting BART ('dbarts::bart').
#' @param data A data frame containing the predictor variables (the design matrix) used in the BART model.
#' @param predictor_names A character vector containing the names of the predictor variables.
#'
#' @return A list containing a data frame for each independent variable with mean, 2.5th percentile, 97.5th percentile, and corresponding values of the variables.
#'
#' @export
response_curve_bart <- function(bart_model, data, predictor_names) {
  # Initialize xind and levs
  xind <- c()
  levs <- list()
  level_counts <- list()

  # Loop over predictor names to construct xind and levs
  for (name in predictor_names) {
    values <- na.omit(data[[name]])

    if (is.character(values) | is.factor(values)) {
      # Handle categorical variables - Filter levels with more than 0 counts
      observed_levels <- table(values)  # Count occurrences of each level
      observed_levels <- names(observed_levels[observed_levels > 0])
      # Create dummy variable names as dbarts
      if (length(observed_levels) == 2) {
        dummy_name <- paste0(name, ".", observed_levels[-1])
        xind <- c(xind, dummy_name)
        levs[[length(levs) + 1]] <- c(0, 1) # Dummy variable takes values 0 and 1
      } else {
        for (lvl in observed_levels) {
          dummy_name <- paste0(name, ".", lvl)
          xind <- c(xind, dummy_name)
          levs[[length(levs) + 1]] <- c(0, 1) # Dummy variable values
        }
      }
    } else {
      # Handle continuous variables
      seq_vals <- seq(min(values), max(values), length.out = 10)
      quant_vals <- quantile(values, prob = seq(0.05, 0.95, length.out = 10))
      combined_vals <- sort(unique(c(seq_vals, quant_vals)))
      xind <- c(xind, name)
      levs[[length(levs) + 1]] <- combined_vals
    }
  }

  # Obtain variable response information using pdbart
  var_r <- dbarts::pdbart(bart_model, xind = xind, levs = levs, pl = FALSE)

  # Compute inverse probit values for each predictor variable
  inv_probit_df <- lapply(var_r$fd, function(fd) {
    data.frame(matrix(mcp::phi(fd), nrow = nrow(fd), ncol = ncol(fd)))
  })

  # Calculate mean, 2.5th percentile, 97.5th percentile, and values of predictor variables
  data_var <- lapply(seq_along(predictor_names), function(i) {
    name <- predictor_names[i]
    values <- na.omit(data[[name]])

    if (is.character(values) | is.factor(values)) {
      # Handle categorical variables - Filter levels with more than 0 counts
      observed_levels <- table(values)  # Count occurrences of each level
      observed_levels <- names(observed_levels[observed_levels > 0])
      if (length(observed_levels) == 2) {
        # For two-level categorical variables, include 0 and 1
        dummy_name <- paste0(name, ".", observed_levels[-1])
        dummy_index <- which(xind == dummy_name)
        prob_inv <- colMeans(inv_probit_df[[dummy_index]], na.rm = TRUE)
        quantile_values_q25 <- apply(inv_probit_df[[dummy_index]], 2, quantile, probs = 0.025, na.rm = TRUE)
        quantile_values_q975 <- apply(inv_probit_df[[dummy_index]], 2, quantile, probs = 0.975, na.rm = TRUE)
        data.frame(value = c(observed_levels[1], observed_levels[2]),
                   mean = prob_inv,
                   q25 = quantile_values_q25,
                   q975 = quantile_values_q975)
      } else {
        # For multi-level categorical variables, use only the value 1 of the dummy
        dummy_results <- lapply(seq_along(observed_levels), function(j) {
          dummy_name <- paste0(name, ".", observed_levels[j])
          dummy_index <- which(xind == dummy_name)
          prob_inv <- colMeans(inv_probit_df[[dummy_index]], na.rm = TRUE)[2]
          quantile_values_q25 <- apply(inv_probit_df[[dummy_index]], 2, quantile, probs = 0.025, na.rm = TRUE)[2]
          quantile_values_q975 <- apply(inv_probit_df[[dummy_index]], 2, quantile, probs = 0.975, na.rm = TRUE)[2]
          data.frame(value = observed_levels[j], mean = prob_inv, q25 = quantile_values_q25, q975 = quantile_values_q975)
        })
        do.call(rbind, dummy_results)
      }
    } else {
      # Handle continuous variables
      covariate_index <- which(xind == name)
      prob_inv <- colMeans(inv_probit_df[[covariate_index]], na.rm = TRUE)
      quantile_values_q25 <- apply(inv_probit_df[[covariate_index]], 2, quantile, probs = 0.025, na.rm = TRUE)
      quantile_values_q975 <- apply(inv_probit_df[[covariate_index]], 2, quantile, probs = 0.975, na.rm = TRUE)
      value <- var_r$levs[[covariate_index]]
      data.frame(mean = prob_inv, q25 = quantile_values_q25, q975 = quantile_values_q975, value = value)
    }
  })

  # Set names for the data frames
  names(data_var) <- predictor_names

  return(data_var)
}

#' Variable Importance in BART Model
#'
#' This function computes the variable importance scores for a fitted BART (Bayesian Additive Regression Trees) model
#' using a permutation-based approach. It measures the impact of each predictor variable on the model's performance
#' by permuting the values of that variable and evaluating the change in performance (F-score is the performance metric).
#'
#' @param bart_model A BART model object.
#' @param y Vector indicating presence (1) or absence (0).
#' @param x Dataframe with same number of rows as the length of the vector `y` with the covariate values.
#' @param cutoff A numeric threshold for converting predicted probabilities into presence-absence.
#' @param n_repeats An integer indicating the number of times to repeat the permutation for each variable.
#' @param seed An optional seed for random number generation.
#' @return A data frame where each column corresponds to a predictor variable, and each row contains the variable importance scores across permutations.
#'
#' @export
variable_importance <- function(bart_model, y, x, cutoff = 0, n_repeats = 10, seed = NULL) {
  predict.bart <- utils::getFromNamespace("predict.bart", "dbarts")
  set.seed(seed)

  # ToDo: Get presence-absence data directly from BART object
  # y <- bart_model$fit$data@y
  # x <- as.data.frame(bart_model$fit$data@x)

  # Calculate baseline performance metric
  prob <- colMeans(predict.bart(bart_model, x))
  pred <- ifelse(prob >= cutoff, 1, 0)

  TP <- sum(y == 1 & pred == 1)
  FP <- sum(y == 0 & pred == 1)
  FN <- sum(y == 1 & pred == 0)
  pr <- TP/(TP + FP)
  sn <- TP/(TP + FN)
  baseline_f_score <- 2 * ((pr * sn) / (pr + sn))

  # Preallocate a matrix
  importance_scores <- matrix(NA, nrow = n_repeats, ncol = ncol(x))
  colnames(importance_scores) <- colnames(x)

  for (i in seq_len(n_repeats)){
    for (j in seq_len(ncol(x))){
      x_permuted <- x
      x_permuted[, j] <- sample(x[, j])  # Permute the j-th column

      # Predict with the permuted data
      prob <- colMeans(predict.bart(bart_model, x_permuted))
      pred <- ifelse(prob >= cutoff, 1, 0)

      # Calculate the performance metric after permutation
      TP <- sum(y == 1 & pred == 1)
      FP <- sum(y == 0 & pred == 1)
      FN <- sum(y == 1 & pred == 0)
      pr <- TP/(TP + FP)
      sn <- TP/(TP + FN)
      permuted_f_score <- 2 * ((pr * sn) / (pr + sn))

      # Calculate the importance score as the decrease in accuracy
      importance_scores[i, j] <- baseline_f_score - permuted_f_score
    }
  }

  # Convert the matrix to a data frame
  importance_scores <- as.data.frame(importance_scores, stringsAsFactors = FALSE)

  return(importance_scores)
}


#' Optimal Cutoff for Presence-Absence Prediction
#'
#' This function calculates the optimal cutoff for presence-absence prediction using a BART model.
#'
#' @param y Vector indicating presence (1) or absence (0).
#' @param x Dataframe with same number of rows as the length of the vector `y` with the covariate values.
#' @param model A BART model object.
#' @param seed Random seed for reproducibility.
#'
#' @return The optimal cutoff value for presence-absence prediction.
#'
#' @export
pa_optimal_cutoff <- function(y, x, model, seed = NULL) {
  predict.bart <- utils::getFromNamespace("predict.bart", "dbarts")
  set.seed(seed)
  pred_data <- predict.bart(model, newdata = x)
  pred_mean <- colMeans(pred_data)

  pa_cutoff <- optimalCutoff(
    actuals = y,
    predictedScores = pred_mean
  )

  return(pa_cutoff)
}

#' Cross-validation for BART model
#'
#' This function performs cross-validation for a Bayesian Additive Regression Trees (BART) model
#' using presence-absence data and environmental covariate layers. It calculates various performance metrics
#' for model evaluation.
#'
#' @param data Data frame with a column (named 'pa') indicating presence (1) or absence (0) and columns for the predictor variables.
#' @param folds A vector of fold assignments (same length as `data`).
#' @param predictor_cols Optional; a character vector of column names to be used as predictors. If NULL, all columns except 'pa' will be used.
#' @param seed Optional; random seed.
#'
#' @return A list with:
#' \describe{
#'   \item{metrics}{A data frame containing the true positives (TP), false positives (FP), false negatives (FN), true negatives (TN), and various performance metrics including precision (PREC), sensitivity (SEN), specificity (SPC), false discovery rate (FDR), negative predictive value (NPV), false negative rate (FNR), false positive rate (FPR), F-score, accuracy (ACC), balanced accuracy (BA), and true skill statistic (TSS) for each fold.}
#'   \item{predictions}{Data frame with observed, predicted, probability, and fold assignment per test instance.}
#' }
#'
#' @export
cross_validate_model <- function(data, folds, predictor_cols = NULL, seed = NULL) {
  predict.bart <- utils::getFromNamespace("predict.bart", "dbarts")
  set.seed(seed)

  if (is.null(predictor_cols)) {
    predictor_cols <- setdiff(colnames(data), "pa")
  }

  fold_index <- sort(unique(folds))
  TP <- FP <- FN <- TN <- AUC <- CBI <- c()
  predictions <- list()
  for (i in fold_index) {
    train_idx <- which(folds != i)
    test_idx <- which(folds == i)

    train <- data[train_idx, ]
    test <- data[test_idx, ]

    # Skip fold if only one class
    if (length(unique(train$pa)) < 2 || length(unique(test$pa)) < 2) {
      warning(paste("Skipping fold", i, "due to insufficient classes in training or testing data."))
      TP <- c(TP, NA)
      FP <- c(FP, NA)
      FN <- c(FN, NA)
      TN <- c(TN, NA)
      CBI <- c(CBI, NA)
      AUC <- c(AUC, NA)
      next
    }

    # Fit model
    bart_model <- fit_bart_model(
      y = train[, "pa"],
      x = train[, predictor_cols, drop = FALSE],
      seed = seed
    )

    # Compute optimal cutoff to predict presence/absence
    cutoff <- pa_optimal_cutoff(
      y = train[, "pa"],
      x = train[, predictor_cols, drop = FALSE],
      bart_model
    )

    probs <- colMeans(predict.bart(bart_model, test[, predictor_cols, drop = FALSE]))
    preds <- ifelse(probs >= cutoff, 1, 0)

    # Confusion matrices
    TP <- c(TP, sum(test$pa == 1 & preds == 1))
    FP <- c(FP, sum(test$pa == 0 & preds == 1))
    FN <- c(FN, sum(test$pa == 1 & preds == 0))
    TN <- c(TN, sum(test$pa == 0 & preds == 0))

    # Compute AUC
    auc_val <- tryCatch({
      pROC::auc(pROC::roc(test$pa, probs, levels = c(0, 1), direction = "<"))
    }, error = function(e) NA)
    AUC <- c(AUC, as.numeric(auc_val))

    # Compute Boyce Index
    boyce <- tryCatch({
      contBoyce(pres = probs[test$pa == 1], contrast = probs[test$pa == 0])
    }, error = function(e) NA)
    CBI <- c(CBI, boyce)

    predictions[[i]] <- data.frame(
      ID = test_idx,
      decimalLongitude = test$decimalLongitude,
      decimalLatitude = test$decimalLatitude,
      timestamp = test$timestamp,
      pa = test$pa,
      predicted = preds,
      probability = probs,
      fold = i
    )
  }

  # Metrics
  PREC <- TP / (TP + FP)
  SEN <- TP / (TP + FN)
  SPC <- TN / (TN + FP)
  FDR <- FP / (TP + FP)
  NPV <- TN / (FN + TN)
  FNR <- FN / (TP + FN)
  FPR <- FP / (FP + TN)
  Fscore <- 2 * ((PREC * SEN) / (PREC + SEN))
  ACC <- (TP + TN)/(TP + FP + FN + TN)
  BA <- (SEN + SPC) / 2
  TSS <- SEN + SPC - 1

  metrics <- data.frame(
    fold = fold_index,
    TP = TP, FP = FP, FN = FN, TN = TN,
    PREC = PREC, SEN = SEN, SPC = SPC, FDR = FDR, NPV = NPV, FNR = FNR, FPR = FPR,
    Fscore = Fscore, ACC = ACC, BA = BA, TSS = TSS,
    CBI = CBI, AUC = AUC,
    status = ifelse(is.na(TP), "skipped", "valid")
  )

  return(list(metrics = metrics, predictions = do.call(rbind, predictions)))
}
