#' Classify phenotypes via ensemble learning.
#'
#' The \code{ph_ensemble} function uses classification predictions from a list of algorithms to train an ensemble model.
#' This can be a list of manually trained algorithms from \code{train} or, more conveniently, the output from \code{ph_train}.
#' The hyperparameter tuning and model evaluations are handled internally to simplify the ensembling process. This function
#' assumes some preprocessing has been performed, hence the training, validation, and test set requirements.
#'
#' @param train_models A \code{list} of at least two \code{train} models.
#' @param train_df A \code{data.frame} containing a class column and the training data.
#' @param vali_df A \code{data.frame} containing a class column and the validation data.
#' @param test_df A \code{data.frame} containing a class column and the test data.
#' @param class_col A \code{character} value for the name of the class column. This should be consistent across data frames.
#' @param ctrl A \code{list} containing the resampling strategy (e.g., "boot") and other parameters for \code{trainControl}. Automatically create one via \code{ph_ctrl} or manually create it with \code{trainControl}.
#' @param train_seed A \code{numeric} value to set the training seed and control the randomness of creating resamples: 123 (default).
#' @param n_cores An \code{integer} value for the number of cores to include in the cluster: 2 (default). We highly recommend increasing this value to, e.g., parallel::detectCores() - 1.
#' @param task A \code{character} value for the type of classification \code{task}: "multi" (default), "binary".
#' @param metric A \code{character} value for which summary metric should be used to select the optimal model: "ROC" (default for "binary") and "Kappa" (default for "multi"). Other options include "logLoss", "Accuracy", "Mean_Balanced_Accuracy", and "Mean_F1".
#' @param top_models A \code{numeric} value for the top n training models to ensemble: 3 (default). Every training model is ordered according to their final metric value (e.g., "ROC" or "Kappa") and the top n models are selected.
#' @param metalearner A \code{character} value for the algorithm used to train the ensemble: "glmnet" (default), "rf". Other methods, such as those listed in ph_train methods, may also be used.
#' @param tune_length If \code{search = "random"} (default), this is an \code{integer} value for the maximum number of hyperparameter combinations to test for each training model in the ensemble; if \code{search = "grid"}, this is an \code{integer} value for the number of levels of each hyperparameter to test for each model.
#' @param quiet A \code{logical} value for whether progress should be printed: TRUE (default), FALSE.
#' @return A list containing the following components:\tabular{ll}{
#'    \code{ensemble_test_preds} \tab The ensemble predictions for the test set. \cr
#'    \tab \cr
#'    \code{vali_preds} \tab The validation predictions for the top models. \cr
#'    \tab \cr
#'    \code{test_preds} \tab The test predictions for the top models. \cr
#'    \tab \cr
#'    \code{all_test_preds} \tab The test predictions for every successfully trained model. \cr
#'    \tab \cr
#'    \code{all_test_results} \tab The confusion matrix results obtained from comparing the model test predictions (i.e., original models and ensemble) against the actual test classes.  \cr
#'    \tab \cr
#'    \code{ensemble_model} \tab The ensemble \code{train} object. \cr
#'    \tab \cr
#'    \code{var_imps} \tab The ensemble variable importances obtained via weighted averaging. The original train importances are multiplied by the model's importance in the ensemble, then averaged across models and normalized.  \cr
#'    \tab \cr
#'    \code{train_df} \tab The training data frame. \cr
#'    \tab \cr
#'    \code{vali_df} \tab The validation data frame. \cr
#'    \tab \cr
#'    \code{test_df} \tab The test data frame. \cr
#'    \tab \cr
#'    \code{train_models} \tab The \code{train} models for the ensemble. \cr
#'    \tab \cr
#'    \code{ctrl} \tab A \code{trainControl} object. \cr
#'    \tab \cr
#'    \code{metric} \tab The summary metric used to select the optimal model. \cr
#'    \tab \cr
#'    \code{task} \tab The type of classification task. \cr
#'    \tab \cr
#'    \code{tune_length} \tab The maximum number of hyperparameter combinations ("random") or individual hyperparameter depth ("grid").  \cr
#'    \tab \cr
#'    \code{top_models} \tab The number of top methods selected for the ensemble.  \cr
#'    \tab \cr
#'    \code{metalearner} \tab The algorithm used to train the ensemble. \cr
#' }
#' @export
#' @examples
#' ## Import data.
#' data(ph_crocs)
#' \donttest{
#' ## Remove anomalies with autoencoder.
#' rm_outs <- ph_anomaly(df = ph_crocs, ids_col = "Biosample",
#'                       class_col = "Species", method = "ae")
#' ## Preprocess anomaly-free data frame into train, validation, and test sets
#' ## with PCs as predictors.
#' pc_dfs <- ph_prep(df = rm_outs$df, ids_col = "Biosample",
#'                   class_col = "Species", vali_pct = 0.15,
#'                   test_pct = 0.15, method = "pca")
#' ## Echo control object for train function.
#' ctrl <- ph_ctrl(ph_crocs$Species, resample_method = "boot")
#' ## Train all models for ensemble.
#' ## Note: Increasing n_cores will dramatically reduce train time.
#' train_models <- ph_train(train_df = pc_dfs$train_df,
#'                          vali_df = pc_dfs$vali_df,
#'                          test_df = pc_dfs$test_df,
#'                          class_col = "Species",
#'                          ctrl = ctrl,
#'                          task = "multi",
#'                          methods = "all",
#'                          tune_length = 5,
#'                          quiet = FALSE)
#' ## You can also train just a few, although more is preferable.
#' ## Note: Increasing n_cores will dramatically reduce train time.
#' train_models <- ph_train(train_df = pc_dfs$train_df,
#'                          vali_df = pc_dfs$vali_df,
#'                          test_df = pc_dfs$test_df,
#'                          class_col = "Species",
#'                          ctrl = ctrl,
#'                          task = "multi",
#'                          methods = c("lda", "mda",
#'                          "nnet", "pda", "sparseLDA"),
#'                          tune_length = 5,
#'                          quiet = FALSE)
#' ## Train the ensemble.
#' ## Note: Increasing n_cores will dramatically reduce train time.
#' ensemble_model <- ph_ensemble(train_models = train_models$train_models,
#'                               train_df = pc_dfs$train_df,
#'                               vali_df = pc_dfs$vali_df,
#'                               test_df = pc_dfs$test_df,
#'                               class_col = "Species",
#'                               ctrl = ctrl,
#'                               task = "multi",
#'                               top_models = 3,
#'                               metalearner = "glmnet",
#'                               tune_length = 25,
#'                               quiet = FALSE)
#' }
ph_ensemble <- function(train_models, train_df, vali_df, test_df, class_col,
                        ctrl, train_seed = 123, n_cores = 2, task = "multi",
                        metric = ifelse(task == "multi", "Kappa", "ROC"),
                        top_models = 3, metalearner = ifelse(task == "multi",
                                                             "glmnet", "rf"),
                        tune_length = 10, quiet = FALSE)
{
    # Ensure that class is the first column of each data frame.
    if (!is.character(class_col)) { class_col <- as.character(class_col) }
    if (class_col %in% colnames(train_df) != TRUE |
        class_col %in% colnames(vali_df) != TRUE |
        class_col %in% colnames(test_df) != TRUE)
        stop(paste("Either the class column name is not in one or more of the",
                   "data frames or it is inconsistent between data frames."))
    train_df <- train_df[, c(which(colnames(train_df) == class_col),
                             which(colnames(train_df) != class_col))]
    vali_df <- vali_df[, c(which(colnames(vali_df) == class_col),
                           which(colnames(vali_df) != class_col))]
    test_df <- test_df[, c(which(colnames(test_df) == class_col),
                           which(colnames(test_df) != class_col))]
    colnames(train_df)[which(colnames(train_df) == class_col)] <- "class"
    colnames(vali_df)[which(colnames(vali_df) == class_col)] <- "class"
    colnames(test_df)[which(colnames(test_df) == class_col)] <- "class"
    if (all.equal(levels(train_df$class), levels(vali_df$class)) != TRUE |
        all.equal(levels(train_df$class), levels(test_df$class)) != TRUE |
        all.equal(levels(vali_df$class), levels(test_df$class)) != TRUE)
        stop("The class levels are inconsistent between data frames.")
    if (ncol(train_df) != ncol(vali_df) |
        ncol(train_df) != ncol(test_df) |
        ncol(vali_df) != ncol(test_df))
        stop(paste("The number of predictors (columns) are inconsistent",
                   "between data frames."))
    if (!is.numeric(n_cores))
        stop("The number of cores must be numeric (an integer).")
    if (!(task %in% c("multi", "binary")))
        stop("The classification task does not exist.")
    if (length(train_models) < 2)
        stop("At least two train models are needed for the ensemble.")
    if (!(metric %in% c("logLoss", "Accuracy", "Mean_Balanced_Accuracy",
                        "Mean_F1", "Kappa", "ROC")))
        stop("The metric does not exist.")
    if (tune_length < 1)
        stop("Tune length must be 1 or higher.")
    if (!is.numeric(top_models))
        stop(paste("The number of models for the ensemble must be numeric",
                   "(an integer)."))
    # Start cluster.
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    train_x <- train_df[, -which(names(train_df) %in% c("class"))]
    vali_x <- vali_df[, -which(names(vali_df) %in% c("class"))]
    test_x <- test_df[, -which(names(test_df) %in% c("class"))]
    train_df$class <- as.factor(train_df$class)
    vali_df$class <- as.factor(vali_df$class)
    test_df$class <- as.factor(test_df$class)
    # Check levels.
    if (length(levels(train_df$class)) > 2 & task == "binary")
        stop("The task and number of class levels do not match.")
    # Verify that factor levels in test and validation datasets
    # have > 2 observations.
    vali_levels <- as.data.frame(table(vali_df$class))
    test_levels <- as.data.frame(table(test_df$class))
    vali_check <- which(vali_levels$Freq < 2)
    test_check <- which(test_levels$Freq < 2)
    if (length(vali_check) > 0)
        stop(paste0(vali_levels$Var1[vali_check],
                    " needs at least two observations in the validation",
                    " set.", sep = ""))
    if (length(test_check) > 0)
        stop(paste0(test_levels$Var1[test_check],
                    " needs at least two observations in the test set.",
                    sep = ""))
    if (!is.list(ctrl))
        stop("Control object must be a list.")
    # Evaluate every initial model.
    iter_a <- 0
    all_test_preds <- list()
    for (i in train_models) {
        iter_a <- iter_a + 1
        all_test_preds[[iter_a]] <- tryCatch(stats::predict(i, test_df),
                                             error = function(e) NA)
    }
    names(all_test_preds) <- names(train_models)
    all_test_preds <- as.data.frame(dplyr::bind_cols(all_test_preds))
    # Find models that failed to predict.
    fail_models <- unname(which(sapply(all_test_preds, anyNA)))
    if (length(fail_models) > 0) {
        train_models <- train_models[-c(fail_models)]
        all_test_preds <- all_test_preds[, -c(fail_models)]
    }
    all_test_preds <- ph_equate(df = all_test_preds, class = test_df$class)
    all_test_cm <- list()
    if (quiet != TRUE) {
        message("Generating test predictions for all models.")
    }
    for (i in 1:ncol(all_test_preds)) {
        all_test_results <- ph_eval(pred = all_test_preds[, i],
                                    obs = test_df$class)
        all_test_cm[[i]] <- all_test_results
        i <- i + 1
    }
    names(all_test_cm) <- names(train_models)
    all_test_results <- do.call("rbind", all_test_cm)
    rownames(all_test_results) <- names(all_test_cm)
    # Rank the models.
    rvalues <- caret::resamples(train_models)
    rvalues_sum <- summary(rvalues)
    acc_sum <- as.data.frame(rvalues_sum[[3]][metric])
    # Order methods by mean (4th col).
    if (quiet != TRUE) { message("Ranking train models.") }
    acc_sum <- acc_sum[order(-acc_sum[,4]), ]
    if (top_models > nrow(acc_sum)) {
        top_models <- nrow(acc_sum)
        warning(paste0("The number of top models is greater than the number",
                       " of potential models. Setting top_models to ",
                       top_models, "."))
    }
    top_methods <- rownames(acc_sum)[1:top_models]
    # Match acc_methods with method_list.
    top_acc_match <- match(top_methods, names(train_models))
    train_models <- train_models[c(top_acc_match)]
    # Get variable importances from these models.
    var_imps <- matrix(nrow = ncol(train_df[, -1]),
                       ncol = length(top_acc_match))
    colnames(var_imps) <- names(train_models)
    rownames(var_imps) <- colnames(train_df[, -1])
    iter_a <- 0
    for (i in train_models) {
        try({
        iter_a <- iter_a + 1
        var_imp <- caret::varImp(i)
        var_imp_match <- match(rownames(var_imp$importance),
                               rownames(var_imps))
        var_imps[var_imp_match, iter_a] <- var_imp$importance[, 1]
        }, silent = TRUE)
    }
    # Convert NAs to 0s. Some models do not support variable importance.
    var_imps[is.na(var_imps)] <- 0
    # Evaluate models on validation and test sets.
    if (quiet != TRUE) {
        message("Generating validation and test predictions for ensemble model.")
    }
    # Get validation and test predictions.
    vali_preds <- stats::predict(train_models, vali_df)
    test_preds <- stats::predict(train_models, test_df)
    vali_preds <- as.data.frame(dplyr::bind_cols(vali_preds))
    test_preds <- as.data.frame(dplyr::bind_cols(test_preds))
    colnames(vali_preds) <- names(train_models)
    colnames(test_preds) <- names(train_models)
    # Concatenate original validation and test class with preds.
    vali_preds <- data.frame(class = vali_df$class, vali_preds,
                             stringsAsFactors = F)
    test_preds <- data.frame(class = test_df$class, test_preds,
                             stringsAsFactors = F)
    # Make sure factor levels are the same across data frame.
    vali_preds <- ph_equate(df = vali_preds, class = vali_df$class)
    test_preds <- ph_equate(df = test_preds, class = test_df$class)
    # Variable importances for the ensemble.
    ensemble_imps <- rowMeans(caret::filterVarImp(vali_preds[, -1],
                                                  vali_preds[, 1]))
    var_imps <- as.data.frame(rowMeans(t(t(var_imps) * ensemble_imps)))
    colnames(var_imps)[1] <- "Importance"
    # Ensemble model.
    if (quiet != TRUE) { message("Working on ensemble model.") }
    set.seed(train_seed)
    ensemble_model <- caret::train(class~.,
                                   data = vali_preds,
                                   metric = metric,
                                   method = metalearner,
                                   allowParallel = TRUE,
                                   trControl = ctrl,
                                   tuneLength = tune_length)
    if (quiet != TRUE) { message("Training complete.") }
    ensemble_test_preds <- stats::predict(ensemble_model, test_preds)
    ensemble_test_results <- ph_eval(pred = ensemble_test_preds,
                                     obs = test_df$class)
    rownames(ensemble_test_results) <- paste0("top_", top_models, "_ensemble")
    # Combine individual model test results and ensemble test results, then
    # sort by F1.
    all_test_results <- rbind.data.frame(all_test_results,
                                         ensemble_test_results)
    all_test_results <- all_test_results[order(all_test_results$F1,
                                               decreasing = TRUE), ]
    colnames(all_test_results)[1] <- "Method"
    # Turn off cluster.
    on.exit(parallel::stopCluster(cl))
    # Output vars.
    list(ensemble_test_preds = ensemble_test_preds, vali_preds = vali_preds,
         test_preds = test_preds, all_test_preds = all_test_preds,
         all_test_results = all_test_results, ensemble_model = ensemble_model,
         var_imps = var_imps, train_df = train_df, vali_df = vali_df,
         test_df = test_df, train_models = train_models, ctrl = ctrl,
         metric = metric, task = task, tune_length = tune_length,
         top_models = top_models, metalearner = metalearner)
}
