#' Evaluate a phenotype classification model.
#'
#' The \code{ph_eval} function generates a confusion matrix for binary or multi-class classification; for the multi-class case, the results are averaged across all class levels.
#'
#' @param pred A \code{factor} value of predicted classes.
#' @param obs A \code{factor} value of the observed or actual classes.
#' @returns A \code{data.frame} of confusion matrix evaluation results; for the multi-class case, the results are averaged across all class levels.
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
#' ## Train a few models for ensemble, although more is preferable.
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
#' ## Evaluate e.g. the first model.
#' test_pred <- predict(train_models$train_models[[1]], pc_dfs$test_df)
#' test_obs <- as.factor(pc_dfs$test_df$Species)
#' test_cm <- ph_eval(pred = test_pred, obs = test_obs)
#' }
ph_eval <- function(pred, obs)
{
    if (!is.factor(obs)) { obs <- as.factor(obs) }
    task <- ifelse(length(levels(obs)) > 2, "multi", "binary")
    if (task == "multi") {
        # Confusion matrix. Take the mean for a given class if the model
        # returns NA.
        cm <- caret::confusionMatrix(pred, obs)
        for (i in 1:ncol(cm$byClass)) {
            cm$byClass[is.na(cm$byClass[,i]), i] <- mean(cm$byClass[,i],
                                                         na.rm = TRUE)
        }
        # Means and 95% ci. Just retrieve means for now.
        cm_means <- suppressWarnings(
                        as.data.frame(apply(as.matrix(cm$byClass), 2,
                                            function(x) gmodels::ci(x)))
                    )
        cm_means <- cm_means[1, ]
        # Accuracy from overall.
        acc <- unname(cm$overall[1])
        # Kappa from overall.
        kappa <- unname(cm$overall[2])
        cm <- cbind.data.frame(cm_means, acc, kappa)
        colnames(cm)[12:13] <- c("Accuracy", "Kappa")
    } else {
        # Confusion matrix.
        cm <- caret::confusionMatrix(pred, obs)
        cm_means <- as.data.frame(t(cm$byClass))
        cm_overall <- as.data.frame(t(cm$overall))
        cm <- cbind.data.frame(cm_means, cm_overall)
    }
    return(cm)
}
