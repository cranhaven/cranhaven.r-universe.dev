#' Generate predictions for phenotype ensemble.
#'
#' The \code{ph_train} function automatically trains a set of binary or multi-class classification models to ultimately
#' build a new dataset of predictions. The data preprocessing and hyperparameter tuning are handled internally to
#' minimize user input and simplify the training.
#'
#' @param train_df A \code{data.frame} containing a class column and the training data.
#' @param vali_df A \code{data.frame} containing a class column and the validation data.
#' @param test_df A \code{data.frame} containing a class column and the test d
#' @param class_col A \code{character} value for the name of the class column shared across the train, validation, and test sets.
#' @param ctrl A \code{list} containing the resampling strategy (e.g., "boot") and other parameters for \code{trainControl}. Automatically create one via \code{ph_ctrl} or manually create it with \code{trainControl}.
#' @param train_seed A \code{numeric} value to set the training seed and control the randomness of creating resamples: 123 (default).
#' @param n_cores An \code{integer} value for the number of cores to include in the cluster: 2 (default). We highly recommend increasing this value to, e.g., parallel::detectCores() - 1.
#' @param task A \code{character} value for the type of classification \code{task}: "multi" (default), "binary".
#' @param methods A \code{character} value enumerating the names (at least two, unless "all") of the classification methods to ensemble: "all" (default).
#' \itemize{
#'   \item If \code{task = "binary"}, there are 33 methods to choose from: "AdaBag", "AdaBoost.M1", "C5.0", "evtree", "glmnet", "hda", "kernelpls", "kknn", "lda", "loclda", "mda", "nb", "nnet", "pda", "pls", "qda", "rda", "rf", "sparseLDA", "stepLDA", "stepQDA", "treebag", "svmLinear", "svmPoly","svmRadial", "gaussprLinear" (slow), "gaussprPoly" (slow), "gaussprRadial" (slow), "bagEarthGCV", "cforest", "earth", "fda", "hdda".
#'   \item If \code{task = "multi"}, there are 30 methods to choose from:  "AdaBag", "AdaBoost.M1",  "C5.0", "evtree", "glmnet", "hda", "kernelpls", "kknn", "lda", "loclda", "mda", "nb", "nnet", "pda", "pls", "qda", "rda", "rf", "sparseLDA", "stepLDA", "stepQDA", "treebag", "svmLinear", "svmPoly", "svmRadial", "bagEarthGCV", "cforest", "earth", "fda", "hdda".
#' }
#' @param metric A \code{character} value for which summary metric should be used to select the optimal model: "ROC" (default for "binary") and "Kappa" (default for "multi"). Other options include "logLoss", "Accuracy", "Mean_Balanced_Accuracy", and "Mean_F1".
#' @param tune_length If \code{search = "random"} (default), this is an \code{integer} value for the maximum number of hyperparameter combinations to test for each training model in the ensemble; if \code{search = "grid"}, this is an \code{integer} value for the number of levels of each hyperparameter to test for each model.
#' @param quiet A \code{logical} value for whether progress should be printed: TRUE (default), FALSE.
#' @return A list containing the following components:\tabular{ll}{
#'    \code{train_models} \tab The \code{train} models for the ensemble. \cr
#'    \tab \cr
#'    \code{train_df} \tab The training data frame. \cr
#'    \tab \cr
#'    \code{vali_df} \tab The validation data frame. \cr
#'    \tab \cr
#'    \code{test_df} \tab The test data frame. \cr
#'    \tab \cr
#'    \code{task} \tab The type of classification task. \cr
#'    \tab \cr
#'    \code{ctrl} \tab A list of resampling parameters used in \code{trainControl}. \cr
#'    \tab \cr
#'    \code{methods} \tab The names of the classification methods to ensemble. \cr
#'    \tab \cr
#'    \code{search} \tab The hyperparameter search strategy. \cr
#'    \tab \cr
#'    \code{n_cores} \tab The number of cores for parallel processing. \cr
#'    \tab \cr
#'    \code{metric} \tab The summary metric used to select the optimal model. \cr
#'    \tab \cr
#'    \code{tune_length} \tab The maximum number of hyperparameter combinations ("random") or individual hyperparameter depth ("grid").  \cr
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
#' }
ph_train <- function(train_df, vali_df, test_df, class_col, ctrl,
                     train_seed = 123, n_cores = 2, task = "multi",
                     methods = "all", metric = ifelse(task == "multi",
                                                      "Kappa", "ROC"),
                     tune_length = 10, quiet = FALSE)
{
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
    if (!is.numeric(train_seed))
        stop("Seed must be numeric (an integer).")
    if (!is.numeric(n_cores))
        stop("Number of cores must be numeric (an integer).")
    if (!(task %in% c("multi", "binary")))
        stop("Classification task does not exist.")
    if (all(methods %in% c("AdaBag", "AdaBoost.M1", "bagEarthGCV", "C5.0",
                           "cforest", "earth", "evtree", "fda",
                           "gaussprLinear", "gaussprPoly", "gaussprRadial",
                           "glmnet", "hda", "hdda", "kernelpls", "kknn",
                           "lda", "loclda", "mda", "nb", "nnet", "pda", "pls",
                           "qda", "rda", "rf", "sparseLDA", "stepLDA",
                           "stepQDA", "svmLinear", "svmPoly", "svmRadial",
                           "treebag", "all")) != TRUE)
        stop(paste("At least one classification method has been entered",
                   "incorrectly or is not available."))
    if (length(methods) > 1) {
        if ("all" %in% methods != FALSE)
           stop("The method \"all\" cannot be added to other methods.")
    }
    if (!(metric %in% c("logLoss", "Accuracy", "Mean_Balanced_Accuracy",
                        "Mean_F1", "Kappa", "ROC")))
        stop("Metric does not exist.")
    if (tune_length < 1)
        stop("Tune length must be 1 or higher.")
    # Start cluster.
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    # Prepare data.
    train_x <- train_df[, -which(names(train_df) %in% c("class"))]
    vali_x <- vali_df[, -which(names(vali_df) %in% c("class"))]
    test_x <- test_df[, -which(names(test_df) %in% c("class"))]
    train_df$class <- as.factor(train_df$class)
    vali_df$class <- as.factor(vali_df$class)
    test_df$class <- as.factor(test_df$class)
    if (length(levels(train_df$class)) > 2 & task == "binary")
        stop("The task and number of class levels do not match.")
    # Verify that factor levels in test and validation datasets have > 2 observations.
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
    # For sink().
    if (.Platform$OS.type == "unix") {
        tmp <- "/dev/null"
    } else {
        tmp <- "NUL"
    }
    sink(tmp)
    # Initialize classification loop.
    train_models <- list()
    if (task == "binary") {
        par_methods <- c("AdaBag", "AdaBoost.M1", "C5.0", "evtree", "glmnet",
                         "hda", "kernelpls", "kknn", "lda", "loclda", "mda",
                         "nb", "nnet", "pda", "pls", "qda", "rda", "rf",
                         "sparseLDA", "stepLDA", "stepQDA", "treebag")
        formula_methods <- c("svmLinear", "svmPoly","svmRadial",
                             "gaussprLinear", "gaussprPoly", "gaussprRadial")
        nopar_methods <- c("bagEarthGCV", "cforest", "earth", "fda", "hdda")
        if ("all" %in% methods) {
            par_methods <- par_methods
            formula_methods <- formula_methods
            nopar_methods <- nopar_methods
        } else {
            if (all(methods %in% c(par_methods,
                                   formula_methods,
                                   nopar_methods)) != TRUE)
                stop(paste("At least one classification method has been",
                           "entered incorrectly or is not available for",
                           "binary classification."))
            if (length(methods) < 2)
                stop("At least two methods are required for the ensemble.")
            par_methods <- intersect(methods, par_methods)
            formula_methods <- intersect(methods, formula_methods)
            nopar_methods <- intersect(methods, nopar_methods)
        }
        iter_a <- 0
        if (length(par_methods) > 0) {
            for (i in par_methods) {
                iter_a <- iter_a + 1
                if (quiet != TRUE) { message(paste0("Working on ",
                                                    i, " model.")) }
                set.seed(train_seed)
                train_models[[iter_a]] <- try(caret::train(x = train_x,
                                                           y = train_df$class,
                                                           metric = metric,
                                                           method = i,
                                                           allowParallel = TRUE,
                                                           trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_a]], "try-error")) { next }
                names(train_models)[iter_a] <- i
            }
        }
        iter_b <- length(par_methods)
        if (length(formula_methods) > 0) {
            for (i in formula_methods) {
                iter_b <- iter_b + 1
                if (quiet != TRUE) { message(paste0("Working on ",
                                                    i, " model.")) }
                set.seed(train_seed)
                train_models[[iter_b]] <- try(caret::train(class~.,
                                                           data = train_df,
                                                           metric = metric,
                                                           method = i,
                                                           allowParallel = TRUE,
                                                           trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_b]], "try-error")) { next }
                names(train_models)[iter_b] <- i
            }
        }
        iter_c <- length(par_methods) + length(formula_methods)
        if (length(nopar_methods) > 0) {
            for (i in nopar_methods) {
                iter_c <- iter_c + 1
                if (quiet != TRUE) { message(paste0("Working on ",
                                                    i, " model.")) }
                set.seed(train_seed)
                train_models[[iter_c]] <- try(caret::train(x = train_x,
                                                           y = train_df$class,
                                                           metric = metric,
                                                           method = i,
                                                           trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_c]], "try-error")) { next }
                names(train_models)[iter_c] <- i
            }
        }
        if (quiet != TRUE) { message("Training complete.") }
    } else {
        par_methods <- c("AdaBag", "AdaBoost.M1",  "C5.0", "evtree", "glmnet",
                         "hda", "kernelpls", "kknn", "lda", "loclda", "mda",
                         "nb", "nnet", "pda", "pls", "qda", "rda", "rf",
                         "sparseLDA","stepLDA","stepQDA", "treebag")
        formula_methods <- c("svmLinear", "svmPoly", "svmRadial")
        nopar_methods <- c("bagEarthGCV", "cforest", "earth", "fda", "hdda")
        if ("all" %in% methods) {
            par_methods <- par_methods
            formula_methods <- formula_methods
            nopar_methods <- nopar_methods
        } else {
            if (all(methods %in% c(par_methods,
                                   formula_methods,
                                   nopar_methods)) != TRUE)
                stop(paste("At least one classification method has been",
                           "entered incorrectly or is not available for",
                           "multi-class classification."))
        if (length(methods) < 2)
            stop("At least two methods are required for the ensemble.")
        par_methods <- intersect(methods, par_methods)
        formula_methods <- intersect(methods, formula_methods)
        nopar_methods <- intersect(methods, nopar_methods)
        }
        iter_a <- 0
        if (length(par_methods) > 0) {
            for (i in par_methods) {
                iter_a <- iter_a + 1
                if (quiet != TRUE) { message(paste0("Working on ",
                                                    i, " model.")) }
                set.seed(train_seed)
                train_models[[iter_a]] <- try(caret::train(x = train_x,
                                                           y = train_df$class,
                                                           metric = metric,
                                                           method = i,
                                                           allowParallel = TRUE,
                                                           trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_a]], "try-error")) { next }
                names(train_models)[iter_a] <- i
            }
        }
        iter_b <- length(par_methods)
        if (length(formula_methods) > 0) {
            for (i in formula_methods) {
                iter_b <- iter_b + 1
                if (quiet != TRUE) { message(paste0("Working on ",
                                                    i, " model.")) }
                set.seed(train_seed)
                train_models[[iter_b]] <- try(caret::train(class~.,
                                                           data = train_df,
                                                           metric = metric,
                                                           method = i,
                                                           allowParallel = TRUE,
                                                           trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_b]], "try-error")) { next }
                names(train_models)[iter_b] <- i
            }
        }
        iter_c <- length(par_methods) + length(formula_methods)
        if (length(nopar_methods) > 0) {
            for (i in nopar_methods) {
                iter_c <- iter_c + 1
                if (quiet != TRUE) { message(paste0("Working on ", i, " model.")) }
                set.seed(train_seed)
                train_models[[iter_c]] <- try(caret::train(x = train_x,
                                                           y = train_df$class,
                                                           metric = metric,
                                                           method = i,
                                                           trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_c]], "try-error")) { next }
                names(train_models)[iter_c] <- i
            }
        }
        if (quiet != TRUE) { message("Training complete.") }
    }
    train_fail <- which(lapply(train_models, length) == 1)
    if (length(train_fail) > 0) {
        train_models <- train_models[-c(train_fail)]
    }
    sink()
    # Turn off cluster.
    on.exit(parallel::stopCluster(cl))
    # Output vars.
    list(train_models = train_models, train_df = train_df, vali_df = vali_df,
         test_df = test_df, task = task, ctrl = ctrl, methods = methods,
         search = search, n_cores = n_cores, metric = metric,
         tune_length = tune_length)
}
