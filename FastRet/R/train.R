# Main #####

#' @title Train a new FastRet model (FRM) for retention time prediction
#' @description Trains a new model from molecule SMILES to predict retention times (RT) using the specified method.
#' @param df A dataframe with columns "NAME", "RT", "SMILES" and optionally a set of chemical descriptors. If no chemical descriptors are provided, they are calculated using the function [preprocess_data()].
#' @param method A string representing the prediction algorithm. Either "lasso", "ridge" or "gbtree".
#' @param verbose A logical value indicating whether to print progress messages.
#' @param nfolds An integer representing the number of folds for cross validation.
#' @param nw An integer representing the number of workers for parallel processing.
#' @param degree_polynomial An integer representing the degree of the polynomial. Polynomials up to the specified degree are included in the model.
#' @param interaction_terms A logical value indicating whether to include interaction terms in the model.
#' @param rm_near_zero_var A logical value indicating whether to remove near zero variance predictors. Setting this to TRUE can cause the CV results to be overoptimistic, as the variance filtering is done on the whole dataset, i.e. information from the test folds is used for feature selection.
#' @param rm_na A logical value indicating whether to remove NA values. Setting this to TRUE can cause the CV results to be overoptimistic, as the variance filtering is done on the whole dataset, i.e. information from the test folds is used for feature selection.
#' @param rm_ns A logical value indicating whether to remove chemical descriptors that were considered as not suitable for linear regression based on previous analysis of an independent dataset. See [check_lm_suitability()] for details on the analysis.
#' @param seed An integer value to set the seed for random number generation to allow for reproducible results.
#' @details Setting `rm_near_zero_var` and/or `rm_na` to TRUE can cause the CV results to be overoptimistic, as the predictor filtering is done on the whole dataset, i.e. information from the test folds is used for feature selection.
#' @return A trained FastRet model.
#' @keywords public
#' @examples \donttest{
#' system.time(m <- train_frm(RP[1:80, ], method = "lasso", nfolds = 2, nw = 1, verbose = 0))
#' # For the sake of a short runtime, only the first 80 rows of the RP dataset
#' # are used in this example. In practice, you should always use the entire
#' # training dataset for model training.
#' }
#' @export
train_frm <- function(df = read_rp_xlsx(),
                      method = "lasso",
                      verbose = 1,
                      nfolds = 5, # folds for cross validation
                      nw = 1, # nr workers for parallel processing, also passed to preprocess_data()
                      degree_polynomial = 1, # options for preprocess_data()
                      interaction_terms = FALSE, # options for preprocess_data()
                      rm_near_zero_var = TRUE, # options for preprocess_data()
                      rm_na = TRUE, # options for preprocess_data()
                      rm_ns = FALSE,
                      seed = NULL
                      ) {

    # Configure logging and check arguments
    if (verbose == 0) catf <- function(...) {}
    catf("Starting training of a %s model", method)
    method <- match.arg(method, c("lasso", "ridge", "gbtree"))
    if (!is.null(seed)) set.seed(seed)
    if (.Platform$OS.type == "windows" && nw > 1) {
        catf("Parallel processing is not supported on Windows. Setting `nw` to 1.")
        nw <- 1
    }

    # Return pregenerated results if mocking is enabled for this function
    if ("train_frm" %in% getOption("FastRet.mocks", c())) {
        mockfile <- sprintf("mockdata/%s_model.rds", method)
        catf("Mocking is enabled. Returning '%s'", mockfile)
        return(readRDS(pkg_file(mockfile)))
    }

    catf("Preprocessing data")
    folds <- caret::createFolds(y = df$RT, k = nfolds)
    df <- preprocess_data(df, degree_polynomial, interaction_terms, verbose, nw)

    nw <- max(min(nw, nfolds), 1)
    catf("Estimating model performance in cross validation using %d workers", nw)
    fit <- c(lasso = fit_lasso, ridge = fit_ridge, gbtree = fit_gbtree)[[method]]
    tmp <- mclapply(seq_along(folds), mc.cores = nw, function(i) {
        catf("Evaluating fold %d of %d", i, nfolds)
        train <- unname(unlist(folds[-i]))
        test <- folds[[i]]
        model <- fit(df[train, ], verbose = 0)
        stats <- get_stats(df = df[test, ], model = model)
        list(model, stats)
    })

    catf("Collecting results from cross validation")
    cv <- list(
        folds = folds,
        models = lapply(tmp, "[[", 1),
        stats = lapply(tmp, "[[", 2),
        preds = rep(NA, nrow(df))
    )
    for (i in seq_along(folds)) {
        X <- as.matrix(df[folds[[i]], colnames(df) %in% CDFeatures])
        preds <- predict(cv$models[[i]], X)
        cv$preds[folds[[i]]] <- preds
    }

    catf("Training final model on whole data set")
    model <- fit(df, verbose = 0)
    RT_pred <- stats::predict(model, as.matrix(df[, colnames(df) %in% CDFeatures]))

    catf("Finished model training. Returning frm object")
    frm <- list(model = model, df = df, cv = cv, seed = seed, version = packageVersion("FastRet"))
    frm <- structure(frm, class = "frm")
}

#' @title Adjust an existing FastRet model for use with a new column
#' @description The goal of this function is to train a model that predicts RT_ADJ (retention time measured on a new, adjusted column) from RT (retention time measured on the original column) and to attach this "adjustmodel" to an existing FastRet model.
#' @param frm An object of class `frm` as returned by [train_frm()].
#' @param new_data Dataframe with columns "RT", "NAME", "SMILES" and optionally a set of chemical descriptors.
#' @param predictors Numeric vector specifying which predictors to include in the model in addition to RT. Available options are: 1=RT, 2=RT^2, 3=RT^3, 4=log(RT), 5=exp(RT), 6=sqrt(RT).
#' @param nfolds An integer representing the number of folds for cross validation.
#' @param verbose A logical value indicating whether to print progress messages.
#' @keywords public
#' @examples \donttest{
#' frm <- read_rp_lasso_model_rds()
#' new_data <- read_rpadj_xlsx()
#' frmAdjusted <- adjust_frm(frm, new_data, verbose = 0)
#' }
#' @return An object of class `frm`, which is a list with the following elements:
#' * `model`: A list containing details about the original model.
#' * `df`: The data frame used for training the model.
#' * `cv`: A list containing the cross validation results.
#' * `seed`: The seed used for random number generation.
#' * `version`: The version of the FastRet package used to train the model.
#' * `adj`: A list containing details about the adjusted model.
#' @export
adjust_frm <- function(frm = train_frm(),
                       new_data = read_rpadj_xlsx(),
                       predictors = 1:6,
                       nfolds = 5,
                       verbose = 1) {
    if (!is.numeric(predictors) || !any(1:6 %in% predictors)) {
        stop("Invalid predictors. Please provide a vector of integers between 1 and 6.")
    }
    if (isFALSE(verbose) || verbose == 0) catf <- function(...) {}

    catf("Starting model Adjustment")
    catf("dim(original_data): %s", paste(dim(frm$df), collapse = " x "))
    catf("dim(new_data): %s", paste(dim(new_data), collapse = " x "))
    catf("predictors: %s", paste(predictors, collapse = ", "))
    catf("nfolds: %s", nfolds)

    catf("Preprocessing data")
    new <- data.frame(NAME = new_data$NAME, SMILES = new_data$SMILES, RT_ADJ = new_data$RT)
    old <- frm$df
    df <- merge(new, old) # Only keep rows where we have NAME and SMILES in both datasets
    pvec <- c("RT", "I(RT^2)", "I(RT^3)", "log(RT)", "exp(RT)", "sqrt(RT)")[predictors]
    pstr <- paste(pvec, collapse = " + ")
    fmstr <- paste("RT_ADJ ~", pstr)
    fm <- as.formula(fmstr)
    catf("Formula: %s", fmstr)
    cv <- list(
        folds = caret::createFolds(y = df$RT, k = nfolds),
        models = vector("list", nfolds),
        preds = rep(NA, nrow(df)),
        preds_adjonly = rep(NA, nrow(df))
    )

    catf("Estimating performance of adjusted model in CV")
    for (i in seq_along(cv$folds)) {
        train <- unname(unlist(cv$folds[-i]))
        cv$models[[i]] <- lm(formula = fm, data = df[train, ])
        # First create adjusted predictions using the true RT from the original column. This allows us to see how the adjustment model would perform if we could predict RT's for the original column with 100% accuracy.
        test <- cv$folds[[i]]
        testdf <- df[test, ]
        cv$preds_adjonly[test] <- predict(cv$models[[i]], testdf)
        # Now create adjusted predictions using the RT values predicted from the orginal model. This is the scenario we will encounter for future data as well.
        testdf$RT <- predict(frm, testdf, adjust = FALSE, verbose = 0)
        cv$preds[test] <- predict(cv$models[[i]], testdf)
    }

    catf("Fitting adjustment model on full new data set")
    model <- lm(formula = fm, data = df)

    catf("Returning adjusted frm object")
    frm$adj <- list(model = model, df = df, cv = cv)
    frm
}

#' @title Predict retention times using a FastRet Model
#' @description Predict retention times for new data using a FastRet Model (FRM).
#' @param object An object of class `frm` as returned by [train_frm()].
#' @param df A data.frame with the same columns as the training data.
#' @param adjust If `object` was adjusted using [adjust_frm()], it will contain a property `object$adj`. If `adjust` is TRUE, `object$adj` will be used to adjust predictions obtained from `object$model`. If FALSE `object$adj` will be ignored. If NULL, `object$model` will be used, if available.
#' @param verbose A logical value indicating whether to print progress messages.
#' @param ... Not used. Required to match the generic signature of `predict()`.
#' @return A numeric vector with the predicted retention times.
#' @keywords public
#' @seealso [train_frm()], [adjust_frm()]
#' @examples
#' frm <- read_rp_lasso_model_rds()
#' newdata <- head(RP)
#' yhat <- predict(frm, newdata)
#' @export
predict.frm <- function(object = train_frm(), df = object$df, adjust = NULL, verbose = 0, ...) {
    if (verbose == 0) catf <- function(...) {}
    predictors <- get_predictors(object)
    if (isTRUE(adjust) && is.null(object$adj)) {
        errmsg <- "Model has not been adjusted yet. Please adjust the model first using `adjust_frm()`."
        stop(errmsg)
    }
    if (sum(predictors %in% colnames(df)) == 0 && "SMILES" %in% colnames(df)) {
        catf("Chemical descriptors not found in newdata. Trying to calculate them from the provided SMILES.")
        cds <- getCDs(df, verbose = verbose) # 1 x 242 data.frame
        df <- cbind(df, cds)
    }
    if (!all(predictors %in% colnames(df))) {
        missing <- paste(setdiff(predictors, colnames(df)), collapse = ", ")
        errmsg <- paste("The following predictors are missing in `df`: ", missing)
        stop(errmsg)
    }
    catf("Predicting retention times")
    yhat <- c(predict(object$model, as.matrix(df[, predictors])))
    catf("Predictions: %s", paste(round(yhat, 2), collapse = ", "))
    if (!is.null(object$adj) && (isTRUE(adjust) || is.null(adjust))) {
        catf("Adjusting predictions using the adjustment model")
        yhatadj <- predict(object$adj$model, data.frame(RT = yhat))
        catf("Adjusted predictions: %s", paste(round(yhatadj, 2), collapse = ", "))
        catf("Returning adjusted predictions")
        return(yhatadj)
    } else {
        catf("Returning (unadjusted) predictions")
        return(yhat)
    }
}

#' @title Extract predictor names from an 'frm' object
#' @description This function extracts the predictor names from an 'frm' object.
#' @param frm An object of class 'frm' from which to extract the predictor names.
#' @return A character vector with the predictor names.
#' @keywords internal
#' @examples
#' frm <- read_rp_lasso_model_rds()
#' get_predictors(frm)
#' @export
get_predictors <- function(frm = train_frm()) {
    m <- frm$model
    if (inherits(m, "glmnet")) rownames(m$beta) else m$feature_names
}

make_X_adj <- function(RT, predictors = 1:5) {
    preds <- data.frame(RT^2, RT^3, log(RT), exp(RT), sqrt(RT))
    colnames(preds) <- c("RT_SQUARED", "RT_CUBED", "RT_LOG", "RT_EXP", "RT_SQRT")
    preds <- preds[, predictors]
    cbind(RT, preds)
}

# Helpers #####

#' @description Get RMSE, Rsquared, MAE and %below1min for a specific dataset and model.
#' @param data dataframe with retention time in the first column
#' @param model object useable as input for [stats::predict()]
#' @noRd
get_stats <- function(df, model) {
    X <- as.matrix(df[, colnames(df) %in% CDFeatures])
    y <- df$RT
    yhat <- stats::predict(model, X, type = "response")
    measures <- c(
        RMSE = sqrt(mean((y - yhat)^2)),
        Rsquared = cor(y, yhat)^2,
        MAE = mean(abs(y - yhat)),
        pBelow1Min = sum(abs(y - yhat) < 1.0) / length(y)
    )
    round(measures, 2)
}

validate_inputdata <- function(df, require = c("RT", "SMILES", "NAME"), min_cds = 1) {
    missing_cols <- setdiff(require, colnames(df))
    if (length(missing_cols) > 0) stop(sprintf("missing columns: %s", paste(missing_cols, collapse = ", ")))
    n_cds <- sum(colnames(df) %in% CDFeatures)
    if (n_cds < min_cds) {
        msg <- sprintf("At least %d chemical descriptors are required, but only %d are present", min_cds, n_cds)
        stop(msg)
    }
    unnown_cols <- setdiff(colnames(df), c("RT", "SMILES", "NAME", CDFeatures))
    if (length(unnown_cols) > 0) {
        msg <- sprintf("Unknown columns present: %s", paste(unnown_cols, collapse = ", "))
        stop(msg)
    }
    invisible(df)
}

validate_inputmodel <- function(model) {
    model_nams <- names(model)
    expected_names <- c("model", "df", "cv")
    n_missing <- sum(!expected_names %in% model_nams)
    if (n_missing > 0) {
        if (n_missing < length(expected_names)) {
            missing <- paste(setdiff(expected_names, model_nams), collapse = ", ")
            errmsg1 <- sprintf("Model object is missing required elements: %s.", missing)
        } else {
            errmsg1 <- sprintf("Model object is invalid.")
        }
        errmsg2 <- sprintf("Please upload a model trained with FastRet version %s or greater.", packageVersion("FastRet"))
        errmsg <- paste(errmsg1, errmsg2)
        stop(errmsg)
    }
    invisible(model)
}

# GLMNET Helpers #####

#' @description Fits a lasso model using the function [glmnet::cv.glmnet()].
#' @param df Dataframe containing columns RT, SMILES, NAME and a set of chemical descriptors
#' @param alpha Elastic net mixing parameter (1 = lasso, 0 = ridge)
#' @param verbose Verbosity level
#' @noRd
fit_glmnet <- function(df = preprocess_data(), verbose = 1, alpha = 1) {
    cds <- colnames(df) %in% CDFeatures
    X <- as.matrix(df[, cds])
    Y <- as.matrix(df[, !cds])
    y <- df[, "RT"]
    if (verbose) catf("Fitting %s model", if (alpha == 1) "Lasso" else "Ridge")
    cvobj <- glmnet::cv.glmnet(X, y, alpha = alpha, standardize = TRUE, family = "gaussian", type.measure = "mse", )
    model <- glmnet::glmnet(X, y, alpha = alpha, standardize = TRUE, family = "gaussian", lambda = cvobj$lambda.min)
    # model$traindata <- list(X = X, Y = Y, y = y) # store traindata for later model analysis and/or potential data imputation into testdata
    if (verbose) catf("End training")
    return(model)
}

fit_lasso <- function(df = preprocess_data(), verbose = 1) {
    fit_glmnet(
        df = df,
        verbose = verbose,
        alpha = 1
    )
}

fit_ridge <- function(df = preprocess_data(), verbose = 1) {
    fit_glmnet(
        df = df,
        verbose = verbose,
        alpha = 0
    )
}

# GBTree Helpers #####

#' @description Finds the optimal number of nrounds for fitting a GBTree model in CV and then uses this number to fit a final model on the complete dataset.
#' @details The optimal value for the other hyperparameters eta, gamma, etc. was determined a priori using a grid search on the RP dataset ([read_rp_xlsx()]). To reproduce the grid search, see the example of function [plot_gbtree_performance()].
#' @param df Dataframe containing columns RT, SMILES, NAME and a set of chemical descriptors
#' @param verbose verbosity level (0 = silent, 1 = progress, 2 = more details)
#' @param nw number of workers to use for parallel processing, ignored on Windows
#' @return A object as returned by `caret::train`
#' @noRd
fit_gbtree <- function(df = preprocess_data(), verbose = 1) {
    if (verbose) catf("Fitting GBTree model")
    obj <- fit_gbtree_grid(
        df = df,
        verbose = 0,
        nrounds = 2000,
        eta = 0.05,
        gamma = 0, # xgboost default
        max_depth = 4, # xgboost default
        min_child_weight = 4, # xgboost default
        subsample = 0.5,
        colsample_bytree = 1, # xgboost default
        n_threads = 1,
        nw = 1
    )
    if (verbose) catf("End training")
    obj$model
}

#' @description Fits multiple GBTree (Gradiant Boosted Tree) models, evaluates their performance in cross validation (CV) and then fits a final model using the optimal set of parameters.
#' @param df Dataframe containing columns RT, SMILES, NAME and a set of chemical descriptors
#' @param verbose verbosity level (0 = silent, 1 = progress, 2 = more details)
#' @param nrounds maximium number of boosting rounds
#' @param eta learning rate
#' @param gamma min loss reduction to allow further partition
#' @param max_depth max depth of a tree
#' @param min_child_weight minimum number of instances needed to be in each node
#' @param subsample subsample ratio of the training instance
#' @param colsample_bytree subsample ratio of columns when constructing each tree
#' @param n_threads threads used internally by xgboost
#' @param nw number of workers to use for parallel processing, ignored on Windows
#' @return A object as returned by `caret::train`
#' @noRd
fit_gbtree_grid <- function(df = preprocess_data(),
                            verbose = 1, # verbose level (0 = silent, 1 = progress, 2 = more details)
                            nrounds = 1000, # maximium number of boosting rounds
                            eta = c(1, 2, 5, 10, 20, 30, 40) / 100, # learning rate
                            gamma = 0:3, # min loss reduction to allow further partition
                            max_depth = 1:6, # max depth of a tree
                            min_child_weight = 1:5, # minimum number of instances needed to be in each node
                            subsample = (4:10) / 10, # subsample ratio of the training instance
                            colsample_bytree = (8:10) / 10, # subsample ratio of columns when constructing each tree
                            n_threads = 1, # threads used internally by xgboost
                            nw = 1, # number of workers to use for parallel processing, ignored on Windows
                            nfolds = 10,
                            seed = NULL) {
    a <- Sys.time()
    validate_inputdata(df, require = c("RT"), min_cds = 1)
    if (verbose == 0) catf <- function(...) {}
    if (!is.null(seed)) set.seed(seed)
    if (.Platform$OS.type == "windows" && nw > 1) {
        catf("Parallel processing is not supported on Windows. Setting `nw` to 1.")
        nw <- 1
    }

    catf("Preparing data for GBTree model fitting")
    y <- df$RT
    X <- as.matrix(df[, colnames(df) %in% CDFeatures])
    data <- xgboost::xgb.DMatrix(X, label = y, nthread = 1)
    foldids <- caret::createFolds(seq_len(nrow(df)), k = nfolds)
    param_grid <- base::expand.grid(max_depth = max_depth, eta = eta, gamma = gamma, colsample_bytree = colsample_bytree, subsample = subsample, min_child_weight = min_child_weight, nthread = n_threads)
    nparams <- nrow(param_grid)
    nw <- min(nw, nparams)

    catf(sprintf("Evaluating %d paramater combinations in %d-fold cross validation using %d workers", nparams, nfolds, nw))
    cv_results_tmp <- mclapply(mc.cores = min(nw, nparams), seq_len(nparams), function(i) {
        catf(sprintf("Evaluating parameter set %d/%d", i, nparams))
        params <- c(param_grid[i, ])
        cv_obj <- xgboost::xgb.cv(params = params, data = data, nrounds = nrounds, folds = foldids, early_stopping_rounds = 20, objective = "reg:squarederror", verbose = (if (verbose == 2) TRUE else FALSE))
        nround_best <- cv_obj$best_iteration
        rmse_best <- cv_obj$evaluation_log$test_rmse_mean[nround_best]
        rmse_std <- cv_obj$evaluation_log$test_rmse_std[nround_best]
        cv_result <- c(nround_best = nround_best, rmse_best = rmse_best, rmse_std = rmse_std)
        cv_result
    })
    cv_results <- data.frame(do.call(rbind, cv_results_tmp))

    catf("Finding optimal parameters from CV and training final model")
    best_index <- which.min(cv_results$rmse_best)
    best_params <- unlist(param_grid[best_index, ])
    best_rmse <- cv_results$rmse_best[best_index]
    catf("Best RMSE: %s", best_rmse)
    catf("Best parameters: %s", paste(names(best_params), best_params, sep = "=", collapse = ", "))
    model <- xgboost::xgb.train(params = as.list(c(best_params)), data = data, nrounds = cv_results$nround_best[best_index], verbose = if (verbose == 2) 1 else 0)

    b <- Sys.time()
    catf("Finished model fitting in %s", format(b - a))

    return(list(model = model, cv_results = cv_results, param_grid = param_grid))
}

#' @description Plot cross validation results of a GBTree model.
#' @param x Object as returned by [fit_gbtree_grid()]
#' @param print Print the plots to the console?
#' @param pdfpath Path to save the plots as PDF
#' @examples
#' \dontrun{
#' df <- preprocess_data(nw = 2)
#' x <- fit_gbtree_grid(df, nw = 64)
#' plot_gbtree_performance(x, pdfpath = "misc/cvgbtree.pdf")
#' plot_gbtree_performance(x, pdfpath = "misc/cvgbtree_box.pdf", type = "box")
#' }
#' @noRd
plot_gbtree_performance <- function(x = fit_gbtree_grid(),
                                    print = TRUE,
                                    pdfpath = NULL,
                                    type = c("box", "violin")[2]) {
    param_grid <- x$param_grid
    cv_results <- x$cv_results
    ggdf <- cbind(param_grid, cv_results)
    cols <- c("max_depth", "eta", "gamma", "colsample_bytree", "subsample", "min_child_weight")
    ggdf[, cols] <- lapply(ggdf[, cols], as.factor)
    plots <- lapply(cols, function(col) {
        ggplot(ggdf, aes_string(x = col, y = "rmse_best")) +
            if (type == "violin") geom_violin(trim = FALSE) else geom_boxplot()
    })
    if (!is.null(pdfpath)) {
        pdf(pdfpath, width = 7, height = 3.5)
        for (plot in plots) print(plot)
        dev.off()
    }
    if (print) for (p in plots) print(p)
    invisible(plots)
}
