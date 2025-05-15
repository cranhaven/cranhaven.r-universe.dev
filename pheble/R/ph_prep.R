#' Preprocessing for phenotype classification via ensemble learning.
#'
#' The \code{ph_prep} function splits a data frame into training, validation, and test sets, all while ensuring that
#' every class is represented in each dataset. By default, it performs a Principal Component Analysis on the training
#' set data and projects the validation and test data into that space. If a non-linear dimensionality reduction
#' strategy is preferred instead, an autoencoder can be used to extract deep features. Note that the parameters
#' \code{max_mem_size}, \code{activation}, \code{hidden}, \code{dropout_ratio}, \code{rate}, \code{search}, and
#' \code{tune_length} are \code{NULL} unless an autoencoder, \code{method = "ae"}, is used. In this case,
#' lists or vectors can be supplied to these parameters (see parameter details) to perform a grid search for the
#' optimal hyperparameter combination. The autoencoder with the lowest reconstruction error is selected as
#' the best model.
#'
#' @param df A \code{data.frame} containing a column of unique ids, a column of classes, and an arbitrary number of \code{numeric} columns.
#' @param ids_col A \code{character} value for the name of the ids column.
#' @param class_col A \code{character} value for the name of the class column.
#' @param vali_pct A \code{numeric} value for the percentage of training data to use as validation data: 0.15 (default).
#' @param test_pct A \code{numeric} value for the percentage of total data to use as test data: 0.15 (default).
#' @param scale A \code{logical} value for whether to scale the data: FALSE (default). Recommended if \code{method = "ae"} and if user intends to train other models.
#' @param center Either a \code{logical} value or numeric-alike vector of length equal to the number of columns of data to scale in \code{df}, where ‘numeric-alike’ means that as.numeric(.) will be applied successfully if is.numeric(.) is not true: NULL (default). If \code{scale = TRUE}, this is set to \code{TRUE} and is used to subtract the mean.
#' @param sd Either a \code{logical} value or a numeric-alike vector of length equal to the number of columns of data to scale in \code{df}: NULL (default). If \code{scale = TRUE}, this is set to \code{TRUE} and is used to divide by the standard deviation.
#' @param split_seed A \code{numeric} value to set the seed and control the randomness of splitting the data: 123 (default).
#' @param method A \code{character} value for the dimensionality reduction method: "pca" (default), "ae", "none".
#' @param pca_pct If \code{method = "pca"}, a \code{numeric} value for the proportion of variance to subset the PCA with: 0.95 (default).
#' @param max_mem_size If \code{method = "ae"}, a \code{character} value for the memory of an h2o session: "15g" (default).
#' @param port A \code{numeric} value for the port number of the H2O server.
#' @param train_seed A \code{numeric} value to set the control the randomness of creating resamples: 123 (default).
#' @param hyper_params A \code{list} of hyperparameters to perform a grid search. the "default" list is: list(missing_values_handling = "Skip", activation = c("Rectifier", "Tanh"), hidden = list(5, 25, 50, 100, 250, 500, nrow(df_h2o)), input_dropout_ratio = c(0, 0.1, 0.2, 0.3), rate = c(0, 0.01, 0.005, 0.001)).
#' @param search If \code{method = "ae"}, a \code{character} value for the hyperparameter search strategy: "random" (default), "grid".
#' @param tune_length If \code{method = "ae"}, a \code{numeric} value (integer) for either the maximum number of hyperparameter combinations ("random") or individual hyperparameter depth ("grid").
#' @return A list containing the following components:\tabular{ll}{
#'    \code{train_df} \tab The training set data frame. \cr
#'    \tab \cr
#'    \code{vali_df} \tab The validation set data frame. \cr
#'    \tab \cr
#'    \code{test_df} \tab The test set data frame. \cr
#'    \tab \cr
#'    \code{train_split} \tab The training set indices from the original data frame. \cr
#'    \tab \cr
#'    \code{vali_split} \tab The validation set indices from the original data frame. \cr
#'    \tab \cr
#'    \code{test_split} \tab The test set indices from the original data frame. \cr
#'    \tab \cr
#'    \code{vali_pct} \tab The percentage of training data used as validation data. \cr
#'    \tab \cr
#'    \code{test_pct} \tab The percentage of total data used as test data. \cr
#'    \tab \cr
#'    \code{method} \tab The dimensionality reduction method. \cr
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
#' ## Alternatively, preprocess data frame into train, validation, and test
#' ## sets with latent variables as predictors. Notice that port is defined,
#' ## because running H2O sessions one after another can cause connection
#' ## errors.
#' ae_dfs <- ph_prep(df = rm_outs$df, ids_col = "Biosample", class_col = "Species",
#'                   vali_pct = 0.15, test_pct = 0.15, method = "ae", port = 50001)
#' }
ph_prep <- function(df, ids_col, class_col, vali_pct = 0.15, test_pct = 0.15,
                    scale = FALSE, center = NULL, sd = NULL, split_seed = 123,
                    method = "pca", pca_pct = 0.95, max_mem_size = "15g",
                    port = 54321, train_seed = 123, hyper_params = list(),
                    search = "random", tune_length = 100)
{
    output <- NULL
    df <- as.data.frame(df)
    if (any(is.na(df)) != FALSE) {
        df <- stats::na.omit(df)
        warning("The data frame contains NAs. Rows have been removed.")
    }
    if (!is.character(ids_col)) { ids_col <- as.character(ids_col) }
    if (!is.character(class_col)) { class_col <- as.character(class_col) }
    if (!(ids_col %in% colnames(df)))
        stop(paste("The ids column is either not in the data frame or is",
                   "differently named."))
    if (!(class_col %in% colnames(df)))
        stop(paste("The class column is either not in the data frame or is",
                   "differently named."))
    # Convert ids and class column names.
    colnames(df)[which(colnames(df) == ids_col)] <- "ids"
    colnames(df)[which(colnames(df) == class_col)] <- "class"
    if (any(duplicated(df$ids)) != FALSE)
        stop("This data frame contains duplicate ids. Ensure they are unique.")
    if (data.table::between(test_pct, 0, 1) != TRUE)
        stop(paste("The percentage of data for testing must be expressed as a",
                   "decimal between 0 and 1."))
    if (data.table::between(vali_pct, 0, 1) != TRUE)
        stop(paste("The percentage of data for validation must be expressed",
                   "as a decimal between 0 and 1."))
    if (scale == FALSE) {
        if (!is.null(center) | !is.null(sd))
            stop("Scale must be set to TRUE.")
    } else {
        if (!is.null(center)) {
            center <- center
        } else {
            center <- TRUE
        }
        if (!is.null(sd)) {
            sd <- sd
        } else {
            sd <- TRUE
        }
        meta_df <- df[, (names(df) %in% c("ids", "class"))]
        scale_df <- df[, !(names(df) %in% c("ids", "class"))]
        scale_df <- as.data.frame(scale(scale_df, center = center, scale = sd))
        df <- cbind.data.frame(meta_df, scale_df)
    }
    if (!is.numeric(split_seed))
        stop("Seed must be numeric (an integer).")
    if (!(method %in% c("pca", "ae", "none")))
        stop("Dimensionality reduction method does not exist.")
    # Split entire data frame with testing indices.
    set.seed(split_seed)
    test_split <- c(caret::createDataPartition(as.factor(df$class),
                                               times = 1,
                                               p = test_pct,
                                               list = F))
    test_df <- df[test_split, ]
    train_df <- df[-test_split, ]
    # Split training set with validation indices.
    vali_samp <- caret::createDataPartition(as.factor(train_df$class),
                                            p = vali_pct,
                                            list = F)
    vali_df <- train_df[vali_samp, ]
    train_seq <- seq(1, length(train_df$class), 1)
    train_samp <- setdiff(train_seq, vali_samp)
    train_split <- stats::na.omit(match(train_df$ids[train_samp], df$ids))
    vali_split <- stats::na.omit(match(train_df$ids[vali_samp], df$ids))
    # Make class column first column.
    df <- df[, c(which(colnames(df) == "class"),
                 which(colnames(df) != "class"))]
    rownames(df) <- df$ids
    # Get rid of ids column.
    df <- df[, -which(colnames(df) %in% "ids")]
    # Subset original data frame with splitting indices.
    train_df <- df[c(train_split), ]
    vali_df <- df[c(vali_split), ]
    test_df <- df[c(test_split), ]
    # PCA dimensionality reduction.
    if (method == "pca") {
        pca_pct <- pca_pct
        if (data.table::between(pca_pct, 0, 1) != TRUE)
            stop(paste("The proportion of variance to subset the PCA with",
                       "must be expressed as a decimal between 0 and 1."))
        pca_obj <- stats::prcomp(train_df[,-1])
        mu <- colMeans(train_df[,-1])
        pov <- pca_obj$sdev^2/sum(pca_obj$sdev^2)
        scores <- as.data.frame(pca_obj$x)
        count <- 0
        var_sum <- 0
        pc_vec <- c()
        # Loop through proportion of variance and add pc # to vector.
        while (var_sum < pca_pct) {
            count <- count + 1
            var_sum <- var_sum + pov[count]
            pc_vec[count] <- count
        }
        pc_vec <- sort(pc_vec)
        scores <- pca_obj$x[, c(pc_vec)]
        output <- list(scores = scores, pc_vec = pc_vec,
                       pca_obj = pca_obj, mu = mu)
        # Project data.
        train_pca <- scale(train_df[,-1], output$pca_obj$center,
                           output$pca_obj$scale) %*% output$pca_obj$rotation
        vali_pca <- scale(vali_df[,-1], output$pca_obj$center,
                          output$pca_obj$scale) %*% output$pca_obj$rotation
        test_pca <- scale(test_df[,-1], output$pca_obj$center,
                          output$pca_obj$scale) %*% output$pca_obj$rotation
        # Subset data by pc vector.
        train_x <- train_pca[, c(output$pc_vec)]
        vali_x <- vali_pca[, c(output$pc_vec)]
        test_x <- test_pca[, c(output$pc_vec)]
    # Autoencoder dimensionality reduction.
    } else if (method == "ae") {
        if (!is.character(max_mem_size))
            stop("Max memory size must be a character value.")
        if (!(search %in% c("random", "grid")))
            stop("Search strategy does not exist.")
        if (!is.numeric(tune_length))
            stop("Tune length must be numeric (an integer).")
        # Spawn h2o cluster.
        if (.Platform$OS.type == "unix") {
          tmp <- "/dev/null"
        } else {
          tmp <- "NUL"
        }
        # closeAllConnections()
        gc()
        sink(tmp)
        requireNamespace("h2o", quietly = TRUE)
        h2o::h2o.init(max_mem_size = max_mem_size, port = port)
        h2o::h2o.removeAll()
        train_h2o <- h2o::as.h2o(train_df)
        vali_h2o <- h2o::as.h2o(vali_df)
        test_h2o <- h2o::as.h2o(test_df)
        predictors <- setdiff(colnames(train_h2o), "class")
        # Redefine search strategy names; making them consistent with train function.
        if (search == "random") {
            search = "RandomDiscrete"
        } else {
            search = "Cartesian"
        }
        if (search == "RandomDiscrete") {
            search_criteria <- list(strategy = search,
                                    max_models = tune_length,
                                    stopping_metric = "AUTO",
                                    stopping_tolerance = 0.00001)
        } else {
            search_criteria <- list(strategy = search,
                                    stopping_metric = "AUTO",
                                    stopping_tolerance = 0.00001)
        }
        if (length(hyper_params) == 0) {
            hyper_params <- list(missing_values_handling = "Skip",
                                 activation = "Rectifier",
                                 hidden = list(5, 25, 50, 100, 250, 500,
                                               nrow(train_h2o)),
                                 input_dropout_ratio = c(0, 0.1, 0.2, 0.3),
                                 rate = c(0, 0.01, 0.005, 0.001))
        } else {
            hyper_params <- hyper_params
        }
        if (!is.list(hyper_params))
            stop("Hyperparameters must be supplied as a list.")
        # Train autoencoder.
        ae_grid <- suppressWarnings(
                       h2o::h2o.grid(algorithm = 'deeplearning',
                                     x = predictors,
                                     training_frame = train_h2o,
                                     grid_id = 'ae_grid',
                                     autoencoder = TRUE,
                                     hyper_params = hyper_params,
                                     search_criteria = search_criteria,
                                     sparse = FALSE,
                                     ignore_const_cols = FALSE,
                                     seed = train_seed)
                   )
        ae_grids <- suppressWarnings(
                        h2o::h2o.getGrid(grid_id = "ae_grid",
                                         sort_by = "mse",
                                         decreasing = FALSE)
                    )
        best_ae <- h2o::h2o.getModel(ae_grids@model_ids[[1]])
        # Extract deep features.
        train_x <- as.data.frame(h2o::h2o.deepfeatures(best_ae,
                                                       train_h2o,
                                                       layer=1))
        vali_x <- as.data.frame(h2o::h2o.deepfeatures(best_ae,
                                                      vali_h2o,
                                                      layer=1))
        test_x <- as.data.frame(h2o::h2o.deepfeatures(best_ae,
                                                      test_h2o,
                                                      layer=1))
        # Shutdown h2o cluster.
        h2o::h2o.shutdown(prompt = FALSE)
        sink()
        # closeAllConnections()
    } else {
        train_x <- train_df[,-1]
        vali_x <- vali_df[,-1]
        test_x <- test_df[,-1]
    }
    # Bind class with datasets.
    train_df <- cbind.data.frame(train_df$class, train_x)
    vali_df <- cbind.data.frame(vali_df$class, vali_x)
    test_df <- cbind.data.frame(test_df$class, test_x)
    colnames(train_df)[1] <- class_col
    colnames(vali_df)[1] <- class_col
    colnames(test_df)[1] <- class_col
    list(train_df = train_df, vali_df = vali_df, test_df = test_df,
         train_split = train_split, vali_split = vali_split,
         test_split = test_split, vali_pct = vali_pct,
         test_pct = test_pct, method = method)
}
