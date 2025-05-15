#' Compute interquartile range.
#'
#' The \code{ph_iqr} function computes the interquartile range.
#'
#' @param x A \code{numeric} vector.
#' @param na.rm A \code{logical} value: FALSE (default). If true, any NA is removed before quantiles are computed.
#' @param type An \code{integer} value (1:9) selecting one of 9 quantile algorithms.
#' @returns The interquartile range.
#' @export
ph_iqr <- function(x, na.rm = FALSE, type = 7)
{
    diff(stats::quantile(as.numeric(x), c(0.25, 0.75),
                         na.rm = na.rm, names = FALSE,
                         type = type))
}

#' Find outlier indices.
#'
#' The \code{ph_outs} function computes outliers with the interquartile method.
#'
#' @param x A \code{numeric} vector.
#' @returns The outlier indices.
#' @export
ph_outs <- function(x)
{
    q <- stats::quantile(x, probs=c(.25, .75), na.rm = FALSE)
    iqr <- ph_iqr(x)
    # Upper range.
    upper <-  q[2] + (1.5*iqr)
    # Lower range.
    lower <- q[1] - (1.5*iqr)
    # Return outlier indices.
    inds <- which(x > upper, arr.ind = TRUE)
    return(inds)
}

#' Detect anomalies.
#'
#' The \code{ph_anomaly} function detects and removes anomalies with an autoencoder. Because it is general
#' purpose, it can be applied to a variety of data types. The parameters in this function (e.g., activation,
#' hidden, dropout_ratio) can be supplied as lists or vectors (see parameter details) to perform a grid search
#' for the optimal hyperparameter combination. The autoencoder with the lowest reconstruction error is selected as
#' the best model.
#'
#' @param df A \code{data.frame} containing a column of ids, a column of classes, and an arbitrary number of predictors.
#' @param ids_col A \code{character} value for the name of the ids column.
#' @param class_col A \code{character} value for the name of the class column.
#' @param method A \code{character} value for the anomaly detection method: "ae" (default), "iso" (abbv. for extended isolation forest).
#' @param scale A \code{logical} value for whether to scale the data: FALSE (default). Recommended if \code{method = "ae"} and if user intends to train other models.
#' @param center Either a \code{logical} value or numeric-alike vector of length equal to the number of columns of data to scale in \code{df}, where ‘numeric-alike’ means that as.numeric(.) will be applied successfully if is.numeric(.) is not true: NULL (default). If \code{scale = TRUE}, this is set to \code{TRUE} and is used to subtract the mean.
#' @param sd Either a \code{logical} value or a numeric-alike vector of length equal to the number of columns of data to scale in \code{df}: NULL (default). If \code{scale = TRUE}, this is set to \code{TRUE} and is used to divide by the standard deviation.
#' @param max_mem_size A \code{character} value for the memory of an h2o session: "15g" (default).
#' @param port A \code{numeric} value for the port number of the H2O server.
#' @param train_seed A \code{numeric} value to set the control the randomness of creating resamples: 123 (default).
#' @param hyper_params A \code{list} of hyperparameters to perform a grid search.
#' \itemize{
#'   \item If \code{method = "ae"}, the "default" list is: list(missing_values_handling = "Skip", activation = c("Rectifier", "Tanh"), hidden = list(5, 25, 50, 100, 250, 500, nrow(df_h2o)), input_dropout_ratio = c(0, 0.1, 0.2, 0.3), rate = c(0, 0.01, 0.005, 0.001))
#'   \item If \code{method = "iso"}, the "default" list is: list(ntrees = c(50, 100, 150, 200), sample_size = c(64, 128, 256, 512))
#' }
#' @param search A \code{character} value for the hyperparameter search strategy: "random" (default), "grid".
#' @param tune_length A \code{numeric} value (integer) for either the maximum number of hyperparameter combinations ("random") or individual hyperparameter depth ("grid"): 100 (default).
#' @return A list containing the following components:\tabular{ll}{
#'    \code{df} \tab The data frame with anomalies removed. \cr
#'    \tab \cr
#'    \code{model} \tab The best model from the grid search used to detect anomalies. \cr
#'    \tab \cr
#'    \code{anom_score} \tab A data frame of predicted anomaly scores. \cr
#' }
#' @export
#' @examples
#' ## Import data.
#' data(ph_crocs)
#' \donttest{
#' ## Remove anomalies with autoencoder.
#' rm_outs <- ph_anomaly(df = ph_crocs, ids_col = "Biosample",
#'                       class_col = "Species", method = "ae")
#' ## Alternatively, remove anomalies with extended isolation forest. Notice
#' ## that port is defined, because running H2O sessions one after another
#' ## can return connection errors.
#' rm_outs <- ph_anomaly(df = ph_crocs, ids_col = "Biosample",
#'                       class_col = "Species", method = "iso",
#'                       port = 50001)
#' }
ph_anomaly <- function(df, ids_col, class_col, method = "ae", scale = FALSE,
                       center = NULL, sd = NULL, max_mem_size = "15g",
                       port = 54321, train_seed = 123, hyper_params = list(),
                       search = "random", tune_length = 100)
{
    df <- as.data.frame(df)
    if (!is.character(ids_col)) { ids_col <- as.character(ids_col) }
    if (!is.character(class_col)) { class_col <- as.character(class_col) }
    if (!(ids_col %in% colnames(df)))
        stop(paste("The ids column is either not in the data frame or is",
                   "differently named."))
    if (!(class_col %in% colnames(df)))
        stop(paste("The class column is either not in the data frame or is",
                   "differently named."))
    if (!(method %in% c("ae", "iso")))
        stop("Anomaly detection method does not exist.")
    if (length(method) > 1)
        stop("Only one method can be used.")
    # For sink().
    if (.Platform$OS.type == "unix") {
        tmp <- "/dev/null"
    } else {
        tmp <- "NUL"
    }
    # Check if h2o is installed.
    if (!is.character(max_mem_size))
        stop("Max memory size must be a character value.")
    if (!is.numeric(port))
        stop("The port number must be numeric.")
    if (!(search %in% c("random", "grid")))
        stop("Search strategy does not exist.")
    if (!is.numeric(train_seed))
        stop("Seed must be numeric (an integer).")
    if (!is.numeric(tune_length))
        stop("Tune length must be numeric (an integer).")
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
        meta_df <- df[, (names(df) %in% c(ids_col, class_col))]
        scale_df <- df[, !(names(df) %in% c(ids_col, class_col))]
        scale_df <- as.data.frame(scale(scale_df, center = center, scale = sd))
        df <- cbind.data.frame(meta_df, scale_df)
    }
    # closeAllConnections()
    gc()
    sink(tmp)
    requireNamespace("h2o", quietly = TRUE)
    h2o::h2o.init(max_mem_size = max_mem_size, port = port)
    h2o::h2o.removeAll()
    df_h2o <- h2o::as.h2o(df)
    predictors <- setdiff(colnames(df_h2o), c(ids_col, class_col))
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
    # Train autoencoder.
    if (method == "ae") {
        # Redefine search strategy names; making them consistent with train.
        if (length(hyper_params) == 0) {
            hyper_params <- list(missing_values_handling = "Skip",
                                 activation = "Rectifier",
                                 hidden = list(5, 25, 50, 100, 250, 500,
                                               nrow(df_h2o)),
                                 input_dropout_ratio = c(0, 0.1, 0.2, 0.3),
                                 rate = c(0, 0.01, 0.005, 0.001))
        } else {
            hyper_params <- hyper_params
        }
        if (!is.list(hyper_params))
            stop("Hyperparameters must be supplied as a list.")
        grid <- suppressWarnings(
                    h2o::h2o.grid(algorithm = 'deeplearning',
                                  x = predictors,
                                  training_frame = df_h2o,
                                  grid_id = 'ae_grid',
                                  autoencoder = TRUE,
                                  hyper_params = hyper_params,
                                  search_criteria = search_criteria,
                                  sparse = FALSE,
                                  ignore_const_cols = FALSE,
                                  seed = train_seed)
                )
        grids <- suppressWarnings(
                    h2o::h2o.getGrid(grid_id = "ae_grid",
                                     sort_by = "mse",
                                     decreasing = FALSE)
                 )
        model <- h2o::h2o.getModel(grids@model_ids[[1]])
        # Anomaly detection.
        anom_score <- as.data.frame(h2o::h2o.anomaly(model, df_h2o))
        outs <- ph_outs(anom_score$Reconstruction.MSE)
    } else {
        if (length(hyper_params) == 0) {
            hyper_params <- list(ntrees = c(50, 100, 150, 200),
                                 sample_size = c(64, 128, 256, 512))
        } else {
            hyper_params <- hyper_params
        }
        if (!is.list(hyper_params))
            stop("Hyperparameters must be supplied as a list.")
        grid <- suppressWarnings(
                    h2o::h2o.grid(algorithm = 'extendedisolationforest',
                                  x = predictors,
                                  training_frame = df_h2o,
                                  grid_id = 'iso_grid',
                                  hyper_params = hyper_params,
                                  search_criteria = search_criteria,
                                  seed = train_seed)
                )
        grids <- suppressWarnings(
                     h2o::h2o.getGrid(grid_id = "iso_grid",
                                      decreasing = TRUE)
                 )
        model <- h2o::h2o.getModel(grids@model_ids[[1]])
        # Anomaly detection.
        anom_score <- as.data.frame(h2o::h2o.predict(model, df_h2o))
        outs <- ph_outs(anom_score$anomaly_score)
    }
    df <- df[-c(outs), ]
    # Shutdown h2o cluster.
    h2o::h2o.shutdown(prompt = FALSE)
    sink()
    # closeAllConnections()
    list(df = df, model = model, anom_score = anom_score)
}
