#' Distribution Mapping based Transfer Learning
#'
#' This function performs distribution mapping based transfer learning (DMTL)
#' regression for given target (primary) and source (secondary) datasets. The
#' data available in the source domain are used to design an appropriate
#' predictive model. The target features with unknown response values are
#' transferred to the source domain _via_ distribution matching and then the
#' corresponding response values in the source domain are predicted using the
#' aforementioned predictive model. The response values are then transferred to
#' the original target space by applying distribution matching again. Hence,
#' this function needs an **unmatched** pair of target datasets (features and
#' response values) and a **matched** pair of source datasets.
#'
#' @param target_set List containing the target datasets. A named list with
#' components `X` (predictors) and `y` (response). The predictions are performed
#' to estimate the response values corresponding to `X` while `y` is only used
#' to estimate the response distribution parameters.
#' @param source_set List containing the source datasets. A named list with
#' components `X` (predictors) and `y` (response). These two sets must be matched
#' and used in both distribution estimation and predictive modeling.
#' @param use_density Flag for using kernel density as distribution estimate
#' instead of histogram counts. Defaults to `FALSE`.
#' @param pred_model String indicating the underlying predictive model. The
#' currently available options are -
#' * `RF` for random forest regression. If `model_optimize = FALSE`, builds a
#' model with `n_tree = 200` and `m_try = 0.4`.
#' * `SVM` for support vector regression. If `model_optimize = FALSE`, builds a
#' model with `kernel = "poly"`, `C = 2`, and `degree = 3`.
#' * `EN` for elastic net regression. If `model_optimize = FALSE`, builds a
#' model with `alpha = 0.8` and `lambda` generated from a 5-fold cross
#' validation.
#' @param model_optimize Flag for model parameter tuning. If `TRUE`, performs a
#' grid search to optimize parameters and train with the resulting model.
#' If `FALSE`, uses a set of predefined parameters. Defaults to `FALSE`.
#' @param sample_size Sample size for estimating distributions of target and
#' source datasets. Defaults to `1e3`.
#' @param random_seed Seed for random number generator (for reproducible
#' outcomes). Defaults to `NULL`.
#' @param all_pred Flag for returning the prediction values in the source space.
#' If `TRUE`, the function returns a named list with two components- `target`
#' and `source` (predictions in the target space and source space,
#' respectively). Defaults to `FALSE`.
#' @param get_verbose Flag for displaying the progress when optimizing the
#' predictive model _i.e._, `model_optimize = TRUE`. Defaults to `FALSE`.
#' @param allow_parallel Flag for allowing parallel processing when performing
#' grid search _i.e._, `model_optimimze = TRUE`. Defaults to `FALSE`.
#'
#' @return
#' If `all_pred = FALSE`, a vector containing the final prediction values.
#'
#' If `all_pred = TRUE`, a named list with two components `target` and `source`
#' _i.e._, predictions in the original target space and in source space,
#' respectively.
#'
#' @note
#' * The datasets in `target_set` (_i.e._, `X` and `y`) do not need to be
#' matched (_i.e._, have the same number of rows) since the response values are
#' used only to estimate distribution for mapping while the feature values are
#' used for both mapping and final prediction. In contrast, the datasets in
#' `source_set` (_i.e._, `X` and `y`) must have matched samples.
#' * It is recommended to normalize the two response values (`y`) so that
#' they will be in the same range. If normalization is not performed, `DMTL()`
#' uses the range of target `y` values as the prediction range.
#'
#' @keywords distribution-matching transfer-learning domain-transfer
#' histogram-matching density-matching
#' @export
#' @examples
#' set.seed(8644)
#'
#' ## Generate two dataset with different underlying distributions...
#' x1 <- matrix(rnorm(3000, 0.3, 0.6), ncol = 3)
#' dimnames(x1) <- list(paste0("sample", 1:1000), paste0("f", 1:3))
#' y1 <- 0.3*x1[, 1] + 0.1*x1[, 2] - x1[, 3] + rnorm(1000, 0, 0.05)
#' x2 <- matrix(rnorm(3000, 0, 0.5), ncol = 3)
#' dimnames(x2) <- list(paste0("sample", 1:1000), paste0("f", 1:3))
#' y2 <- -0.2*x2[, 1] + 0.3*x2[, 2] - x2[, 3] + rnorm(1000, 0, 0.05)
#'
#' ## Model datasets using DMTL & compare with a baseline model...
#' library(DMTL)
#'
#' target <- list(X = x1, y = y1)
#' source <- list(X = x2, y = y2)
#' y1_pred <- DMTL(target_set = target, source_set = source, pred_model = "RF")
#' y1_pred_bl <- RF_predict(x_train = x2, y_train = y2, x_test = x1)
#'
#' print(performance(y1, y1_pred, measures = c("MSE", "PCC")))
#' print(performance(y1, y1_pred_bl, measures = c("MSE", "PCC")))
#'

## Dependency: stats, ks
## Dependency_own: lambda_functions, estimate_cdf, dist_match,
##                  predictive_modeling
##
## Author: SR Dhruba, Dec 2020
################################################################################

DMTL <- function(target_set, source_set, use_density = FALSE, pred_model = "RF", model_optimize = FALSE, sample_size = 1e3,
                 random_seed = NULL, all_pred = FALSE, get_verbose = FALSE, allow_parallel = FALSE) {

    ## Provide element names...
    if (is.null(names(target_set)) & length(target_set) == 2) {
        warning("Missing names for target set elements... assigning 'X' to first element and 'y' to second.")
        names(target_set) <- c("X", "y")
    }

    if (is.null(names(source_set)) & length(source_set) == 2) {
        warning("Missing names for source set elements... assigning 'X' to first element and 'y' to second.")
        names(source_set) <- c("X", "y")
    }


    ## Initial check...
    if (ncol(target_set[["X"]]) != ncol(source_set[["X"]]))
        stop("Source and target set covariates must have the same number of features!")

    if (nrow(source_set[["X"]]) != length(source_set[["y"]]))
        stop("Source sets must have the same number of samples!")


    ## Define datasets...
    X1 <- norm_data(target_set[["X"]]);      y1 <- target_set[["y"]]
    X2 <- norm_data(source_set[["X"]]);      y2 <- source_set[["y"]]
    n_feat <- ncol(X1);                      data_lims <- range(y1)


    ## Distribution matching for predictors...
    X2_map <- lapply(1:n_feat, function(j) {
        dist_match(X1[, j], ref = X2[, j], density = use_density, lims = data_lims, samples = sample_size, seed = random_seed)
        })
    X2_map <- as.data.frame(X2_map);    dimnames(X2_map) <- dimnames(X1)


    ## Perform prediction...
    pred_model <- toupper(pred_model)
    if (pred_model == "RF") {
        y2_pred_map <- RF_predict(x_train = X2, y_train = y2, x_test = X2_map, lims = data_lims, optimize = model_optimize,
                                  n_tree = 200, m_try = 0.4, seed = random_seed, verbose = get_verbose, parallel = allow_parallel)
    } else if (pred_model == "SVM") {
        y2_pred_map <- SVM_predict(x_train = X2, y_train = y2, x_test = X2_map, lims = data_lims, optimize = model_optimize,
                                   kernel = "poly", C = 2, eps = 0.01, kpar = list(degree = 3), seed = random_seed,
                                   verbose = get_verbose, parallel = allow_parallel)
    } else if (pred_model == "EN") {
        y2_pred_map <- EN_predict(x_train = X2, y_train = y2, x_test = X2_map, lims = data_lims, optimize = model_optimize,
                                  alpha = 0.8, seed = random_seed, verbose = get_verbose, parallel = allow_parallel)
    } else {
        stop("Invalide model! Please check the documentation for the valid model options available.")
    }
    y2_pred_map <- as.vector(y2_pred_map);              names(y2_pred_map) <- rownames(X2_map)


    ## Map back to original space...
    y2_cdf  <- estimate_cdf(y2, samples = sample_size, unit_range = TRUE, density = use_density, grids = 1e3, seed = random_seed)

    y1_pred <- dist_match(y2_pred_map, ref = y1, src_cdf = y2_cdf, density = use_density, samples = sample_size,
                          lims = data_lims, seed = random_seed)
    y1_pred <- confined(y1_pred, lims = data_lims);     names(y1_pred) <- names(y2_pred_map)


    ## Return output objects...
    if (all_pred)
        return( list("target" = y1_pred, "source" = y2_pred_map) )

    y1_pred

}
