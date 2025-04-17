#' Resampling approach for model evaluation
#'
#' [evaluate_resampling()] uses repeated K-fold cross-validation and
#' the Root Mean Square Error (RMSE) of testing sets to measure the predictive
#' power of a single model. Methods are provided for
#' [`trending::trending_model`] (and lists of these) objects.
#'
#' @details These functions wrap around existing functions from several
#'   packages.  [evaluate_resampling.trending_model()] and
#'   [evaluate_resampling.list()] both use [rsample::vfold_cv()] for sampling
#'   and, for the calculating the different metrics, the
#'   [yardstick](https://yardstick.tidymodels.org/) package.
#'
#' @seealso [calculate_aic()], [calculate_rmse()], [calculate_mae()] and
#'   [calculate_rsq()].
#'
#' @inheritParams calculate_rmse
#' @param metric One of "rmse" (see [calculate_rmse]), "mae" (see
#'   [calculate_mae]) and "rsq" (see [calculate_rsq]).
#' @param metric_arguments A named list of arguments passed to the underlying
#'   functions that calculate the metrics.
#' @param v the number of equally sized data partitions to be used for K-fold
#'   cross-validation; `v` cross-validations will be performed, each using `v -
#'   1` partition as training set, and the remaining partition as testing set.
#'   Defaults to the number of row in data, so that the method uses
#'   leave-one-out cross validation, akin to Jackknife except that the testing
#'   set (and not the training set) is used to compute the fit statistics.
#' @param repeats the number of times the random K-fold cross validation should
#'   be repeated for; defaults to 1; larger values are likely to yield more
#'   reliable / stable results, at the expense of computational time
#'
#' @examples
#' x <- rnorm(100, mean = 0)
#' y <- rpois(n = 100, lambda = exp(x + 1))
#' dat <- data.frame(x = x, y = y)
#' model <- trending::glm_model(y ~ x, poisson)
#' models <- list(
#'   poisson_model = trending::glm_model(y ~ x, poisson),
#'   linear_model = trending::lm_model(y ~ x)
#' )
#'
#' evaluate_resampling(model, dat)
#' evaluate_resampling(models, dat)
#'
#' @export
evaluate_resampling <- function(x, ...) {
  UseMethod("evaluate_resampling")
}

# -------------------------------------------------------------------------

#' @aliases evaluate_resampling.default
#' @rdname evaluate_resampling
#' @export
evaluate_resampling.default <- function(x, ...) {
  not_implemented(x)
}

# -------------------------------------------------------------------------

#' @aliases evaluate_resampling.trending_model
#' @rdname evaluate_resampling
#' @export
evaluate_resampling.trending_model <- function(
    x,
    data,
    metric = c("rmse", "rsq", "mae"),
    metric_arguments = list(na.rm = TRUE),
    v = nrow(data),
    repeats = 1,
    ...
) {
  metric <- match.arg(metric)
  envir <- parent.frame(1)
  groups <- rsample::vfold_cv(data, v = v, repeats = repeats)
  fun <- sprintf("calculate_%s", metric)
  fun <- getFromNamespace(fun, ns = "trendeval")
  res <- evaluate_over_splits(x, groups$splits, fun, metric_arguments)
  structure(res, class = c("trendeval_resampling", class(res)))
}

# -------------------------------------------------------------------------

#' @aliases evaluate_resampling.trending_model
#' @rdname evaluate_resampling
#' @export
evaluate_resampling.list <- function(
    x,
    data,
    metric = c("rmse", "rsq", "mae"),
    metric_arguments = list(na.rm = TRUE),
    v = nrow(data),
    repeats = 1,
    ...
) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  nms <- names(x)
  if (is.null(nms)) nms <- paste0("model_", 1:length(x))
  nms <- rep(nms, each = v*repeats)
  res <- lapply(
    x,
    evaluate_resampling,
    data = data,
    metric = metric,
    metric_arguments = metric_arguments,
    v = v,
    repeats = repeats
  )
  out <- do.call(rbind, res)
  out <- tibble(model_name = nms, out)
  class(out) <- c("trendeval_resampling", class(out))
  out
}

#' @export
summary.trendeval_resampling <- function(object, ...) {
  if (!"model_name" %in% names(object)) object$model_name <- "model_1"
  res <- tapply(object$result, object$model_name, mean, na.rm = TRUE, simplify = FALSE)
  nas <- tapply(object$result, object$model_name, is.na, simplify = FALSE)
  splits_averaged <- vapply(nas, length, numeric(1))
  nas_removed <- vapply(nas, sum, numeric(1))
  value <- unlist(res)
  tibble(
    model_name = names(value),
    metric = object$metric[1],
    value,
    splits_averaged,
    nas_removed
  )
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

evaluate_over_splits <- function(model, splits, fun, fun_args) {
  res <- lapply(
    splits,
    function(split) {
      splitfit <- fit(model, rsample::analysis(split))
      validation <- predict(splitfit, rsample::assessment(split))
      metric_results <- do.call(
        fun,
        args = append(list(x=validation), fun_args)
      )
      tibble::tibble(
        metric_results,
        model = list(model),
      #  fitted_model = splitfit[1],
        fitting_warnings = splitfit[2],
        fitting_errors = splitfit[3],
     #   pred = validation[1],
        predicting_warnings = validation[2],
        predicting_errors = validation[3]
      )
    }
  )
  do.call(rbind, res)

}
