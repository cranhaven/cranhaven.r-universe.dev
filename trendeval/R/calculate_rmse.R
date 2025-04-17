#' Generic for calculating the root mean squared error
#'
#' Generic `calculate_rmse()` returns the root mean square error for the given
#'   input.
#'
#' @inheritParams calculate_aic
#' @param new_data a `data.frame` containing data (including the response variable
#'   and all predictors) on which to assess the model.
#' @param na.rm Should NA values should be removed before calculation of metric
#'   (passed to the underlying function [yardstick::rmse_vec]).
#'
#' @details Specific methods are given for [`trending_model`] (and lists of
#'   these), [`trending_fit`][trending::fit.trending_model()],
#'   [`trending_fit_tbl`][trending::fit.trending_model()],
#'   [`trending_predict_tbl`][trending::predict.trending_fit()],
#'   [`trending_predict_tbl`][trending::predict.trending_fit_tbl()] and
#'   `trending_prediction` objects. Each of these are simply wrappers around the
#'   [yardstick::rmse_vec] with the addition of explicit error handling.
#'
#' @return For a single [`trending_fit`][trending::fit()] input, if
#'   `as_tibble = FALSE` the object returned will be a list with entries:
#'
#'   - metric: "rmse"
#'   - result: the resulting rmse value (NULL if the calculation failed)
#'   - warnings: any warnings generated during calculation
#'   - errors: any errors generated during calculation
#'
#'   If `as_tibble = TRUE`, or for the other `trending` classes, then the output
#'   will be a [tibble][tibble::tibble()] with one row for each fitted model
#'   columns corresponding to output generated with single model input.
#'
#' @author Tim Taylor
#'
#' #' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#' fitted_model <- fit(poisson_model, dat)
#' fitted_models <- fit(list(poisson_model, negbin_model), data = dat)
#'
#' calculate_rmse(poisson_model, dat)
#' calculate_rmse(fitted_model)
#' calculate_rmse(fitted_model, as_tibble = TRUE)
#' calculate_rmse(fitted_models)
#'
#' @export
calculate_rmse <- function(x, ...) UseMethod("calculate_rmse")

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.default
#' @rdname calculate_rmse
#' @export
calculate_rmse.default <- function(x, ...) not_implemented(x)

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.trending_model
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_model <- function(x, data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_model(
    x = x,
    data = data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "rmse_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.list
#' @rdname calculate_rmse
#' @export
calculate_rmse.list <- function(x, data, na.rm = TRUE, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  res <- eval(bquote(fit(x, .(data))))
  calculate_rmse(res, na.rm = na.rm)
}

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.trending_fit
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_fit <- function(x, new_data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_fit(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "rmse_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.trending_fit_tbl
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_fit_tbl <- function(x, new_data, na.rm = TRUE, ...) {
  calculate_yardstick_trending_fit_tbl(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    metric = "rmse_vec"
  )

}

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.trending_predict
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_predict <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_predict(
    x = x,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "rmse_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.trending_predict_tbl
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_predict_tbl <- function(x, na.rm = TRUE, ...) {
  calculate_yardstick_trending_predict_tbl(
    x = x,
    na.rm = na.rm,
    metric = "rmse_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_rmse.trending_prediction
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_prediction <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick(
    x,
    truth = get_response(x),
    estimate = get_estimate(x),
    metric = "rmse_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}
