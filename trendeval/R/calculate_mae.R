#' Generic for calculating the root mean squared error
#'
#' Generic `calculate_mae()` returns the root mean square error for the given
#'   input.
#'
#' @inheritParams calculate_rmse
#' @param na.rm Should NA values should be removed before calculation of metric
#'   (passed to the underlying function [yardstick::mae_vec]).
#'
#' @details Specific methods are given for [`trending_model`] (and lists of
#'   these), [`trending_fit`][trending::fit.trending_model()],
#'   [`trending_fit_tbl`][trending::fit.trending_model()],
#'   [`trending_predict_tbl`][trending::predict.trending_fit()],
#'   [`trending_predict_tbl`][trending::predict.trending_fit_tbl()] and
#'   `trending_prediction` objects. Each of these are simply wrappers around the
#'   [yardstick::mae_vec] with the addition of explicit error handling.
#'
#' @return For a single [`trending_fit`][trending::fit()] input, if
#'   `as_tibble = FALSE` the object returned will be a list with entries:
#'
#'   - metric: "mae"
#'   - result: the resulting mae value (NULL if the calculation failed)
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
#' calculate_mae(poisson_model, dat)
#' calculate_mae(fitted_model)
#' calculate_mae(fitted_model, as_tibble = TRUE)
#' calculate_mae(fitted_models)
#'
#' @export
calculate_mae <- function(x, ...) UseMethod("calculate_mae")

# -------------------------------------------------------------------------

#' @aliases calculate_mae.default
#' @rdname calculate_mae
#' @export
calculate_mae.default <- function(x, ...) not_implemented(x)

# -------------------------------------------------------------------------

#' @aliases calculate_mae.trending_model
#' @rdname calculate_mae
#' @export
calculate_mae.trending_model <- function(x, data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_model(
    x = x,
    data = data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "mae_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_mae.list
#' @rdname calculate_mae
#' @export
calculate_mae.list <- function(x, data, na.rm = TRUE, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  res <- eval(bquote(fit(x, .(data))))
  calculate_mae(res, na.rm = na.rm)
}

# -------------------------------------------------------------------------

#' @aliases calculate_mae.trending_fit
#' @rdname calculate_mae
#' @export
calculate_mae.trending_fit <- function(x, new_data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_fit(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "mae_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_mae.trending_fit_tbl
#' @rdname calculate_mae
#' @export
calculate_mae.trending_fit_tbl <- function(x, new_data, na.rm = TRUE, ...) {
  calculate_yardstick_trending_fit_tbl(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    metric = "mae_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_mae.trending_predict
#' @rdname calculate_mae
#' @export
calculate_mae.trending_predict <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_predict(
    x = x,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "mae_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_mae.trending_predict_tbl
#' @rdname calculate_mae
#' @export
calculate_mae.trending_predict_tbl <- function(x, na.rm = TRUE, ...) {
  calculate_yardstick_trending_predict_tbl(
    x = x,
    na.rm = na.rm,
    metric = "mae_vec"
  )
}

# -------------------------------------------------------------------------

#' @aliases calculate_mae.trending_prediction
#' @rdname calculate_mae
#' @export
calculate_mae.trending_prediction <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick(
    x,
    truth = get_response(x),
    estimate = get_estimate(x),
    metric = "mae_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}
