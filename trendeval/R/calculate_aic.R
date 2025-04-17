#' Generic for calculating the AIC
#'
#' Generic `calculate_aic()` returns the Akaike's 'An Information Criterion' for
#'   the given input.
#'
#' @param x An \R object.
#' @param data a `data.frame` containing data (including the response variable
#'   and all predictors) used in the specified model.
#' @param as_tibble Should the result be returned as [tibble][tibble::tibble()]
#'  (`as_tibble = TRUE`) or a list (`as_tibble = FALSE`).
#' @param ... Not currently used.
#'
#' @details Specific methods are given for
#'   [`trending_fit`][trending::fit.trending_model()] and
#'   [`trending_fit_tbl`][trending::fit.trending_model()] objects. The default
#'   method applies [stats::AIC()] directly.
#'
#' @return For a single [`trending_fit`][trending::fit()] input, if
#'   `as_tibble = FALSE` the object returned will be a list with entries:
#'
#'   - metric: "AIC"
#'   - result: the resulting AIC value fit (NULL if the calculation failed)
#'   - warnings: any warnings generated during calculation
#'   - errors: any errors generated during calculation
#'
#'   If `as_tibble = TRUE`, or the input is a
#'   [`trending_fit_tbl`][trending::fit.trending_model()], then the output
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
#' calculate_aic(poisson_model, dat)
#' calculate_aic(fitted_model)
#' calculate_aic(fitted_model, as_tibble = TRUE)
#' calculate_aic(fitted_models)
#'
#' @export
calculate_aic <- function(x, ...) {
  UseMethod("calculate_aic")
}

# -------------------------------------------------------------------------

#' @aliases calculate_aic.default
#' @rdname calculate_aic
#' @export
calculate_aic.default <- function(x, ...) {
  stats::AIC(x)
}

# -------------------------------------------------------------------------

#' @aliases calculate_aic.trending_model
#' @rdname calculate_aic
#' @export
calculate_aic.trending_model <- function(x, data, as_tibble = FALSE, ...) {
  tmp <- fit(x, data)
  fitted_model <- get_fitted_model(tmp)[[1L]]
  calculate_aic_internal(fitted_model, as_tibble = as_tibble)
}

# -------------------------------------------------------------------------

#' @aliases calculate_aic.list
#' @rdname calculate_aic
#' @export
calculate_aic.list <- function(x, data, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  res <- eval(bquote(fit(x, .(data))))
  calculate_aic(res)
}

# -------------------------------------------------------------------------

#' @aliases calculate_aic.trending_fit
#' @rdname calculate_aic
#' @export
calculate_aic.trending_fit <- function(x, as_tibble = FALSE, ...) {
  fitted_model <- get_fitted_model(x)
  calculate_aic_internal(fitted_model, as_tibble = as_tibble)
}

# -------------------------------------------------------------------------

#' @aliases calculate_aic.trending_fit_tbl
#' @rdname calculate_aic
#' @export
calculate_aic.trending_fit_tbl <- function(x, ...) {
  fitted_models <- get_fitted_model(x)
  res <- lapply(fitted_models, calculate_aic_internal, as_tibble = TRUE)
  res <- do.call(rbind, res)
  nm_var <- attr(x, "model_name")
  nms <- if (is.null(nm_var)) paste0("model_", 1:nrow(x)) else x[[nm_var]]
  tibble(model_name = nms, res)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

calculate_aic_internal <- function(x, as_tibble) {
  f <- make_catcher(stats::AIC)
  res <- f(x)
  if (as_tibble) {
    result <- res$result
    if (is.null(result)) result <- NA_real_
    res <- tibble(
      metric = "aic",
      result = result,
      warnings = list(res$warnings),
      errors = list(res$errors)
    )
  } else {
    res <- c(list(metric = "aic"), res)
  }
  res
}

