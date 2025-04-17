#' Generic for calculating the AIC
#'
#' `evaluate_aic()` is a a generic for evaluating the Akaike's
#'   'An Information Criterion' for a given input
#'
#' @inheritParams calculate_aic
#'
#' @details Specific methods are given for
#'   [`trending_fit`][trending::fit.trending_model()] and lists of these
#'   models.
#'
#' @return If `as_tibble = TRUE`, or the input is a list of models then the
#'   output will be a [tibble][tibble::tibble()] with one row for each fitted
#'   model columns corresponding to output generated with single model input.
#'
#' @author Tim Taylor
#'
#' #' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#'
#' evaluate_aic(poisson_model, dat)
#' evaluate_aic(list(poisson_model, negbin_model), data = dat)
#'
#' @export
evaluate_aic <- function(x, ...) {
  UseMethod("evaluate_aic")
}

# -------------------------------------------------------------------------

#' @aliases evaluate_aic.default
#' @rdname evaluate_aic
#' @export
evaluate_aic.default <- function(x, ...) {
  not_implemented(x)
}

# -------------------------------------------------------------------------

#' @aliases evaluate_aic.trending_model
#' @rdname evaluate_aic
#' @export
evaluate_aic.trending_model <- calculate_aic.trending_model

# -------------------------------------------------------------------------

#' @aliases evaluate_aic.trending_list
#' @rdname evaluate_aic
#' @export
evaluate_aic.list <- calculate_aic.list
