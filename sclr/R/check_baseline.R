# Check for baseline boundary
# Arseniy Khvorov
# Created 2019/11/04
# Last edit 2019/11/04

#' Check for baseline boundary
#' 
#' Fits the scaled logit model as well as logistic regression. Does
#' a likelihood ratio test.
#'
#' @param formula Formula to use for model fitting.
#' @param data Optional dataframe.
#' @param fit_sclr Fit object returned by \code{\link{sclr}}.
#' @param fit_lr Fit object returned by \code{\link[stats]{glm}}.
#' @param conf_lvl Confidence level for the test
#' @param verbose Whether to print message based on test result.
#'
#' @return A \code{\link[tibble]{tibble}} with a summary.
#' 
#' @importFrom stats logLik pchisq terms glm binomial
#' 
#' @export
#'
#' @examples
#' library(sclr)
#' l1 <- sclr_ideal_data(n = 50, theta = 1e6, seed = 20191104)
#' check_baseline(status ~ logHI, l1)
check_baseline <- function(formula = NULL, data = NULL, 
                           fit_sclr = NULL, fit_lr = NULL,
                           conf_lvl = 0.95, verbose = TRUE) {
  if (is.null(formula)) {
    if (is.null(fit_sclr) && is.null(fit_lr))
      abort("specify formula or at least one fit object")
    else if (is.null(fit_sclr)) formula <- terms(fit_lr)
    else if (is.null(fit_lr)) formula <- terms(fit_sclr)
    else if (!identical(terms(fit_sclr), terms(fit_lr)))
      abort("the two fits must fit the same model")
    else formula <- terms(fit_sclr)
  }
  if (is.null(data)) {
    if (is.null(fit_sclr)) data <- fit_lr$data
    else if (is.null(fit_lr)) data <- fit_sclr$data
    else if (!identical(fit_lr$data, fit_sclr$data))
      abort("the two fits must fit the same data")
    else data <- fit_lr$data
  }
  if (is.null(fit_sclr)) fit_sclr <- suppressWarnings(sclr(formula, data))
  if (is.null(fit_lr)) fit_lr <- glm(formula, binomial(link = "logit"), data)
  ll_sclr <- sclr_log_likelihood(fit_sclr)
  ll_lr <- logLik(fit_lr)
  test_stat <-  2 * (ll_sclr - ll_lr)
  p_value <- pchisq(test_stat, df = 1, lower.tail = FALSE)
  if (verbose) {
    if (p_value < 1 - conf_lvl) message("unlikely baseline of 1")
    else message("likely baseline of 1")
  }
  tibble(
    sclr_log_lkhd = ll_sclr,
    lr_log_lkhd = ll_lr,
    test_statistic = test_stat,
    p_value = p_value
  )
}
