#' Cross-Validated Predictive Accuracy of an IMR Fit
#'
#' @description
#' `cv_imr()` evaluates predictive accuracy of a model fitted with [imr()] using
#' repeated \eqn{K}-fold splits and Bayesian model averaging over the fitted
#' MCMC samples.  The sampler is not re-run inside each training fold; fit a
#' separate object with `method = "BMS"` if a no-borrowing comparison is needed.
#' The accuracy measure depends on the outcome type: the concordance index
#' (C-index) for right-censored outcomes, the area under the ROC curve (AUC) for
#' binary outcomes, and the mean squared error (MSE) for continuous outcomes.
#'
#' @param object A fitted object of class `"imr"` returned by [imr()].
#' @param k Integer number of cross-validation folds per round (default `5`).
#'   Must be at least `2`, and each availability subgroup must contain at least
#'   `k` subjects.
#' @param rounds Integer number of independent cross-validation rounds to
#'   average over (default `2`).  Must be positive.
#' @param method Optional compatibility argument.  If supplied, it must match
#'   the method stored in `object`; `cv_imr()` cannot turn an IMR fit into a BMS
#'   fit or vice versa.
#' @param max_models Integer maximum number of selection models used for
#'   Bayesian model averaging (default `100`).  Must be positive.
#' @param verbose Logical; if `TRUE`, print the C routine's cross-validation
#'   diagnostics.  Defaults to `FALSE`.
#'
#' @return A list with two numeric matrices, each of dimension
#'   `rounds` x `(n_subgroups + 1)`, whose last column corresponds to all
#'   subjects pooled and whose remaining columns are named by the availability
#'   subgroup bitstrings:
#'   \item{total_cindex}{Accuracy computed on the pooled cross-validated
#'     predictions within each availability subgroup (and overall).}
#'   \item{subset_cindex}{Accuracy computed fold-by-fold within each
#'     availability subgroup.}
#'   The accuracy metric (`"C-index"`, `"AUC"` or `"MSE"`) is recorded in the
#'   `"metric"` attribute of the returned list.
#'
#' @seealso [imr()], [predict.imr()]
#'
#' @examples
#' \donttest{
#' data("simIMR", package = "IntegMultiReg")
#' fit <- imr(
#'   platform_data_list = simIMR$platforms, outcome = simIMR$outcome,
#'   cov = simIMR$covariates, type_outcome = "binary",
#'   nu = c(-4, -3, -4), sample_mcmc = c(200, 100), ssize = 5, seed = 1
#' )
#' cv <- cv_imr(fit, k = 5, rounds = 2)
#' cv$total_cindex
#' }
#' @export
cv_imr <- function(object, k = 5, rounds = 2,
                   method = NULL,
                   max_models = 100, verbose = FALSE) {
  if (!inherits(object, "imr")) {
    .imr_abort("`object` must be an `imr` object returned by `imr()`.")
  }
  .imr_check_flag(verbose, "verbose")
  k <- .imr_check_integer_scalar(k, "k", min = 2)
  rounds <- .imr_check_integer_scalar(rounds, "rounds", min = 1)
  max_models <- .imr_check_integer_scalar(max_models, "max_models", min = 1)
  object_method <- object$method
  if (is.null(object_method)) object_method <- "IMR"
  if (is.null(method)) {
    method <- object_method
  } else {
    method <- match.arg(method, c("IMR", "BMS"))
    if (!identical(method, object_method)) {
      .imr_abort(
        "`method` must match the fitted object; fit a separate `imr(..., method = \"BMS\")` object for BMS validation."
      )
    }
  }
  if (any(object$sample_size < k)) {
    .imr_abort(
      "`k` must not exceed the sample size of any modelled availability subgroup."
    )
  }
  n_platform <- object$data1[[1]]
  method_c <- as.character(method)
  type_out <- object$data1[[8]]
  metric <- c("C-index", "AUC", "MSE")[type_out]

  results <- .quietly(verbose, .Call("mainFunctionPrediction",
    h0_c = object$list_hyperpara[1],
    hh_c = object$list_hyperpara[2],
    alpha_c = object$list_hyperpara[3],
    psi_c = object$list_hyperpara[4],
    alpha0_c = object$list_hyperpara[5],
    beta0_c = object$list_hyperpara[6],
    seed_c = object$list_hyperpara[7],
    nu_c = object$list_hyperpara[8:(7 + n_platform)],
    y_latent = object$estimate_latent_y,
    gam_sample_c = object$gam_sample,
    theta_c = object$theta_mean,
    method_c = method_c,
    n_platform_c = as.integer(n_platform),
    platform_models_c = object$data1[[2]],
    model_platforms_c = object$data1[[3]],
    n_models = as.integer(object$data1[[4]]),
    sample_size = as.integer(object$data1[[5]]),
    n_features = as.integer(object$data1[[6]]),
    n_cov = as.integer(object$data1[[7]]),
    x_filtered = object$data2[[1]],
    y_list = object$data2[[2]],
    cov_list = object$data2[[3]],
    type_outcome = as.integer(object$data1[[8]]),
    sample = as.integer(object$data1[[9]]),
    kcv = as.integer(k),
    round10cv = as.integer(rounds),
    max_models_pred = as.integer(max_models)
  ))

  ## Label the columns: one per availability subgroup plus a pooled "all" column.
  col_labels <- c(object$model_bitstrings, "all")
  if (!is.null(results$total_cindex) &&
      ncol(results$total_cindex) == length(col_labels)) {
    colnames(results$total_cindex) <- col_labels
    colnames(results$subset_cindex) <- col_labels
  }
  attr(results, "metric") <- metric
  return(results)
}
