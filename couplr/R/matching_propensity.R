# ==============================================================================
# Propensity Score Matching
# ==============================================================================

#' Propensity Score Matching
#'
#' Matches treated and control units based on estimated propensity scores.
#' Fits a logistic regression model (or accepts a pre-fitted one), computes
#' logit propensity scores, and calls [match_couples()] with a caliper on
#' the logit scale.
#'
#' @param formula Formula for propensity score model (treatment ~ covariates).
#'   Required if `ps_model` is NULL.
#' @param data Combined dataset containing both treated and control units
#' @param treatment Name of the binary treatment column (0/1 or logical)
#' @param ps_model Pre-fitted `glm` object (alternative to formula).
#'   If provided, `formula` is ignored.
#' @param caliper_sd Caliper width in standard deviations of logit(PS).
#'   Default: 0.2 (Rosenbaum and Rubin recommendation).
#' @param method LAP solver method (default: "auto")
#' @param replace If TRUE, match with replacement (default: FALSE)
#' @param ratio Integer k for k:1 matching (default: 1)
#' @param ... Additional arguments passed to [match_couples()]
#'
#' @return A matching_result object with additional propensity score info
#'   in `result$info$ps_model` and `result$info$caliper_value`.
#'
#' @details
#' The propensity score is the probability of treatment assignment conditional
#' on observed covariates. Matching is performed on the logit of the propensity
#' score (Rosenbaum and Rubin 1985), which provides better distributional
#' properties than matching on the raw probability scale.
#'
#' The default caliper of 0.2 SD of logit(PS) is recommended by Austin (2011)
#' as removing approximately 98% of bias.
#'
#' @examples
#' set.seed(42)
#' n <- 100
#' data <- data.frame(
#'   id = seq_len(n),
#'   treated = rbinom(n, 1, 0.4),
#'   age = rnorm(n, 50, 10),
#'   income = rnorm(n, 50000, 15000)
#' )
#' result <- ps_match(treated ~ age + income, data = data, treatment = "treated")
#' print(result)
#'
#' @export
ps_match <- function(formula = NULL,
                     data = NULL,
                     treatment = NULL,
                     ps_model = NULL,
                     caliper_sd = 0.2,
                     method = "auto",
                     replace = FALSE,
                     ratio = 1L,
                     ...) {

  # --- Validation ---
  if (is.null(data)) {
    stop("data must be provided", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop(sprintf("data must be a data.frame, got %s", class(data)[1]),
         call. = FALSE)
  }
  if (is.null(treatment)) {
    stop("treatment column name must be specified", call. = FALSE)
  }
  if (!(treatment %in% names(data))) {
    stop(sprintf("treatment column '%s' not found in data", treatment),
         call. = FALSE)
  }

  trt <- data[[treatment]]
  # Coerce logical to integer
  if (is.logical(trt)) trt <- as.integer(trt)
  if (!all(trt %in% c(0L, 1L))) {
    stop("treatment column must be binary (0/1 or TRUE/FALSE)", call. = FALSE)
  }

  if (is.null(formula) && is.null(ps_model)) {
    stop("Either formula or ps_model must be provided", call. = FALSE)
  }

  if (!is.numeric(caliper_sd) || length(caliper_sd) != 1 || caliper_sd <= 0) {
    stop("caliper_sd must be a positive number", call. = FALSE)
  }

  # --- Fit model ---
  if (is.null(ps_model)) {
    ps_model <- stats::glm(formula, data = data, family = stats::binomial())
  }

  # --- Compute propensity scores ---
  # Predict on `data` explicitly so a pre-fitted ps_model (possibly fit on a
  # differently ordered or subset frame) produces scores aligned to these rows.
  ps <- stats::predict(ps_model, newdata = data, type = "response")

  # Clip extreme values to avoid Inf logit
  ps <- pmax(ps, 1e-6)
  ps <- pmin(ps, 1 - 1e-6)

  logit_ps <- log(ps / (1 - ps))

  # --- Split into treated (left) and control (right) ---
  treated_idx <- which(trt == 1L)
  control_idx <- which(trt == 0L)

  if (length(treated_idx) == 0 || length(control_idx) == 0) {
    stop("Both treatment groups must have at least one unit", call. = FALSE)
  }

  # Detect ID column
  id_col <- if ("id" %in% names(data)) "id" else NULL

  left <- data.frame(
    id = if (!is.null(id_col)) data[[id_col]][treated_idx] else treated_idx,
    logit_ps = logit_ps[treated_idx],
    stringsAsFactors = FALSE
  )

  right <- data.frame(
    id = if (!is.null(id_col)) data[[id_col]][control_idx] else control_idx,
    logit_ps = logit_ps[control_idx],
    stringsAsFactors = FALSE
  )

  # --- Caliper ---
  caliper_value <- caliper_sd * stats::sd(logit_ps)

  # --- Match ---
  result <- match_couples(
    left, right,
    vars = "logit_ps",
    calipers = list(logit_ps = caliper_value),
    method = method,
    replace = replace,
    ratio = ratio,
    ...
  )

  # --- Attach PS metadata ---
  result$info$ps_model <- ps_model
  result$info$caliper_value <- caliper_value
  result$info$caliper_sd <- caliper_sd
  result$info$n_treated <- length(treated_idx)
  result$info$n_control <- length(control_idx)

  result
}
