# R/calibrate.R -- et_calibrate() and et_diagnose()

# ******************************************************************************
# et_calibrate(): coverage probability assessment
# ______________________________________________________________________________

#' Assess calibration of posterior predictive intervals
#'
#' Computes observed coverage probability at multiple nominal CI levels.
#' A well-calibrated Bayesian model should produce 90\% CIs that contain
#' the true value 90\% of the time, etc.
#'
#' @param predictions An \code{et_prediction} or \code{et_prediction_list}.
#' @param observed A \code{data.frame} with true response values.  Must have
#'   the same number of rows as \code{predictions$newdata} (rows are matched
#'   positionally) and a column with the true response values.
#' @param response_col Character.  Name of the response column in
#'   \code{observed}.  Defaults to the left-hand side of the model formula
#'   if it can be inferred, otherwise must be specified.
#' @param ci_levels Numeric vector.  CI levels to assess.  Defaults to all
#'   levels present in the \code{et_prediction} object.
#' @param ... Unused.
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{ci_level}{Nominal CI level.}
#'     \item{nominal}{Same as ci_level.}
#'     \item{observed_coverage}{Fraction of true values falling inside the CI.}
#'     \item{n_obs}{Number of observations used.}
#'     \item{calibration_error}{Signed difference: observed - nominal.
#'       Positive = over-coverage (CIs too wide / conservative).
#'       Negative = under-coverage (CIs too narrow / overconfident).}
#'     \item{sharpness}{Mean CI width across observations.  Sharpness and
#'       calibration are complementary: a model can be calibrated but useless
#'       if sharpness is poor (very wide CIs).}
#'   }
#'   For grouped predictions, a \code{group} column is prepended.
#' @examples
#' \donttest{
#' set.seed(1)
#' df  <- data.frame(y = rnorm(20), x1 = rnorm(20))
#' fit <- et_fit(y ~ x1, data = df,
#'               chains = 1, iter = 500, warmup = 250,
#'               cores = 1, refresh = 0)
#' valid_df <- data.frame(y = rnorm(5), x1 = rnorm(5))
#' pred <- et_predict(fit, newdata = valid_df,
#'                    n_draws = 200, n_perturb = 50)
#' cal <- et_calibrate(pred, observed = valid_df, response_col = "y")
#' print(cal)
#' }
#' @export
et_calibrate <- function(predictions, observed, response_col = NULL,
                          ci_levels = NULL, ...) {
  UseMethod("et_calibrate")
}

#' @export
et_calibrate.et_prediction <- function(predictions, observed,
                                         response_col = NULL,
                                         ci_levels = NULL, ...) {
  response_col <- .resolve_response_col(response_col, predictions$model)
  if (nrow(observed) != nrow(predictions$newdata)) {
    stop("observed must have the same number of rows as newdata (",
         nrow(predictions$newdata), "), got ", nrow(observed), ".")
  }
  y_true <- observed[[response_col]]
  if (is.null(y_true)) {
    stop("Column '", response_col, "' not found in observed data.frame.")
  }
  .compute_calibration(predictions$credible_intervals, y_true, ci_levels)
}

#' @export
et_calibrate.et_prediction_list <- function(predictions, observed,
                                              response_col = NULL,
                                              ci_levels = NULL, ...) {
  grouping <- predictions$grouping
  if (!grouping %in% colnames(observed)) {
    stop("Grouping column '", grouping, "' not found in observed.")
  }

  parts <- lapply(names(predictions$predictions), function(g) {
    pred <- predictions$predictions[[g]]
    if (is.null(pred)) return(NULL)
    obs_g <- observed[observed[[grouping]] == g, , drop = FALSE]
    if (nrow(obs_g) == 0) return(NULL)

    rc <- .resolve_response_col(response_col, pred$model)
    y_true <- obs_g[[rc]]
    if (is.null(y_true) || length(y_true) != nrow(pred$newdata)) return(NULL)

    cal <- .compute_calibration(pred$credible_intervals, y_true, ci_levels)
    cbind(data.frame(group = g, stringsAsFactors = FALSE), cal)
  })

  do.call(rbind, Filter(Negate(is.null), parts))
}

.compute_calibration <- function(ci_df, y_true, ci_levels) {
  avail <- unique(ci_df$ci_level)
  levels_use <- if (is.null(ci_levels)) avail else intersect(ci_levels, avail)

  rows <- lapply(levels_use, function(lv) {
    sub    <- ci_df[ci_df$ci_level == lv, ]
    inside <- y_true >= sub$lower & y_true <= sub$upper
    cov    <- mean(inside, na.rm = TRUE)
    data.frame(
      ci_level           = lv,
      nominal            = lv,
      observed_coverage  = cov,
      n_obs              = sum(!is.na(inside)),
      calibration_error  = cov - lv,   # signed: + over-coverage, - under-coverage
      sharpness          = mean(sub$width, na.rm = TRUE),
      stringsAsFactors   = FALSE
    )
  })
  do.call(rbind, rows)
}

.resolve_response_col <- function(response_col, model) {
  if (!is.null(response_col)) return(response_col)
  if (!is.null(model) && !is.null(model$formula)) {
    lhs <- all.vars(stats::update(model$formula, . ~ 0))
    if (length(lhs) > 0) return(lhs[1])
  }
  stop("Could not infer response column name. ",
       "Please supply response_col explicitly.")
}

# ******************************************************************************
# et_diagnose(): MCMC convergence + LOO diagnostics
# ______________________________________________________________________________

#' Diagnose a fitted ErrorTracer model
#'
#' Computes Rhat, effective sample size ratios, divergent transitions, and
#' leave-one-out cross-validation (LOO-CV) for a fitted \code{et_model}.
#'
#' @param model An \code{et_model} or \code{et_model_list}.
#' @param loo Logical.  Whether to run LOO-CV (can be slow; default
#'   \code{TRUE}).
#' @param ... Unused.
#' @return A list with elements:
#'   \describe{
#'     \item{convergence}{List: \code{rhat_max}, \code{rhat_all_ok},
#'       \code{neff_min}, \code{neff_all_ok}, \code{n_divergences}.}
#'     \item{loo}{List or \code{NULL}: \code{elpd_loo}, \code{p_loo},
#'       \code{looic}, \code{n_bad_pareto_k}, \code{loo_object}.}
#'     \item{summary}{Printed summary from \code{brms::summary()}.}
#'   }
#'   For \code{et_model_list}, a named list of per-group diagnostic lists
#'   plus an aggregated \code{summary} data.frame.
#' @export
et_diagnose <- function(model, loo = TRUE, ...) {
  UseMethod("et_diagnose")
}

#' @export
et_diagnose.et_model <- function(model, loo = TRUE, ...) {
  .diagnose_single(model$fit, loo = loo)
}

#' @export
et_diagnose.et_model_list <- function(model, loo = TRUE, ...) {
  diag_list <- lapply(names(model$models), function(g) {
    m <- model$models[[g]]
    if (is.null(m)) return(NULL)
    .diagnose_single(m$fit, loo = loo)
  })
  names(diag_list) <- names(model$models)

  # Build summary data.frame
  summary_df <- data.frame(
    group          = names(diag_list),
    rhat_ok        = vapply(diag_list, function(d) if (is.null(d)) NA else d$convergence$rhat_all_ok, logical(1)),
    neff_ok        = vapply(diag_list, function(d) if (is.null(d)) NA else d$convergence$neff_all_ok, logical(1)),
    n_divergences  = vapply(diag_list, function(d) if (is.null(d)) NA_real_ else d$convergence$n_divergences, numeric(1)),
    elpd_loo       = vapply(diag_list, function(d) if (is.null(d) || is.null(d$loo)) NA_real_ else d$loo$elpd_loo, numeric(1)),
    n_bad_pareto_k = vapply(diag_list, function(d) if (is.null(d) || is.null(d$loo)) NA_real_ else d$loo$n_bad_pareto_k, numeric(1)),
    stringsAsFactors = FALSE
  )

  list(per_group = diag_list, summary = summary_df)
}

.diagnose_single <- function(fit, loo) {
  rhat_vals  <- brms::rhat(fit)
  neff_vals  <- brms::neff_ratio(fit)
  nuts_df    <- brms::nuts_params(fit)
  divs       <- sum(nuts_df$Value[nuts_df$Parameter == "divergent__"], na.rm = TRUE)

  convergence <- list(
    rhat_max      = max(rhat_vals, na.rm = TRUE),
    rhat_all_ok   = all(rhat_vals < 1.05, na.rm = TRUE),
    neff_min      = min(neff_vals, na.rm = TRUE),
    neff_all_ok   = all(neff_vals > 0.1, na.rm = TRUE),
    n_divergences = divs
  )

  if (!convergence$rhat_all_ok) {
    .et_warn("Rhat > 1.05 for some parameters -- consider more iterations or ",
             "reparametrization.")
  }
  if (divs > 0) {
    .et_warn(divs, " divergent transitions -- consider increasing adapt_delta.")
  }

  loo_res <- NULL
  if (loo) {
    loo_res <- tryCatch({
      loo_obj <- brms::loo(fit)
      list(
        elpd_loo       = loo_obj$estimates["elpd_loo", "Estimate"],
        p_loo          = loo_obj$estimates["p_loo", "Estimate"],
        looic          = loo_obj$estimates["looic", "Estimate"],
        n_bad_pareto_k = sum(loo_obj$diagnostics$pareto_k > 0.7, na.rm = TRUE),
        loo_object     = loo_obj
      )
    }, error = function(e) {
      .et_warn("LOO-CV failed: ", e$message)
      NULL
    })
  }

  list(
    convergence = convergence,
    loo         = loo_res,
    summary     = summary(fit)
  )
}
