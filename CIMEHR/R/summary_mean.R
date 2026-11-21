.fit_summary_mean <- function(
    d, focal = "Z", id = "id", time = "time", outcome = "Y",
    obs_indicator = "R", visit_covs = c("Z", "X"),
    long_covs = c("Z", "X"), random_covs = "Z") {

  empty_coef <- function() data.frame(
    term = character(), estimate = numeric(), se = numeric(),
    se_model = numeric(), statistic = numeric(), p_value = numeric(),
    row.names = NULL, check.names = FALSE
  )
  failure <- function(message, warnings = character(), diagnostics = list()) {
    base_diag <- list(
      ok = FALSE, converged = FALSE, identified = FALSE,
      fixed_rank = NA_integer_, fixed_columns = NA_integer_,
      n_subjects_input = NA_integer_, n_subjects_analysis = 0L,
      n_rows_input = if (is.data.frame(d)) nrow(d) else NA_integer_,
      n_rows_observed = 0L, warnings = unique(warnings), message = message,
      model_vcov = NULL, hc3_vcov = NULL
    )
    base_diag[names(diagnostics)] <- diagnostics
    list(
      method_key = "summary_mean", method = "Summary-Mean",
      estimand = paste0(
        "OLS linear projection of each subject's mean observed outcome on ",
        "the subject-level mean covariates, conditional on at least one observed outcome"
      ),
      coefficients = empty_coef(), vcov = NULL, se = numeric(),
      vcov_type = "HC3", fit = NULL, diagnostics = base_diag,
      assumptions = c(
        "R == 1 denotes an observed outcome",
        "subjects are independent",
        "the reported coefficient targets a subject-summary projection, not the longitudinal mean coefficient",
        "HC3 is used for heteroskedasticity-robust inference"
      )
    )
  }

  d <- tryCatch(as.data.frame(d), error = function(e) NULL)
  if (is.null(d)) return(failure("`d` cannot be converted to a data.frame."))
  long_covs <- unique(as.character(long_covs))
  required <- unique(c(id, outcome, obs_indicator, long_covs))
  missing_cols <- setdiff(required, names(d))
  if (length(missing_cols)) {
    return(failure(paste("Missing required column(s):", paste(missing_cols, collapse = ", "))))
  }
  if (!is.numeric(d[[outcome]])) return(failure("The outcome column must be numeric."))
  nonnumeric <- long_covs[!vapply(d[long_covs], is.numeric, logical(1))]
  if (length(nonnumeric)) {
    return(failure(paste(
      "Summary covariates must be numeric:", paste(nonnumeric, collapse = ", ")
    )))
  }

  n_subjects_input <- length(unique(d[[id]][!is.na(d[[id]])]))
  observed <- !is.na(d[[id]]) & !is.na(d[[obs_indicator]]) &
    d[[obs_indicator]] == 1 & is.finite(d[[outcome]])
  obs <- d[observed, unique(c(id, outcome, long_covs)), drop = FALSE]
  if (!nrow(obs)) {
    return(failure("No rows satisfy R == 1 with a finite outcome.", diagnostics = list(
      n_subjects_input = n_subjects_input, n_rows_input = nrow(d)
    )))
  }

  plain <- vapply(obs[unique(c(outcome, long_covs))], function(x)
    (is.double(x) || is.integer(x)) && is.null(attributes(x)), logical(1))
  if (all(plain)) {
    ids <- obs[[id]]
    uid <- unique(ids)
    key <- as.character(uid)
  }
  if (all(plain) && !anyNA(key)) {
    key_levels <- unique(key[order(key)])
    code <- match(key, key_levels)[match(ids, uid)]
    summaries <- plm_subject_summary(obs[[outcome]], obs[long_covs], code, length(key_levels), 0L)
    dimnames(summaries) <- list(if (any(nzchar(key_levels))) key_levels, c(".summary", long_covs))
    subject_data <- as.data.frame(summaries, check.names = FALSE)
  } else {
    groups <- split(seq_len(nrow(obs)), as.character(obs[[id]]), drop = TRUE)
    rows <- lapply(groups, function(ii) {
      g <- obs[ii, , drop = FALSE]
      ans <- c(.summary = mean(g[[outcome]], na.rm = TRUE))
      if (length(long_covs)) {
        ans <- c(ans, stats::setNames(vapply(long_covs, function(v) {
          x <- g[[v]]
          x <- x[is.finite(x)]
          if (length(x)) mean(x) else NA_real_
        }, numeric(1)), long_covs))
      }
      ans
    })
    subject_data <- as.data.frame(do.call(rbind, rows), check.names = FALSE)
  }
  subject_data <- subject_data[stats::complete.cases(subject_data), , drop = FALSE]
  if (!nrow(subject_data)) {
    return(failure("No complete subject-level summaries remain.", diagnostics = list(
      n_subjects_input = n_subjects_input, n_rows_input = nrow(d),
      n_rows_observed = nrow(obs)
    )))
  }

  fixed_formula <- stats::reformulate(long_covs, response = ".summary")
  caught_warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      stats::lm(fixed_formula, data = subject_data),
      warning = function(w) {
        caught_warnings <<- c(caught_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(failure(conditionMessage(fit), caught_warnings, list(
      n_subjects_input = n_subjects_input,
      n_subjects_analysis = nrow(subject_data), n_rows_input = nrow(d),
      n_rows_observed = nrow(obs)
    )))
  }

  X <- stats::model.matrix(fit)
  fixed_rank <- fit$rank
  full_rank <- identical(as.integer(fixed_rank), as.integer(ncol(X)))
  beta <- stats::coef(fit)
  V_model <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  V_hc3 <- matrix(NA_real_, nrow = length(beta), ncol = length(beta),
                  dimnames = list(names(beta), names(beta)))
  if (full_rank && nrow(X) > ncol(X)) {
    bread <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
    if (!is.null(bread)) {
      leverage <- stats::hatvalues(fit)
      adjusted_sq <- (stats::residuals(fit) /
                        pmax(1 - leverage, sqrt(.Machine$double.eps)))^2
      meat <- crossprod(X, X * adjusted_sq)
      V_hc3 <- bread %*% meat %*% bread
      dimnames(V_hc3) <- list(colnames(X), colnames(X))
    }
  }
  hc3_ok <- all(is.finite(diag(V_hc3)))
  primary_vcov <- if (hc3_ok) V_hc3 else V_model
  vcov_type <- if (hc3_ok) "HC3" else "model-based fallback (HC3 unavailable)"
  primary_se <- if (is.null(primary_vcov)) {
    stats::setNames(rep(NA_real_, length(beta)), names(beta))
  } else {
    stats::setNames(sqrt(pmax(diag(primary_vcov), 0)), rownames(primary_vcov))
  }
  model_se <- if (is.null(V_model)) rep(NA_real_, length(beta)) else sqrt(pmax(diag(V_model), 0))
  names(model_se) <- names(beta)
  se_aligned <- primary_se[names(beta)]
  coef_table <- data.frame(
    term = names(beta), estimate = unname(beta), se = unname(se_aligned),
    se_model = unname(model_se[names(beta)]),
    statistic = unname(beta / se_aligned),
    p_value = unname(2 * stats::pnorm(-abs(beta / se_aligned))),
    row.names = NULL, check.names = FALSE
  )
  focal_ok <- focal %in% coef_table$term &&
    is.finite(coef_table$estimate[match(focal, coef_table$term)])
  if (!hc3_ok) caught_warnings <- c(caught_warnings, "HC3 unavailable; model-based vcov used.")

  list(
    method_key = "summary_mean", method = "Summary-Mean",
    estimand = paste0(
      "OLS linear projection of each subject's mean observed outcome on ",
      "the subject-level mean covariates, conditional on at least one observed outcome"
    ),
    coefficients = coef_table, vcov = primary_vcov, se = primary_se,
    vcov_type = vcov_type, fit = fit,
    diagnostics = list(
      ok = isTRUE(focal_ok), converged = TRUE, identified = full_rank,
      fixed_rank = fixed_rank, fixed_columns = ncol(X),
      n_subjects_input = n_subjects_input,
      n_subjects_analysis = nrow(subject_data), n_rows_input = nrow(d),
      n_rows_observed = nrow(obs), warnings = unique(caught_warnings),
      message = if (full_rank) "" else "Rank-deficient subject-level regression.",
      model_vcov = V_model, hc3_vcov = V_hc3
    ),
    assumptions = c(
      "R == 1 denotes an observed outcome",
      "subjects are independent",
      "the reported coefficient targets a subject-summary projection, not the longitudinal mean coefficient",
      "HC3 is used for heteroskedasticity-robust inference"
    )
  )
}

Summary_stat <- function(data,stat=c('mean','median','min','max'),...) {
  stat<-match.arg(stat);fit_method(method=c(min='summary_min',mean='summary_mean',median='summary_median',max='summary_max')[stat],data=data,...)
}
