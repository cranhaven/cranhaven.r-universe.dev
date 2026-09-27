# R/glm-anova.R
#
# Design-based ANOVA for `survey_glm_fit` objects. Implements the Rao-Scott
# working-LRT and Wald-F/Chisq tests (cf. survey::anova.svyglm()) using the
# design-based variance already stored on `survey_glm_fit`. Saddlepoint CDFs
# are vendored in `R/variance-vendored-saddlepoint.R`.
#
# Public:
#   get_anova()                  -- user-facing analysis function
#   anova.survey_glm_fit()       -- S3 method, registered in zzz.R
#   print.survey_anova()         -- S3 print method
#
# Internal:
#   .reg_term_test()             -- one-term Rao-Scott / Wald test
#   .anova_sequential()          -- sequential term drop
#   .anova_compare()             -- nested-model comparison
#   .refit_drop_terms()          -- reduced-model refit helper
#   .anova_design_label()        -- header design-type rendering
#   .anova_design_df_string()    -- header design-df rendering
#   .anova_compose_term_labels() -- variable-label composition for `term`

# -- .refit_drop_terms() -------------------------------------------------------

# Drop `drop_terms` from `model@formula` and call `survey_glm()` on the
# original design. See spec sec. V.3.
.refit_drop_terms <- function(model, drop_terms) {
  drop_rhs <- paste(drop_terms, collapse = " - ")
  new_formula <- stats::update.formula(
    model@formula,
    stats::as.formula(paste(". ~ . -", drop_rhs))
  )

  family_obj <- if (inherits(model@family, "family")) {
    model@family
  } else {
    do.call(
      stats::family,
      list(model@family$family, link = model@family$link)
    )
  }

  # `model@call$na.action` is captured via match.call(); defaults are absent.
  # Fall back to stats::na.omit (the survey_glm default). If the user passed a
  # function, reuse it; otherwise rely on the default.
  na_action_raw <- model@call$na.action
  na_action <- if (is.function(na_action_raw)) {
    na_action_raw
  } else {
    stats::na.omit
  }

  reduced <- survey_glm(
    design = model@design,
    formula = new_formula,
    family = family_obj,
    na.action = na_action,
    quiet = TRUE
  )

  # A-20 defensive assertion on domain invariance.
  dom_col <- SURVEYCORE_DOMAIN_COL
  if (dom_col %in% names(model@design@data)) {
    if (
      !identical(
        model@design@data[[dom_col]],
        reduced@design@data[[dom_col]]
      )
    ) {
      cli::cli_abort(
        c(
          "x" = "Domain indicator differs between full and reduced fits for term {.field {paste(drop_terms, collapse = ', ')}}.",
          "i" = "This usually means {.code model@design@data} was mutated between fits.",
          "v" = "Do not modify the design between fits; pass the same design object to both {.fn survey_glm} calls."
        ),
        class = "surveycore_error_domain_mismatch"
      )
    }
  }
  reduced
}


# Moore-Penrose pseudoinverse via SVD. Pure base-R substitute for MASS::ginv()
# so we can avoid taking a MASS dependency for a single singular-V0 code path.
.ginv_svd <- function(X, tol = sqrt(.Machine$double.eps)) {
  s <- svd(X)
  pos <- s$d > max(tol * s$d[1L], 0)
  if (!any(pos)) {
    return(matrix(0, nrow = ncol(X), ncol = nrow(X)))
  }
  s$v[, pos, drop = FALSE] %*%
    (t(s$u[, pos, drop = FALSE]) / s$d[pos])
}


# -- .reg_term_test() ----------------------------------------------------------

# Analog of survey::regTermTest() for one term (or a set of terms supplied as
# a formula RHS, for comparison mode). Returns a named list with the fields
# described in sec. V.1 step 7. See spec sec. V.1 for full pseudocode.
.reg_term_test <- function(
  model,
  test_terms,
  method,
  test,
  null = NULL,
  ddf = NULL,
  reduced = NULL,
  tolerance = sqrt(.Machine$double.eps)
) {
  term_labels <- attr(stats::terms(model@formula), "term.labels")
  assign <- model@term_assign

  if (is.character(test_terms)) {
    these_labels <- test_terms
  } else {
    # test_terms is a formula / call; extract term labels
    tt_form <- stats::as.formula(
      paste0("~ ", paste(deparse(test_terms), collapse = " "))
    )
    these_labels <- attr(stats::terms(tt_form), "term.labels")
  }

  # Canonicalize: match on sorted interaction operands so `a:b` and `b:a` match.
  canon <- function(lbls) {
    vapply(
      strsplit(lbls, ":", fixed = TRUE),
      function(parts) paste(sort(trimws(parts)), collapse = ":"),
      character(1L)
    )
  }
  term_labels_c <- canon(term_labels)
  these_labels_c <- canon(these_labels)

  # Resolve positions in term_labels for each requested label. Intercept column
  # has assign == 0; k-th term column has assign == k.
  term_positions <- match(these_labels_c, term_labels_c)
  if (any(is.na(term_positions))) {
    cli::cli_abort(
      c(
        "x" = "Term(s) {.field {paste(these_labels[is.na(term_positions)], collapse = ', ')}} not found in model formula."
      ),
      class = "surveycore_error_term_not_in_model"
    )
  }
  idx <- which(assign %in% term_positions)
  q <- length(idx)

  term_label_for_msg <- paste(these_labels, collapse = " + ")

  # Null length check
  if (!is.null(null) && length(null) != q) {
    cli::cli_abort(
      c(
        "x" = "{.arg null} has length {length(null)} but {q} coefficient{?s} are tested for term {.field {term_label_for_msg}}.",
        "v" = "Supply one value per tested coefficient, or {.code null = NULL}."
      ),
      class = "surveycore_error_null_length_mismatch"
    )
  }

  # ddf handling
  if (is.null(ddf)) {
    p_total <- length(model@coefficients)
    ddf <- model@degf - (p_total - 1L)
  }

  beta <- model@coefficients[idx]
  V <- model@vcov[idx, idx, drop = FALSE]
  if (!is.null(null)) {
    beta <- beta - null
  }

  raw_chisq <- NA_real_
  lambda <- NULL
  p_value <- NA_real_
  statistic_val <- NA_real_
  deff_val <- NA_real_
  ddf_reported <- if (identical(test, "Chisq")) NA_real_ else ddf

  if (identical(method, "Wald")) {
    # Wald: beta' V^{-1} beta
    chisq <- tryCatch(
      as.numeric(t(beta) %*% solve(V, beta)),
      error = function(e) NA_real_
    )
    raw_chisq <- chisq
    if (identical(test, "F")) {
      statistic_val <- chisq / q
      p_value <- stats::pf(
        statistic_val,
        df1 = q,
        df2 = ddf,
        lower.tail = FALSE
      )
    } else {
      statistic_val <- chisq
      p_value <- stats::pchisq(chisq, df = q, lower.tail = FALSE)
    }
  } else {
    # LRT -- refit reduced model if not supplied.
    if (is.null(reduced)) {
      reduced <- .refit_drop_terms(model, test_terms)
    }

    # n-invariance check: row identifiers must match between full and reduced.
    full_rows <- rownames(stats::model.frame(model@fit_))
    red_rows <- rownames(stats::model.frame(reduced@fit_))
    if (!identical(full_rows, red_rows)) {
      cli::cli_abort(
        c(
          "x" = "Full model was fit on {length(full_rows)} row(s); reduced fit has {length(red_rows)}.",
          "i" = "LRT requires the two fits to cover the same rows -- usually this means a dropped term was the only source of NAs in some rows.",
          "v" = "Filter the design to rows complete on all predictors, then re-fit both models."
        ),
        class = "surveycore_error_n_mismatch"
      )
    }
    stopifnot(length(model@weights) == length(reduced@weights))

    # Replicate-nonconvergence surfacing (A-19)
    if (S7::S7_inherits(model@design, survey_replicate)) {
      rep_failures <- tryCatch(reduced@fit_$rep_failures, error = function(e) {
        NULL
      })
      if (is.null(rep_failures)) {
        # survey_glm() currently only emits A-19 via surveycore_warning_glm_convergence
        # at the replicate level; we don't re-raise at the ANOVA level when
        # the refit succeeded. This branch is a placeholder for when the
        # @rep_failures property lands on survey_glm_fit.
      }
    }

    # Rao-Scott working-LRT: the Rao-Scott chi-square is constructed on
    # the rescaled-weight scale (weights normalized so the mean weight is
    # 1 across the *full design*, i.e. before NA filtering -- matching
    # survey::svyglm()'s rescale=TRUE behavior). surveycore fits with raw
    # design weights, so recompute both deviances on the rescaled weight
    # scale via stats::glm.fit(). The normalization divisor is the mean
    # reciprocal probability over the entire design (including rows
    # dropped to NA), not the per-fit mean weight.
    design_weights_var <- model@design@variables$weights
    if (
      !is.null(design_weights_var) &&
        design_weights_var %in% names(model@design@data)
    ) {
      full_w <- model@design@data[[design_weights_var]]
      full_w <- full_w[is.finite(full_w) & full_w > 0]
      w_mean <- if (length(full_w) > 0) mean(full_w) else 1
    } else {
      w_mean <- mean(model@weights)
    }
    if (!is.finite(w_mean) || w_mean <= 0) {
      w_mean <- 1
    }

    w_full <- model@weights
    w_red <- reduced@weights

    dev_full_scaled <- tryCatch(
      {
        y <- stats::model.response(stats::model.frame(model@fit_))
        X <- stats::model.matrix(model@fit_)
        fam <- if (inherits(model@family, "family")) {
          model@family
        } else {
          do.call(
            stats::family,
            list(model@family$family, link = model@family$link)
          )
        }
        f <- stats::glm.fit(
          x = X,
          y = y,
          weights = w_full / w_mean,
          family = fam
        )
        f$deviance
      },
      error = function(e) model@deviance / w_mean
    )
    dev_red_scaled <- tryCatch(
      {
        y2 <- stats::model.response(stats::model.frame(reduced@fit_))
        X2 <- stats::model.matrix(reduced@fit_)
        fam2 <- if (inherits(reduced@family, "family")) {
          reduced@family
        } else {
          do.call(
            stats::family,
            list(reduced@family$family, link = reduced@family$link)
          )
        }
        f2 <- stats::glm.fit(
          x = X2,
          y = y2,
          weights = w_red / w_mean,
          family = fam2
        )
        f2$deviance
      },
      error = function(e) reduced@deviance / w_mean
    )
    # survey::regTermTest rescale factor: mean(model$prior.weights) /
    # mean(model0$prior.weights). Since both w_full and w_red are divided
    # by the same w_mean (full-design scale), the rescale factor is
    # mean(w_full) / mean(w_red).
    w_mean_full <- mean(w_full / w_mean)
    w_mean_red <- mean(w_red / w_mean)
    rescale_factor <- if (w_mean_red > 0) w_mean_full / w_mean_red else 1
    chisq <- dev_red_scaled * rescale_factor - dev_full_scaled
    neg_tol <- sqrt(.Machine$double.eps) * abs(dev_full_scaled)
    if (chisq < -neg_tol) {
      cli::cli_warn(
        c(
          "!" = "Deviance difference is negative for term {.field {term_label_for_msg}} ({.val {signif(chisq, 3)}}); the reduced fit may not have converged.",
          "i" = "Treating as 0 to keep the LRT well-defined.",
          "v" = "Inspect reduced-model convergence via {.fn summary} on a manual refit."
        ),
        class = "surveycore_warning_negative_deviance_diff"
      )
    }
    chisq <- max(chisq, 0)
    raw_chisq <- chisq

    # V0 block (q x q) from naive model-based covariance. survey_glm fits
    # with raw design weights so cov.unscaled is on the raw-weight scale --
    # multiply by mean(w) to put it on the normalized (mean-weight = 1)
    # scale that matches survey::regTermTest()'s V0.
    cov_unsc <- summary(model@fit_)$cov.unscaled * w_mean
    V0 <- cov_unsc[idx, idx, drop = FALSE]

    use_ginv <- FALSE
    rc <- tryCatch(rcond(V0), error = function(e) 0)
    if (!is.finite(rc) || rc < tolerance) {
      cli::cli_warn(
        c(
          "!" = "Naive covariance V\u2080 is near-singular for term {.field {term_label_for_msg}}; using pseudoinverse.",
          "i" = "Reciprocal condition number: {.val {signif(rc, 3)}} (threshold: {.val {signif(tolerance, 3)}}).",
          "v" = "Inspect the model for collinearity in the tested coefficients."
        ),
        class = "surveycore_warning_singular_v0"
      )
      use_ginv <- TRUE
    }

    lambda <- tryCatch(
      {
        if (use_ginv) {
          eigen(.ginv_svd(V0) %*% V, only.values = TRUE)$values
        } else {
          eigen(solve(V0) %*% V, only.values = TRUE)$values
        }
      },
      error = function(e) {
        # Fall through to chi^2(q) reference; saddlepoint helpers will return NaN
        # but callers expect finite lambda, so use a unit vector.
        rep(1, q)
      }
    )
    lambda <- Re(lambda)
    mean_lambda <- mean(lambda)

    # Statistic is the raw deviance difference (spec sec. 3.7.2)
    statistic_val <- chisq
    deff_val <- mean_lambda

    # Reference distribution
    if (identical(test, "Chisq")) {
      p_value <- .pchisqsum_sad(
        chisq,
        df = rep(1L, q),
        a = lambda,
        lower.tail = FALSE
      )
    } else {
      p_value <- .pFsum_sad(
        chisq / mean_lambda,
        df = rep(1L, q),
        a = lambda,
        ddf = ddf,
        mean.deff = mean_lambda,
        lower.tail = FALSE
      )
    }
  }

  list(
    term = term_label_for_msg,
    statistic = statistic_val,
    df = as.numeric(q),
    ddf = ddf_reported,
    deff = deff_val,
    p_value = p_value,
    raw_chisq = raw_chisq,
    lambda = lambda,
    ddf_used = ddf,
    test_terms = these_labels
  )
}


# -- .anova_sequential() -------------------------------------------------------

# Right-to-left drop each term in `model`. See spec sec. 3.4 / sec. V.2.
.anova_sequential <- function(model, method, test, null, tolerance) {
  tt <- attr(stats::terms(model@formula), "term.labels")
  nt <- length(tt)
  if (nt < 1L) {
    cli::cli_abort(
      c(
        "x" = "Model is intercept-only; there are no terms to test.",
        "i" = "Sequential ANOVA requires at least one non-intercept term in the formula.",
        "v" = "Fit a model with at least one predictor, e.g. {.code y ~ x}."
      ),
      class = "surveycore_error_no_terms_to_test"
    )
  }

  current <- model
  rows_rev <- vector("list", nt)
  for (i in seq.int(nt, 1L)) {
    this_term <- tt[[i]]
    row <- .reg_term_test(
      current,
      this_term,
      method = method,
      test = test,
      null = null,
      tolerance = tolerance
    )
    # restore the "pretty" label (single term, not multi-term composition)
    row$term <- this_term
    rows_rev[[nt - i + 1L]] <- row

    if (i > 1L && identical(method, "LRT")) {
      # Drop this term and continue loop with reduced as current
      current <- .refit_drop_terms(current, this_term)
    } else if (i > 1L) {
      # Wald path: we still need a `current` with one fewer term, but since
      # Wald tests are computed from @coefficients / @vcov (indexed via
      # @term_assign), we refit to keep the sequential chain consistent.
      current <- .refit_drop_terms(current, this_term)
    }
  }
  # Output in formula order (leftmost first).
  rev(rows_rev)
}


# -- .anova_compare() ----------------------------------------------------------

# Nested-comparison path. See spec sec. 3.5 / sec. V.4.
.anova_compare <- function(model, model2, method, test, null, tolerance) {
  t1 <- attr(stats::terms(model@formula), "term.labels")
  t2 <- attr(stats::terms(model2@formula), "term.labels")

  # Response invariance (A-3) -- check first so callers with divergent LHS
  # get a targeted error even if term sets match.
  lhs1 <- model@formula[[2L]]
  lhs2 <- model2@formula[[2L]]
  if (!identical(lhs1, lhs2)) {
    cli::cli_abort(
      c(
        "x" = "Models have different response variables.",
        "i" = "{.field {deparse(lhs1)}} vs. {.field {deparse(lhs2)}}."
      ),
      class = "surveycore_error_response_mismatch"
    )
  }

  # Identical term sets -> A-16.
  if (setequal(t1, t2)) {
    cli::cli_abort(
      c(
        "x" = "Both models have identical term sets; there are no terms to test.",
        "i" = "Comparison mode requires one model's terms to be a strict superset of the other's.",
        "v" = "Use {.code get_anova(model)} for sequential term tests, or fit one of the models with additional terms."
      ),
      class = "surveycore_error_identical_term_sets"
    )
  }

  # Symbolic nesting
  if (all(t1 %in% t2)) {
    bigger <- model2
    reduced <- model
    bigger_terms <- t2
    reduced_terms <- t1
  } else if (all(t2 %in% t1)) {
    bigger <- model
    reduced <- model2
    bigger_terms <- t1
    reduced_terms <- t2
  } else {
    cli::cli_abort(
      c(
        "x" = "Models are not symbolically nested.",
        "i" = "Neither model's term set is a subset of the other.",
        "v" = "Refit one model so its terms are a superset of the other's."
      ),
      class = "surveycore_error_models_not_nested"
    )
  }

  # Design invariance (A-5)
  if (
    !identical(model@design@data, model2@design@data) ||
      !identical(model@design@variables, model2@design@variables)
  ) {
    cli::cli_abort(
      c(
        "x" = "Models were fit on designs with different {.field @data} or {.field @variables} slots.",
        "i" = "Comparison mode requires both fits to reference semantically-identical designs (same rows, same design variables).",
        "v" = "Confirm the two designs wrap the same data frame and use the same {.code ids} / {.code weights} / {.code strata} / {.code fpc} columns; typically this means fitting both models against one stored design object."
      ),
      class = "surveycore_error_design_mismatch"
    )
  }

  # fit_ guard for n-invariance comparison (A-11 applies in comparison mode
  # regardless of method; see spec sec. 3.3.1 / sec. 3.5 step 5).
  if (is.null(model@fit_) || is.null(model2@fit_)) {
    cli::cli_abort(
      c(
        "x" = "Variance / n-invariance checks require the underlying GLM fit object(s), which are not persisted after serialization.",
        "i" = "Sequential LRT needs {.field model@fit_}. Comparison mode (both LRT and Wald) needs {.field model@fit_} and {.field model2@fit_} for the row-identity check.",
        "v" = "Re-fit the model(s) in the current R session. Sequential Wald is unaffected and remains serialization-safe."
      ),
      class = "surveycore_error_lrt_requires_fit_object"
    )
  }

  # n-invariance (A-12)
  full_rows <- rownames(stats::model.frame(bigger@fit_))
  red_rows <- rownames(stats::model.frame(reduced@fit_))
  if (!identical(full_rows, red_rows)) {
    cli::cli_abort(
      c(
        "x" = "Full model was fit on {length(full_rows)} row(s); reduced fit has {length(red_rows)}.",
        "i" = "Comparison requires the two fits to cover the same rows.",
        "v" = "Filter the design to rows complete on all predictors, then re-fit both models."
      ),
      class = "surveycore_error_n_mismatch"
    )
  }

  # Design df invariance (spec sec. 3.5 step 7)
  stopifnot(isTRUE(all.equal(bigger@degf, reduced@degf)))
  p_bigger <- length(bigger@coefficients)
  ddf <- bigger@degf - (p_bigger - 1L)

  added_terms <- setdiff(bigger_terms, reduced_terms)
  test_formula <- stats::reformulate(added_terms)[[2L]]

  row <- .reg_term_test(
    bigger,
    test_formula,
    method = method,
    test = test,
    null = null,
    ddf = ddf,
    reduced = reduced,
    tolerance = tolerance
  )
  # Compose comparison-mode term label: "added | base"
  base_str <- paste(reduced_terms, collapse = " + ")
  added_str <- paste(added_terms, collapse = " + ")
  row$term <- paste(added_str, "|", base_str)
  row$added_terms <- added_terms
  row$model2_fit <- reduced
  row$bigger_fit <- bigger
  list(row)
}


# -- Term-label composition for `label_vars` -----------------------------------

# Compose per-row label strings from stored variable labels. Returns a
# character vector of the same length as `terms`, or NULL if label_vars is
# FALSE or none of the terms have labels.
.anova_compose_term_labels <- function(design, terms, label_vars) {
  if (!isTRUE(label_vars)) {
    return(NULL)
  }
  vl <- design@metadata@variable_labels
  composed <- vapply(
    terms,
    function(tm) {
      if (grepl("|", tm, fixed = TRUE)) {
        return(NA_character_)
      }
      parts <- strsplit(tm, ":", fixed = TRUE)[[1L]]
      labs <- vl[parts]
      if (any(vapply(labs, is.null, logical(1L)))) {
        return(NA_character_)
      }
      paste(unlist(labs), collapse = " \u00d7 ")
    },
    character(1L)
  )
  composed
}


# -- Design label / df rendering helpers --------------------------------------

.anova_design_label <- function(design_type, replicate_type) {
  switch(
    design_type,
    taylor = "Taylor series",
    replicate = if (!is.na(replicate_type)) {
      sprintf("Replicate weights (%s)", replicate_type)
    } else {
      "Replicate weights"
    },
    twophase = "Two-phase",
    nonprob = "Non-probability",
    srs = "Simple random sample",
    design_type
  )
}

.anova_design_df_string <- function(x) {
  m <- attr(x, ".meta")
  if (nrow(x) == 0L) {
    return("NA")
  }
  ddf <- x$ddf[[1L]]
  if (is.na(ddf)) {
    # Fall back to the underlying design df from .meta$terms
    tm <- m$terms[[1L]]
    ddf <- tm$ddf_used
  }
  if (is.null(ddf) || length(ddf) == 0L || is.na(ddf)) {
    return("NA")
  }
  if (isTRUE(all.equal(ddf, round(ddf)))) {
    format(as.integer(round(ddf)), big.mark = ",")
  } else {
    format(round(ddf, 2), big.mark = ",", nsmall = 2L)
  }
}


# -- .get_anova_single() -------------------------------------------------------

# Internal single-fit / two-fit kernel. The exported `get_anova()` lives in
# `R/glm-anova-dispatch.R` and routes here after resolving the polymorphic
# `object` argument. Callers inject `call` so surface errors/metadata report
# the user-visible `get_anova()` call, not the internal helper.
.get_anova_single <- function(
  model,
  model2 = NULL,
  method = c("LRT", "Wald"),
  test = c("F", "Chisq"),
  null = NULL,
  tolerance = sqrt(.Machine$double.eps),
  decimals = NULL,
  label_vars = TRUE,
  name_style = "surveycore",
  call = NULL
) {
  cl <- call %||% match.call()

  # 1. Validate `model`
  if (!S7::S7_inherits(model, survey_glm_fit)) {
    cli::cli_abort(
      c(
        "x" = "{.arg model} must be a {.cls survey_glm_fit} object, not {.cls {class(model)[1]}}."
      ),
      class = "surveycore_error_not_glm_fit",
      call = cl
    )
  }

  # 2. Validate `model2` (if supplied)
  if (!is.null(model2) && !S7::S7_inherits(model2, survey_glm_fit)) {
    cli::cli_abort(
      c(
        "x" = "{.arg model2} must be a {.cls survey_glm_fit} object, not {.cls {class(model2)[1]}}."
      ),
      class = "surveycore_error_not_glm_fit",
      call = cl
    )
  }

  # 3. Validate `method` / `test`
  method <- match.arg(method)
  test <- match.arg(test)

  # 4. Validate `null` in LRT mode
  if (!is.null(null) && identical(method, "LRT")) {
    cli::cli_abort(
      c(
        "x" = "{.arg null} is only valid with {.code method = \"Wald\"}.",
        "v" = "Set {.code method = \"Wald\"} to test a non-zero null hypothesis."
      ),
      class = "surveycore_error_null_with_lrt",
      call = cl
    )
  }
  if (!is.null(null) && !is.numeric(null)) {
    cli::cli_abort(
      c(
        "x" = "{.arg null} must be numeric or {.code NULL}.",
        "i" = "Got {.cls {class(null)[1]}}."
      ),
      class = "surveycore_error_null_length_mismatch",
      call = cl
    )
  }

  # 5. LRT serialization guard (sequential) -- comparison path guarded in .anova_compare()
  if (
    is.null(model2) &&
      identical(method, "LRT") &&
      is.null(model@fit_)
  ) {
    cli::cli_abort(
      c(
        "x" = "Variance / n-invariance checks require the underlying GLM fit object(s), which are not persisted after serialization.",
        "i" = "Sequential LRT needs {.field model@fit_}.",
        "v" = "Re-fit the model in the current R session. Sequential Wald is unaffected and remains serialization-safe."
      ),
      class = "surveycore_error_lrt_requires_fit_object",
      call = cl
    )
  }

  # 6. Insufficient-df guard (sequential path only -- comparison uses bigger@degf)
  if (is.null(model2)) {
    p <- length(model@coefficients)
    ddf_raw <- model@degf - (p - 1L)
    if (ddf_raw < 1) {
      cli::cli_abort(
        c(
          "x" = "Design has {.val {round(ddf_raw, 2)}} residual degree{?s} of freedom for a model with {p} coefficient{?s}; at least 1 is required.",
          "i" = "The reference distribution cannot be calibrated at this ddf and the p-value is not well-defined for either {.val F} or {.val Chisq} tests.",
          "v" = "Fit a simpler model (fewer coefficients) or use a design with more primary sampling units."
        ),
        class = "surveycore_error_insufficient_df_for_anova",
        call = cl
      )
    }
  }

  # 6a. Nonprob inference notice
  if (S7::S7_inherits(model@design, survey_nonprob)) {
    cli::cli_warn(
      c(
        "!" = "Variance for nonprob designs is model-based; inference assumes the fitted model is correctly specified.",
        "i" = "ANOVA p-values do not account for selection bias.",
        "v" = "Treat results as model-based inference; for design-based ANOVA use a probability-sample design."
      ),
      class = "surveycore_warning_nonprob_inference"
    )
  }

  # 7. Validate decimals / name_style
  .validate_decimals_namestyle(decimals, name_style)

  # 7a. Validate tolerance
  if (
    !is.numeric(tolerance) ||
      length(tolerance) != 1L ||
      !is.finite(tolerance) ||
      tolerance < 0
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg tolerance} must be a single finite non-negative numeric value.",
        "i" = "Got {.cls {class(tolerance)[1]}} of length {length(tolerance)}: {.val {tolerance}}.",
        "v" = "Use the default {.code sqrt(.Machine$double.eps)}, or supply any {.val 0}-or-positive numeric scalar."
      ),
      class = "surveycore_error_invalid_tolerance",
      call = cl
    )
  }

  # 8. Mode dispatch
  if (is.null(model2)) {
    rows <- .anova_sequential(
      model,
      method = method,
      test = test,
      null = null,
      tolerance = tolerance
    )
  } else {
    rows <- .anova_compare(
      model,
      model2,
      method = method,
      test = test,
      null = null,
      tolerance = tolerance
    )
  }

  # Stack rows into columns
  term_vec <- vapply(rows, function(r) r$term, character(1L))
  statistic_vec <- vapply(
    rows,
    function(r) as.numeric(r$statistic),
    numeric(1L)
  )
  df_vec <- vapply(rows, function(r) as.numeric(r$df), numeric(1L))
  ddf_vec <- vapply(
    rows,
    function(r) as.numeric(r$ddf %||% NA_real_),
    numeric(1L)
  )
  deff_vec <- vapply(
    rows,
    function(r) as.numeric(r$deff %||% NA_real_),
    numeric(1L)
  )
  p_vec <- vapply(rows, function(r) as.numeric(r$p_value), numeric(1L))
  stars_vec <- .stars_pval(p_vec)

  result <- tibble::tibble(
    term = term_vec,
    statistic = statistic_vec,
    df = df_vec,
    ddf = ddf_vec,
    deff = deff_vec,
    p_value = p_vec,
    stars = stars_vec
  )

  # Apply decimals (manual loop -- .apply_decimals would need class set first,
  # but we apply it here before class/meta attachment)
  if (!is.null(decimals)) {
    for (j in seq_along(result)) {
      if (is.double(result[[j]])) {
        result[[j]] <- round(result[[j]], decimals)
      }
    }
  }

  # Apply class
  result_class <- c(
    "survey_anova",
    "survey_result",
    "tbl_df",
    "tbl",
    "data.frame"
  )
  class(result) <- result_class

  # Column-level labels
  stat_label <- switch(
    paste(method, test),
    "Wald F" = "Wald-F",
    "Wald Chisq" = "Wald-\u03c7\u00b2",
    "LRT F" = "LRT-F",
    "LRT Chisq" = "LRT-\u03c7\u00b2",
    "Statistic"
  )
  col_labels <- list(
    term = "Term",
    statistic = stat_label,
    df = "df",
    ddf = "Denom df",
    deff = "DEff",
    p_value = "P-Value",
    stars = ""
  )

  # Apply name_style (renames p_value, ddf)
  if (identical(name_style, "broom")) {
    nm <- names(result)
    nm[nm == "p_value"] <- "p.value"
    nm[nm == "ddf"] <- "df_residual"
    names(result) <- nm
    # Preserve labels across rename
    col_labels$p.value <- col_labels$p_value
    col_labels$df_residual <- col_labels$ddf
  }

  for (nm in names(result)) {
    lbl <- col_labels[[nm]]
    if (!is.null(lbl)) {
      attr(result[[nm]], "label") <- lbl
    }
  }
  class(result) <- result_class

  # label_vars composition -- attach per-row labels to individual term cells
  if (isTRUE(label_vars)) {
    design_src <- model@design
    composed <- .anova_compose_term_labels(
      design_src,
      term_vec,
      label_vars = TRUE
    )
    if (!is.null(composed)) {
      # Store composed labels in a parallel attribute; cell-level attr() on
      # scalar character elements requires wrapping -- we attach as a named
      # list on the `term` column.
      non_na <- which(!is.na(composed))
      if (length(non_na) > 0L) {
        composed_list <- stats::setNames(
          as.list(composed[non_na]),
          term_vec[non_na]
        )
        # Attach parallel "labels_by_row" attribute for programmatic access.
        labels_by_row <- vector("list", length(term_vec))
        for (i in non_na) {
          labels_by_row[[i]] <- composed[[i]]
        }
        attr(result$term, "labels_by_row") <- labels_by_row
        # Column-level label is "Term"; per-cell labels live on `labels_by_row`.
      }
    }
  }

  # Assemble .meta
  design_type <-
    if (S7::S7_inherits(model@design, survey_taylor)) {
      # SRS is represented as survey_taylor with no ids/strata
      if (
        is.null(model@design@variables$ids) &&
          is.null(model@design@variables$strata)
      ) {
        "srs"
      } else {
        "taylor"
      }
    } else if (S7::S7_inherits(model@design, survey_replicate)) {
      "replicate"
    } else if (S7::S7_inherits(model@design, survey_twophase)) {
      "twophase"
    } else if (S7::S7_inherits(model@design, survey_nonprob)) {
      "nonprob"
    } else {
      "unknown"
    }

  replicate_type <- if (identical(design_type, "replicate")) {
    rt <- model@design@variables$type
    if (is.null(rt) || length(rt) == 0L) NA_character_ else as.character(rt)
  } else {
    NA_character_
  }

  fam_name <- if (inherits(model@family, "family")) {
    model@family$family
  } else {
    model@family$family %||% NA_character_
  }
  link_name <- if (inherits(model@family, "family")) {
    model@family$link
  } else {
    model@family$link %||% NA_character_
  }

  model_meta <- list(
    formula = model@formula,
    family = fam_name,
    link = link_name,
    n_obs = as.integer(length(model@fitted_values)),
    coefficients = names(model@coefficients)
  )

  terms_meta <- lapply(rows, function(r) {
    sub <- list(
      raw_chisq = r$raw_chisq,
      lambda = r$lambda,
      ddf_used = r$ddf_used,
      test_terms = r$test_terms
    )
    if (!is.null(r$added_terms)) {
      sub$added_terms <- r$added_terms
    }
    sub
  })

  model2_meta <- NULL
  if (!is.null(model2)) {
    fam2_name <- if (inherits(model2@family, "family")) {
      model2@family$family
    } else {
      model2@family$family %||% NA_character_
    }
    link2_name <- if (inherits(model2@family, "family")) {
      model2@family$link
    } else {
      model2@family$link %||% NA_character_
    }
    model2_meta <- list(
      formula = model2@formula,
      family = fam2_name,
      link = link2_name,
      n_obs = as.integer(length(model2@fitted_values)),
      coefficients = names(model2@coefficients)
    )
  }

  meta_args <- list(
    call = cl,
    model = model_meta,
    method = method,
    test = test,
    terms = terms_meta,
    replicate_type = replicate_type
  )
  if (!is.null(model2_meta)) {
    meta_args$model2 <- model2_meta
  }

  full_meta <- c(
    list(
      design_type = design_type,
      n_respondents = as.integer(nrow(model@design@data))
    ),
    meta_args
  )
  stopifnot(all(ANOVA_META_KEYS %in% names(meta_args)))

  attr(result, ".meta") <- full_meta
  class(result) <- result_class

  result
}


# -- anova.survey_glm_fit() ----------------------------------------------------

#' ANOVA Method for Survey GLM Fits
#'
#' S3 method that dispatches to [get_anova()]. Pass one or two
#' [survey_glm_fit] objects; the single-model or pairwise path is chosen
#' automatically.
#'
#' @param object A [survey_glm_fit] object.
#' @param ... An optional second [survey_glm_fit] for pairwise comparison;
#'   anything else errors.
#' @param method Character(1). `"LRT"` (default) or `"Wald"`.
#' @param test Character(1). `"F"` (default) or `"Chisq"`.
#' @param null Numeric or `NULL`. Hypothesized coefficient value (Wald only).
#' @return A `survey_anova` tibble; see [get_anova()] for column details.
#' @method anova survey_glm_fit
#' @export
anova.survey_glm_fit <- function(
  object,
  ...,
  method = "LRT",
  test = "F",
  null = NULL
) {
  others <- list(...)
  if (length(others) == 0L) {
    get_anova(
      object,
      method = method,
      test = test,
      null = null
    )
  } else if (
    length(others) == 1L &&
      S7::S7_inherits(others[[1L]], survey_glm_fit)
  ) {
    get_anova(
      list(object, others[[1L]]),
      method = method,
      test = test,
      null = null
    )
  } else {
    cli::cli_abort(
      c(
        "x" = "anova() on {.cls survey_glm_fit} accepts at most one additional {.cls survey_glm_fit} model.",
        "i" = "Got {length(others)} extra argument(s)."
      ),
      class = "surveycore_error_anova_bad_dots"
    )
  }
}


# -- print.survey_anova() -----------------------------------------------------

#' Print Method for survey_anova Objects
#'
#' @param x A `survey_anova` tibble produced by [get_anova()].
#' @param ... Additional arguments (currently unused).
#' @return `x`, invisibly.
#' @method print survey_anova
#' @export
print.survey_anova <- function(x, ...) {
  m <- attr(x, ".meta")

  method_label <- switch(
    m$method,
    LRT = "Rao-Scott LRT",
    Wald = "Design-based Wald",
    m$method
  )
  test_label <- switch(
    m$test,
    F = "F reference",
    Chisq = "Chi-sq reference",
    m$test
  )
  cat(sprintf(
    "# A survey_anova result (%s, %s)\n",
    method_label,
    test_label
  ))
  cat(sprintf("# Model: %s\n", deparse1(m$model$formula)))

  design_label <- .anova_design_label(m$design_type, m$replicate_type)
  n_fmt <- format(m$n_respondents, big.mark = ",")
  ddf_str <- .anova_design_df_string(x)
  cat(sprintf(
    "# Design: %s | N: %s | Design df: %s\n",
    design_label,
    n_fmt,
    ddf_str
  ))

  if (identical(m$method, "Wald")) {
    keep <- setdiff(names(x), "deff")
    x_print <- x[, keep, drop = FALSE]
    attr(x_print, ".meta") <- attr(x, ".meta")
    class(x_print) <- class(x)
    x <- x_print
  }

  # Explicit dispatch to tbl_df -- NOT NextMethod(); see sec. 3.9 pseudocode.
  class(x) <- setdiff(class(x), c("survey_anova", "survey_result"))
  print(x, ...)
  invisible(x)
}
