# R/analysis-diffs-helpers.R
#
# Internal helpers for get_diffs(). Single-use helpers specific to
# get_diffs() live here; shared helpers used by multiple analysis
# functions live in R/analysis-helpers.R.
#
# Contents:
#   .stars_pval()               — significance stars from p-values
#   .extract_clean_estimates()  — clean-path estimate extraction
#   .extract_me_estimates()     — ME-path estimate extraction
#   .build_diffs_output()       — unified output row builder

# -- .stars_pval() ------------------------------------------------------------
#
# Convert p-values to significance star annotations.
#
# Cutpoints (strict inequality — equals goes to the less-significant bucket):
#   p < 0.001  -> "***"
#   p < 0.01   -> "**"
#   p < 0.05   -> "*"
#   p < 0.1    -> "."
#   p >= 0.1   -> ""
#   NA         -> ""
#
# @param p Numeric vector of p-values.
# @return Character vector of the same length as p.
.stars_pval <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 0.001,
      "***",
      ifelse(
        p < 0.01,
        "**",
        ifelse(
          p < 0.05,
          "*",
          ifelse(p < 0.1, ".", "")
        )
      )
    )
  )
}

# -- .extract_clean_estimates() -----------------------------------------------
#
# Extract treatment-level estimates, SEs, CIs, p-values, and level means
# from the direct-coefficient (clean) path.
#
# @param fit survey_glm_fit. Fitted survey GLM model.
# @param treats_name character(1). Name of the treatment variable column.
# @param conf_level numeric(1). Confidence level in (0, 1).
#
# @return Named list with 10 keys:
#   result_levels, result_estimates, result_ses, result_ci_lows,
#   result_ci_highs, result_p_values, result_means, result_groups,
#   reference_mean, preds_df
.extract_clean_estimates <- function(fit, treats_name, conf_level) {
  tidy_result <- clean(
    fit,
    conf_level = conf_level,
    include_reference = TRUE,
    n = TRUE,
    statistic = FALSE
  )

  # Identify the intercept row
  intercept_mask <- tidy_result$term == "(Intercept)"
  if (sum(intercept_mask) != 1L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Reference row not found in model output. ",
          "Expected exactly one intercept row."
        )
      ),
      class = "surveycore_error_reference_row_not_found"
    )
  }
  reference_mean <- tidy_result$estimate[intercept_mask]

  # Treatment rows: not intercept, not the reference-level placeholder row
  treat_mask <- !intercept_mask & !tidy_result$reference_row
  treat_df <- tidy_result[treat_mask, , drop = FALSE]

  # Strip the variable name prefix to recover bare level names
  result_levels <- vapply(
    treat_df$term,
    function(t) sub(paste0("^", treats_name), "", t),
    character(1L),
    USE.NAMES = FALSE
  )

  result_estimates <- treat_df$estimate
  result_ses <- treat_df$std_error
  result_ci_lows <- treat_df$conf_low
  result_ci_highs <- treat_df$conf_high
  result_p_values <- treat_df$p_value
  result_means <- reference_mean + result_estimates

  list(
    result_levels = result_levels,
    result_estimates = result_estimates,
    result_ses = result_ses,
    result_ci_lows = result_ci_lows,
    result_ci_highs = result_ci_highs,
    result_p_values = result_p_values,
    result_means = result_means,
    result_groups = NULL,
    reference_mean = reference_mean,
    preds_df = NULL
  )
}

# -- .extract_me_estimates() --------------------------------------------------
#
# Extract treatment-level estimates and level means via
# marginaleffects::avg_slopes() and marginaleffects::avg_predictions().
# Handles both the grouped and non-grouped cases.
#
# @param fit survey_glm_fit. Fitted survey GLM model.
# @param treats_name character(1). Treatment variable column name.
# @param group_names character. Group variable names; character(0) for none.
# @param ref_level character(1). Reference level name.
# @param scale character(1). "ame" or "link".
# @param suppress_mean logical(1). If TRUE, skip avg_predictions() call.
#
# @return Named list with 10 keys matching .extract_clean_estimates() return.
.extract_me_estimates <- function(
  fit,
  treats_name,
  group_names,
  ref_level,
  scale,
  suppress_mean
) {
  p <- length(stats::coef(fit))
  res_df <- max(1, fit@degf - (p - 1L))
  me_type <- if (scale == "link") "link" else "response"
  has_group <- length(group_names) > 0L

  # avg_slopes — by argument genuinely absent in the no-group case
  if (has_group) {
    slopes <- marginaleffects::avg_slopes(
      fit,
      variables = treats_name,
      by = group_names,
      type = me_type,
      wts = fit@weights,
      df = res_df
    )
  } else {
    slopes <- marginaleffects::avg_slopes(
      fit,
      variables = treats_name,
      type = me_type,
      wts = fit@weights,
      df = res_df
    )
  }
  slopes_df <- as.data.frame(slopes)

  # avg_predictions — by argument always present (treats_name is required)
  preds_df <- NULL
  if (!suppress_mean) {
    if (has_group) {
      preds <- marginaleffects::avg_predictions(
        fit,
        by = c(treats_name, group_names),
        type = me_type,
        wts = fit@weights,
        df = res_df
      )
    } else {
      preds <- marginaleffects::avg_predictions(
        fit,
        by = treats_name,
        type = me_type,
        wts = fit@weights,
        df = res_df
      )
    }
    preds_df <- as.data.frame(preds)
  }

  # Parse level names from "X - ref" contrast format.
  # Use fixed = TRUE so ref_level metacharacters (e.g. "(Control)", "A.B")
  # are treated as literal strings, not regex patterns.
  ref_suffix <- paste0(" - ", ref_level)
  result_levels <- sub(
    ref_suffix,
    "",
    as.character(slopes_df$contrast),
    fixed = TRUE
  )
  result_estimates <- slopes_df$estimate
  result_ses <- slopes_df$std.error
  result_ci_lows <- slopes_df$conf.low
  result_ci_highs <- slopes_df$conf.high
  result_p_values <- slopes_df$p.value

  # Group columns from slopes output
  result_groups <- if (has_group) {
    slopes_df[, group_names, drop = FALSE]
  } else {
    NULL
  }

  # Populate result_means
  if (is.null(preds_df)) {
    result_means <- NULL
  } else if (!has_group) {
    # No-group: look up means by level name
    pred_means <- stats::setNames(
      preds_df$estimate,
      as.character(preds_df[[treats_name]])
    )
    result_means <- unname(pred_means[result_levels])
  } else {
    # With groups: composite-key lookup
    result_means <- numeric(nrow(slopes_df))
    for (i in seq_len(nrow(slopes_df))) {
      lvl <- result_levels[[i]]
      pred_match <- preds_df[[treats_name]] == lvl
      for (gn in group_names) {
        pred_match <- pred_match &
          (preds_df[[gn]] == slopes_df[[gn]][[i]])
      }
      result_means[[i]] <- preds_df$estimate[pred_match][[1L]]
    }
  }

  list(
    result_levels = result_levels,
    result_estimates = result_estimates,
    result_ses = result_ses,
    result_ci_lows = result_ci_lows,
    result_ci_highs = result_ci_highs,
    result_p_values = result_p_values,
    result_means = result_means,
    result_groups = result_groups,
    reference_mean = NULL,
    preds_df = preds_df
  )
}

# -- .build_diffs_output() ----------------------------------------------------
#
# Unified replacement for the duplicated non-group and grouped branches of
# Steps 12-17 in get_diffs(). Builds reference and treatment rows, checks
# cell sizes, adjusts p-values, computes stars and pct_change, and assembles
# the final col_vecs + groups_df.
#
# @param result Named list from .extract_clean_estimates() or
#   .extract_me_estimates().
# @param design survey_base. The survey design object.
# @param treats_name character(1). Treatment variable column name.
# @param group_names character. Group variable names; character(0) for none.
# @param ref_level character(1). Reference level name.
# @param domain_mask logical. Row mask from .apply_domain(design).
# @param wt_var character(1) or NULL. Weight column name.
# @param show_means logical(1). Include reference row and mean column.
# @param use_marginaleffects logical(1). Which estimation path was used.
# @param pval_adj character(1) or NULL. P-value adjustment method.
# @param show_pct_change logical(1). Include pct_change column.
# @param suppress_mean logical(1). Omit mean-related columns.
# @param min_cell_n integer(1). Small-cell warning threshold.
# @param variance character or NULL. Which uncertainty columns to include.
# @param n_weighted logical(1). Include n_weighted column.
#
# @return Named list: list(col_vecs = list, groups_df = data.frame)
.build_diffs_output <- function(
  result,
  design,
  treats_name,
  group_names,
  ref_level,
  domain_mask,
  wt_var,
  show_means,
  use_marginaleffects,
  pval_adj,
  show_pct_change,
  suppress_mean,
  min_cell_n,
  variance,
  n_weighted
) {
  has_group <- length(group_names) > 0L

  # Determine unique group combinations to iterate over.
  # When no groups, use a single-row sentinel (NULL marks no group filtering).
  if (!has_group) {
    # One implicit group — treat entire dataset as one combo
    unique_group_combos <- NULL
    n_combos <- 1L
  } else {
    unique_group_combos <- unique(
      result$result_groups[, group_names, drop = FALSE]
    )
    n_combos <- nrow(unique_group_combos)
  }

  all_rows <- list()
  all_group_rows <- list()

  for (gi in seq_len(n_combos)) {
    if (has_group) {
      gc <- unique_group_combos[gi, , drop = FALSE]
    } else {
      gc <- NULL
    }

    # ── Reference row ────────────────────────────────────────────────────────
    if (show_means) {
      ref_mask <- domain_mask &
        (design@data[[treats_name]] == ref_level) &
        !is.na(design@data[[treats_name]])
      if (has_group) {
        for (gn in group_names) {
          ref_mask <- ref_mask &
            !is.na(design@data[[gn]]) &
            (design@data[[gn]] == gc[[gn]])
        }
      }
      ref_n <- sum(ref_mask)
      if (!is.null(wt_var)) {
        ref_nw <- sum(
          design@data[[wt_var]][ref_mask],
          na.rm = TRUE
        )
      } else {
        ref_nw <- as.numeric(ref_n)
      }

      # Reference group mean
      if (suppress_mean) {
        ref_mean <- NA_real_
      } else if (!use_marginaleffects) {
        ref_mean <- result$reference_mean
      } else if (!is.null(result$preds_df)) {
        pred_match <- result$preds_df[[treats_name]] == ref_level
        if (has_group) {
          for (gn in group_names) {
            pred_match <- pred_match &
              (result$preds_df[[gn]] == gc[[gn]])
          }
        }
        ref_mean <- result$preds_df$estimate[pred_match][[1L]]
      } else {
        # nocov start
        # Defensive: ME path with show_means=TRUE, suppress_mean=FALSE, but
        # preds_df=NULL. Not reachable via the public API (preds_df is NULL
        # iff suppress_mean=TRUE), but guards against internal state changes.
        ref_mean <- NA_real_
        # nocov end
      }

      ref_row <- list(
        level = ref_level,
        estimate = 0,
        mean = ref_mean,
        n = as.integer(ref_n),
        n_weighted = ref_nw,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p_value = NA_real_,
        stars = ""
      )
      all_rows <- c(all_rows, list(ref_row))
      if (has_group) {
        all_group_rows <- c(all_group_rows, list(gc))
      } else {
        all_group_rows <- c(all_group_rows, list(data.frame()))
      }
    }

    # ── Treatment rows ───────────────────────────────────────────────────────
    if (has_group) {
      # Find indices in result that match this group combo
      group_match <- rep(TRUE, nrow(result$result_groups))
      for (gn in group_names) {
        group_match <- group_match &
          (result$result_groups[[gn]] == gc[[gn]])
      }
      treat_idx <- which(group_match)
    } else {
      treat_idx <- seq_along(result$result_levels)
    }

    for (i in treat_idx) {
      lvl <- result$result_levels[[i]]
      lvl_mask <- domain_mask &
        (design@data[[treats_name]] == lvl) &
        !is.na(design@data[[treats_name]])
      if (has_group) {
        for (gn in group_names) {
          lvl_mask <- lvl_mask &
            !is.na(design@data[[gn]]) &
            (design@data[[gn]] == gc[[gn]])
        }
      }
      lvl_n <- sum(lvl_mask)
      if (!is.null(wt_var)) {
        lvl_nw <- sum(
          design@data[[wt_var]][lvl_mask],
          na.rm = TRUE
        )
      } else {
        lvl_nw <- as.numeric(lvl_n)
      }

      treat_row <- list(
        level = lvl,
        estimate = result$result_estimates[[i]],
        mean = if (!is.null(result$result_means)) {
          result$result_means[[i]]
        } else {
          NA_real_
        },
        n = as.integer(lvl_n),
        n_weighted = lvl_nw,
        se = result$result_ses[[i]],
        ci_low = result$result_ci_lows[[i]],
        ci_high = result$result_ci_highs[[i]],
        p_value = result$result_p_values[[i]],
        stars = ""
      )
      all_rows <- c(all_rows, list(treat_row))
      if (has_group) {
        all_group_rows <- c(all_group_rows, list(gc))
      } else {
        all_group_rows <- c(all_group_rows, list(data.frame()))
      }
    }
  }

  # ── Small-cell warnings ───────────────────────────────────────────────────
  for (r in all_rows) {
    if (!is.na(r$n) && r$n > 0L && r$n < min_cell_n) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Treatment level {.val {r$level}} has only ",
            "{r$n} observation{?s} (threshold: {min_cell_n})."
          )
        ),
        class = "surveycore_warning_small_cell"
      )
    }
  }

  # ── P-value adjustment ────────────────────────────────────────────────────
  if (!is.null(pval_adj)) {
    if (!has_group) {
      # Global adjustment across all non-reference rows
      comp_idx <- which(
        vapply(all_rows, function(r) r$estimate != 0, logical(1L))
      )
      if (length(comp_idx) > 0L) {
        pvals <- vapply(all_rows[comp_idx], function(r) r$p_value, double(1L))
        adj_pvals <- stats::p.adjust(pvals, method = pval_adj)
        for (j in seq_along(comp_idx)) {
          all_rows[[comp_idx[[j]]]]$p_value <- adj_pvals[[j]]
        }
      }
    } else {
      # Within-group adjustment
      for (gi in seq_len(n_combos)) {
        gc_adj <- unique_group_combos[gi, , drop = FALSE]
        comp_idx <- integer(0)
        for (ri in seq_along(all_rows)) {
          if (all_rows[[ri]]$estimate != 0) {
            grp_match <- TRUE
            for (gn in group_names) {
              grp_match <- grp_match &&
                (all_group_rows[[ri]][[gn]] == gc_adj[[gn]])
            }
            if (grp_match) {
              comp_idx <- c(comp_idx, ri)
            }
          }
        }
        if (length(comp_idx) > 0L) {
          pvals <- vapply(
            all_rows[comp_idx],
            function(r) r$p_value,
            double(1L)
          )
          adj_pvals <- stats::p.adjust(pvals, method = pval_adj)
          for (j in seq_along(comp_idx)) {
            all_rows[[comp_idx[[j]]]]$p_value <- adj_pvals[[j]]
          }
        }
      }
    }
  }

  # ── Stars and pct_change ──────────────────────────────────────────────────
  #
  # For pct_change we need the reference mean per group combo. When
  # show_means = TRUE, it's the mean field of the reference row. When
  # show_means = FALSE and show_pct_change = TRUE and !suppress_mean, obtain
  # the fallback ref_mean:
  #   - clean path: result$reference_mean directly
  #   - ME path:   filter preds_df to the ref_level row (+ group match)
  #
  # Build a per-combo ref_mean lookup.
  if (show_pct_change && !suppress_mean) {
    ref_mean_per_combo <- vector("list", n_combos)
    for (gi in seq_len(n_combos)) {
      if (has_group) {
        gc_pct <- unique_group_combos[gi, , drop = FALSE]
      } else {
        gc_pct <- NULL
      }

      # Try to find from existing rows (show_means = TRUE)
      ref_mean_val <- NA_real_
      for (ri in seq_along(all_rows)) {
        if (all_rows[[ri]]$estimate == 0) {
          grp_ok <- TRUE
          if (has_group) {
            for (gn in group_names) {
              grp_ok <- grp_ok &&
                (all_group_rows[[ri]][[gn]] == gc_pct[[gn]])
            }
          }
          if (grp_ok) {
            ref_mean_val <- all_rows[[ri]]$mean
            break
          }
        }
      }

      # Fallback when show_means = FALSE
      if (is.na(ref_mean_val)) {
        if (!use_marginaleffects && !is.null(result$reference_mean)) {
          ref_mean_val <- result$reference_mean
        } else if (use_marginaleffects && !is.null(result$preds_df)) {
          pred_match <- result$preds_df[[treats_name]] == ref_level
          if (has_group) {
            for (gn in group_names) {
              pred_match <- pred_match &
                (result$preds_df[[gn]] == gc_pct[[gn]])
            }
          }
          matched <- result$preds_df$estimate[pred_match]
          if (length(matched) > 0L) {
            ref_mean_val <- matched[[1L]]
          }
        }
      }
      ref_mean_per_combo[[gi]] <- ref_mean_val
    }

    # Emit zero-ref warnings once per combo
    for (gi in seq_len(n_combos)) {
      rmv <- ref_mean_per_combo[[gi]]
      ref_is_zero <- !is.na(rmv) &&
        abs(rmv) < .Machine$double.eps * 100
      if (ref_is_zero) {
        cli::cli_warn(
          c(
            "!" = paste0(
              "Reference group mean is 0; ",
              "percentage change is undefined."
            )
          ),
          class = "surveycore_warning_pct_change_zero_ref"
        )
      }
    }
  }

  # Apply stars and pct_change to each row
  for (i in seq_along(all_rows)) {
    r <- all_rows[[i]]
    all_rows[[i]]$stars <- .stars_pval(r$p_value)

    if (show_pct_change && !suppress_mean) {
      # Determine which combo this row belongs to
      if (has_group) {
        combo_idx <- NA_integer_
        for (gi in seq_len(n_combos)) {
          gc_row <- unique_group_combos[gi, , drop = FALSE]
          match_g <- TRUE
          for (gn in group_names) {
            match_g <- match_g &&
              (all_group_rows[[i]][[gn]] == gc_row[[gn]])
          }
          if (match_g) {
            combo_idx <- gi
            break
          }
        }
        ref_mean_g <- ref_mean_per_combo[[combo_idx]]
      } else {
        ref_mean_g <- ref_mean_per_combo[[1L]]
      }

      ref_g_is_zero <- !is.na(ref_mean_g) &&
        abs(ref_mean_g) < .Machine$double.eps * 100

      if (r$estimate == 0) {
        all_rows[[i]]$pct_change <- NA_real_
      } else if (!is.na(ref_mean_g) && !ref_g_is_zero) {
        all_rows[[i]]$pct_change <- r$estimate / ref_mean_g
      } else {
        all_rows[[i]]$pct_change <- NA_real_
      }
    }
  }

  # ── Assemble col_vecs ────────────────────────────────────────────────────
  col_vecs <- list()

  col_vecs[[treats_name]] <- vapply(
    all_rows,
    function(r) r$level,
    character(1L)
  )
  col_vecs[["estimate"]] <- vapply(
    all_rows,
    function(r) r$estimate,
    double(1L)
  )

  if (show_pct_change && !suppress_mean) {
    col_vecs[["pct_change"]] <- vapply(
      all_rows,
      function(r) r$pct_change %||% NA_real_,
      double(1L)
    )
  }

  if (show_means && !suppress_mean) {
    col_vecs[["mean"]] <- vapply(
      all_rows,
      function(r) r$mean,
      double(1L)
    )
  }

  col_vecs[["n"]] <- vapply(
    all_rows,
    function(r) r$n,
    integer(1L)
  )

  if (n_weighted) {
    col_vecs[["n_weighted"]] <- vapply(
      all_rows,
      function(r) r$n_weighted,
      double(1L)
    )
  }

  if (!is.null(variance) && "se" %in% variance) {
    col_vecs[["se"]] <- vapply(
      all_rows,
      function(r) r$se,
      double(1L)
    )
  }

  if (!is.null(variance) && "ci" %in% variance) {
    col_vecs[["ci_low"]] <- vapply(
      all_rows,
      function(r) r$ci_low,
      double(1L)
    )
    col_vecs[["ci_high"]] <- vapply(
      all_rows,
      function(r) r$ci_high,
      double(1L)
    )
  }

  col_vecs[["p_value"]] <- vapply(
    all_rows,
    function(r) r$p_value,
    double(1L)
  )
  col_vecs[["stars"]] <- vapply(
    all_rows,
    function(r) r$stars,
    character(1L)
  )

  # ── Assemble groups_df ────────────────────────────────────────────────────
  if (has_group) {
    groups_df <- do.call(rbind, all_group_rows)
    rownames(groups_df) <- NULL
  } else {
    groups_df <- data.frame()
  }

  list(col_vecs = col_vecs, groups_df = groups_df)
}
