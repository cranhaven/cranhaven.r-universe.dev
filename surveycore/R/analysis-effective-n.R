# R/analysis-effective-n.R
# Effective sample size for survey designs.
# Exported function: get_effective_n()

# ── .kish_effective_n() [internal] ────────────────────────────────────────────

.kish_effective_n <- function(weights) {
  if (length(weights) == 0L) {
    return(NA_real_)
  }
  sum(weights)^2 / sum(weights^2)
}

# ── .get_analysis_weights() [internal] ────────────────────────────────────────
# Extract full analysis weights for all rows. Handles twophase combined weights.

.get_analysis_weights <- function(design) {
  if (S7::S7_inherits(design, survey_twophase)) {
    subset_col <- design@data[[design@variables$subset]]
    pi2_full <- .compute_phase2_probs(design, subset_col)
    w_full <- design@data[[design@variables$phase1$weights]]
    w_full / pi2_full
  } else {
    design@data[[design@variables$weights]]
  }
}

# ── get_effective_n() ─────────────────────────────────────────────────────────

#' Effective Sample Size for a Survey Design
#'
#' Computes the effective sample size of a survey design using either the
#' Kish (1965) weight-only approximation (`method = "kish"`) or the full
#' design-effect-based formula for a specified variable (`method = "deff"`).
#'
#' @details
#' The **Kish method** (`method = "kish"`) computes effective N from survey
#' weights alone: `n_eff = sum(w)^2 / sum(w^2)`. It captures only weight variation.
#' For clustered designs with equal weights, `deff_kish = 1.0` even when the
#' true design effect is substantially greater due to clustering. Use
#' `method = "deff"` to capture the full design effect for a specific
#' analysis variable.
#'
#' The **DEFF method** (`method = "deff"`) computes effective N as
#' `n_eff = n / DEFF`, where `DEFF = Var_design / Var_SRS` for variable `x`.
#' It captures clustering, stratification, and weight variation jointly.
#'
#' @param design A survey design object: `survey_taylor`, `survey_replicate`,
#'   `survey_twophase`, `survey_nonprob`, or a `survey_collection`.
#' @param x <[`tidy-select`][tidyselect::language]> A single unquoted numeric
#'   variable name. Required when `method = "deff"`; ignored (with a message)
#'   when `method = "kish"`. Default `NULL`.
#' @param group <[`tidy-select`][tidyselect::language]> Optional grouping
#'   variable(s). Combined with any grouping set by `group_by()`. Default
#'   `NULL`.
#' @param method Character(1). `"kish"` (default) or `"deff"`. Controls the
#'   effective-N formula. Matched via `match.arg()`.
#' @param na.rm Logical. If `TRUE` (default), exclude observations with `NA`
#'   weights or group variables from the Kish computation; passed to
#'   `get_means()` for the DEFF computation.
#' @param decimals Integer or `NULL`. Rounds `n_eff` and deff columns to this
#'   many decimal places. `n` is always integer and is never rounded. Default
#'   `NULL`.
#' @param min_cell_n Integer. Minimum unweighted cell count before
#'   `surveycore_warning_small_cell` fires (Kish method only). Default `30L`.
#' @param ... Unused. Reserved so that `.id` and `.if_missing_var` remain
#'   named-only when a `survey_collection` is passed.
#' @param .id Character(1) or `NULL`. Column name identifying each survey in a
#'   `survey_collection`. Default `NULL` (uses the collection's stored `@id`).
#' @param .if_missing_var `"error"`, `"skip"`, or `NULL`. Handling for
#'   surveys in a collection that lack `x`. Default `NULL`.
#'
#' @return A `survey_effective_n` tibble (also inheriting `survey_result`).
#'   Columns, in order:
#' \itemize{
#'   \item `[.id]` — survey identifier column (when `design` is a collection).
#'   \item `[group_cols...]` — group variable columns (when grouping is active).
#'   \item `n` — integer. Unweighted count of observations.
#'   \item `n_eff` — numeric. Effective sample size.
#'   \item `deff_kish` — numeric. Weight-based design effect (`n / n_eff`).
#'     Present when `method = "kish"` only.
#'   \item `deff` — numeric. Full design effect (`Var_design / Var_SRS`).
#'     Present when `method = "deff"` only.
#' }
#' Use `meta(result)$method` to retrieve the formula used. For DEFF,
#' `meta(result)$x` is a named list with variable metadata.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#'
#' # Kish effective N (weight-only approximation)
#' get_effective_n(d)
#'
#' # Full DEFF effective N for a specific variable
#' get_effective_n(d, ridageyr, method = "deff")
#' @family analysis
#' @export
get_effective_n <- function(
  design,
  x = NULL,
  group = NULL,
  method = c("kish", "deff"),
  na.rm = TRUE,
  decimals = NULL,
  min_cell_n = 30L,
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  # ── Collection dispatch ────────────────────────────────────────────────────
  if (S7::S7_inherits(design, survey_collection)) {
    return(.dispatch_over_collection(
      get_effective_n,
      design,
      x = {{ x }},
      group = {{ group }},
      method = method,
      na.rm = na.rm,
      decimals = decimals,
      min_cell_n = min_cell_n,
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }

  # ── Step 1: Validate ───────────────────────────────────────────────────────
  .check_unsupported_class(design, "get_effective_n")
  method <- match.arg(method)
  .validate_shared_args(
    NULL,
    0.95,
    "surveycore",
    decimals = decimals,
    na.rm = na.rm
  )

  # ── Step 2: Resolve x and group ────────────────────────────────────────────
  x_quo <- rlang::enquo(x)
  group_quo <- rlang::enquo(group)

  x_name <- NULL
  x_meta <- NULL

  if (method == "deff") {
    if (rlang::quo_is_null(x_quo)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg x} is required when {.arg method} = {.val \"deff\"}."
          ),
          "i" = paste0(
            "Supply a numeric variable name, or switch to ",
            "{.code method = \"kish\"} which needs no variable."
          )
        ),
        class = "surveycore_error_effective_n_deff_requires_x"
      )
    }
    selected <- .resolve_tidy_select(x_quo, design@data)
    if (length(selected) != 1L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg x} must select exactly one variable, not ",
            "{length(selected)}."
          ),
          "i" = "Selected: {.field {selected}}.",
          "v" = "Pass one variable at a time, or loop over variables."
        ),
        class = "surveycore_error_effective_n_x_multi_col"
      )
    }
    x_name <- selected[[1L]]
    .precheck_vars_present(design, x_name)
    x_meta <- .extract_var_meta(design, x_name)
  } else {
    if (!rlang::quo_is_null(x_quo)) {
      rlang::inform("x is ignored when method = 'kish'")
    }
  }

  # ── Step 3: Resolve groups and domain ──────────────────────────────────────
  group_vars <- .resolve_groups(design, group_quo)
  domain_mask <- .apply_domain(design)

  # For Kish on twophase: restrict domain to Phase 2 rows
  if (method == "kish" && S7::S7_inherits(design, survey_twophase)) {
    subset_col <- design@data[[design@variables$subset]]
    domain_mask <- domain_mask & subset_col
  }

  # ── Step 4: Single-level warning ───────────────────────────────────────────
  if (length(group_vars) > 0L) {
    for (gv in group_vars) {
      gv_vals <- design@data[[gv]][domain_mask]
      uniq_lvls <- unique(gv_vals[!is.na(gv_vals)])
      if (length(uniq_lvls) < 2L) {
        cli::cli_warn(
          c(
            "!" = paste0(
              "Grouping variable {.field {gv}} has only one observed level ",
              if (length(uniq_lvls) == 1L) {
                "({.val {as.character(uniq_lvls[[1L]])}})."
              } else {
                "."
              },
              " Grouped estimates will have a single row."
            )
          ),
          class = "surveycore_warning_single_level"
        )
      }
    }
  }

  # ── Step 5: Build group combos ─────────────────────────────────────────────
  if (length(group_vars) > 0L) {
    domain_data <- design@data[domain_mask, group_vars, drop = FALSE]
    group_combos <- .build_group_combos(domain_data, na.rm)
    n_combos <- nrow(group_combos)
  } else {
    group_combos <- data.frame()
    n_combos <- 1L
  }

  # ── Step 6: Branch on method ───────────────────────────────────────────────
  if (method == "kish") {
    all_weights <- .get_analysis_weights(design)

    acc_n <- integer(0)
    acc_n_eff <- numeric(0)
    acc_grp_rows <- vector("list", 0L)
    small_cell_ns <- integer(0)

    for (ci in seq_len(n_combos)) {
      if (length(group_vars) > 0L) {
        combo_row <- group_combos[ci, , drop = FALSE]
        data_cols <- as.list(design@data[group_vars])
        group_match <- .match_group_combo(data_cols, combo_row)
        active_mask <- domain_mask & group_match
      } else {
        active_mask <- domain_mask
        combo_row <- NULL
      }

      w <- all_weights[active_mask]
      if (isTRUE(na.rm)) {
        w <- w[!is.na(w)]
      }

      n_cell <- length(w)
      n_eff_cell <- .kish_effective_n(w)

      if (n_cell > 0L && n_cell < min_cell_n) {
        small_cell_ns <- c(small_cell_ns, n_cell)
      }

      acc_n <- c(acc_n, as.integer(n_cell))
      acc_n_eff <- c(acc_n_eff, n_eff_cell)

      if (length(group_vars) > 0L) {
        acc_grp_rows <- c(acc_grp_rows, list(combo_row))
      }
    }

    if (length(small_cell_ns) > 0L) {
      n_small <- length(small_cell_ns)
      cli::cli_warn(
        c(
          "!" = paste0(
            "{n_small} cell{?s} {?has/have} fewer than {min_cell_n} ",
            "unweighted observations. Estimates in these cells may be ",
            "unreliable for public reporting (AAPOR guidance)."
          )
        ),
        class = "surveycore_warning_small_cell"
      )
    }

    col_vecs <- list(
      n = acc_n,
      n_eff = acc_n_eff,
      deff_kish = acc_n / acc_n_eff
    )
    meta_x <- NULL

    # Build groups_df
    if (length(group_vars) > 0L && length(acc_grp_rows) > 0L) {
      groups_df <- do.call(rbind, acc_grp_rows)
      rownames(groups_df) <- NULL
      groups_df <- .apply_group_labels(groups_df, group_vars, design, TRUE)
    } else {
      groups_df <- data.frame()
    }
  } else {
    # method = "deff" — delegate to get_means(variance = "deff")
    means_result <- rlang::inject(get_means(
      design,
      !!rlang::sym(x_name),
      group = !!group_quo,
      variance = "deff",
      na.rm = na.rm,
      min_cell_n = min_cell_n,
      ...
    ))

    extracted_n <- means_result$n
    extracted_deff <- means_result$deff
    computed_n_eff <- ifelse(
      is.finite(extracted_deff) & extracted_deff > 0,
      extracted_n / extracted_deff,
      NA_real_
    )

    col_vecs <- list(
      n = as.integer(extracted_n),
      n_eff = computed_n_eff,
      deff = extracted_deff
    )
    meta_x <- stats::setNames(list(x_meta), x_name)

    # Extract group columns from means_result
    if (length(group_vars) > 0L) {
      groups_df <- as.data.frame(means_result[, group_vars, drop = FALSE])
      rownames(groups_df) <- NULL
    } else {
      groups_df <- data.frame()
    }
  }

  # ── Step 7: Build meta and assemble result ─────────────────────────────────
  group_meta <- .build_group_meta(design, group_vars)

  meta_args <- list(
    conf_level = 0.95,
    call = match.call(),
    group = group_meta,
    x = meta_x,
    method = method
  )

  result <- .make_result_tibble(
    col_vecs,
    groups_df,
    "survey_effective_n",
    design,
    meta_args,
    EFFECTIVE_N_META_KEYS
  )

  # ── Step 8: Apply decimals ─────────────────────────────────────────────────
  if (!is.null(decimals)) {
    result <- .apply_decimals(result, decimals)
  }

  result
}

# ── print.survey_effective_n() ────────────────────────────────────────────────

#' Print method for survey_effective_n objects.
#' @param x A `survey_effective_n` object.
#' @param ... Additional arguments (unused).
#' @return `x`, invisibly.
#' @method print survey_effective_n
#' @keywords internal
#' @export
print.survey_effective_n <- function(x, ...) {
  m <- attr(x, ".meta")
  cls <- class(x)[1L]
  dims <- paste(nrow(x), "\u00d7", ncol(x))
  method_str <- paste("method:", m$method)
  x_str <- if (!is.null(m$x)) paste("  x:", names(m$x)[1L]) else ""
  cat(sprintf("# A <%s> [%s]  %s%s\n", cls, dims, method_str, x_str))
  NextMethod()
  invisible(x)
}
