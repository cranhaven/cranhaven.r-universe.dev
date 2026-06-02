#' Calculate Standard Deviation Scores (SDS; z-scores) for health markers
#'
#' Computes per-variable SDS as (x - mean) / sd using supplied reference statistics.
#' Includes input validation, NA/error handling, data quality warnings, and
#' verbose progress via the package logger (hm_inform), aligned with HM-CS v3.
#'
#' Derivation: for each requested variable, SDS is calculated as
#' `(observed_value - reference_mean) / reference_sd` using user-supplied
#' reference statistics in `ref`.
#'
#' Usage note: this function does not derive normative reference values internally.
#' Users should provide `ref` from the target population (preferred), or from an
#' external source matched as closely as possible on age/sex/ethnicity/context.
#' Interpretability depends on the quality and relevance of those supplied
#' reference means and SDs.
#'
#' By default, returns a tibble with added `[var]_sds` columns (tidyverse-friendly).
#' For backward compatibility, you can request the previous list output.
#'
#' @param data A data.frame/tibble containing the variables.
#' @param vars Character vector of variable names in `data` to compute SDS for.
#'   Must be non-empty and unique.
#' @param ref A data.frame with columns: `variable`, `mean`, `sd` supplying reference
#'   statistics for each variable listed in `vars`. Must contain exactly one row per
#'   requested variable, with finite `mean` and `sd > 0`.
#' @param id_col Optional character scalar naming an ID column in `data` (used only
#'   in messages and duplicate-ID notes).
#' @param sds_cap Numeric scalar; absolute cap for SDS when `extreme_strategy = "cap"`.
#'   Must be positive. Default 6.
#' @param na_strategy One of c("omit","error","keep"):
#'   - "omit": drop rows with missing values across any of `vars` (default)
#'   - "error": stop if any missing values among `vars`
#'   - "keep": keep rows; SDS for missing inputs will be NA
#' @param extreme_strategy One of c("cap","warn","error","NA"):
#'   - "cap": cap |SDS| at `sds_cap` and warn (default)
#'   - "warn": keep extreme SDS, but warn
#'   - "error": stop if any |SDS| > `sds_cap`
#'   - "NA": set extreme SDS to NA
#' @param na_action Optional HM-CS alias for `na_strategy`
#'   (accepted values: keep/omit/error; used if provided).
#' @param check_extreme Logical; if TRUE, enables SDS extreme handling (alias for
#'   legacy behavior; defaults to TRUE in this implementation).
#' @param extreme_action Optional HM-CS alias for `extreme_strategy`
#'   (accepted values: cap/NA/error; used if provided).
#' @param warn_thresholds Named list controlling warnings (proportions in \eqn{[0,1]}):
#'   - na_prop: warn if proportion of rows with NA among `vars` exceeds this (default 0.05)
#'   - extreme_prop: warn if proportion of extreme SDS (cells) exceeds this (default 0.01)
#' @param return One of c("data","list"). "data" returns a tibble with added
#'   `[var]_sds` columns (default). "list" returns a list with components
#'   `data`, `summary`, and `warnings` (backward compatible).
#' @param verbose Logical; if TRUE, emit progress via hm_inform(). Also controlled by
#'   options(healthmarkers.verbose = "none"|"inform"|"debug").
#'
#' @return
#' - If `return = "data"` (default): a tibble with added `[var]_sds` columns.
#' - If `return = "list"`: a list with:
#'   - data: tibble with added SDS columns
#'   - summary: list with input/output row counts, omitted rows, total extremes, and per-variable summary
#'   - warnings: character vector of warning messages emitted
#'
#' @examples
#' ref <- data.frame(
#'   variable = c("bmi","sbp"),
#'   mean     = c(25, 120),
#'   sd       = c(4, 15)
#' )
#' df <- data.frame(
#'   id  = 1:6,
#'   bmi = c(24, 30, NA, 29, 10, 26),
#'   sbp = c(118, 200, 119, 121, 500, 120)
#' )
#' out_tbl <- calc_sds(
#'   data = df,
#'   vars = c("bmi","sbp"),
#'   ref = ref,
#'   id_col = "id",
#'   na_strategy = "omit",
#'   extreme_strategy = "cap",
#'   sds_cap = 6,
#'   verbose = FALSE
#' )
#' @importFrom tibble as_tibble
#' @export
calc_sds <- function(
  data,
  vars,
  ref,
  id_col = NULL,
  sds_cap = 6,
  na_strategy = c("omit","error","keep"),
  extreme_strategy = c("cap","warn","error","NA"),
  warn_thresholds = list(na_prop = 0.05, extreme_prop = 0.01),
  return = c("data","list"),
  verbose = TRUE,
  # HM-CS aliases (back-compat)
  na_action = NULL,
  check_extreme = TRUE,
  extreme_action = NULL
) {
  # HM-CS alias mapping (keep messages gated at debug)
  if (!is.null(na_action)) {
    na_strategy <- match.arg(na_action, c("keep","omit","error"))
    hm_inform("calc_sds(): using 'na_action' alias of 'na_strategy'.", level = "debug")
  }
  if (!is.null(extreme_action)) {
    extreme_strategy <- match.arg(extreme_action, c("cap","NA","error"))
    hm_inform("calc_sds(): using 'extreme_action' alias of 'extreme_strategy'.", level = "debug")
  }

  na_strategy <- match.arg(na_strategy)
  extreme_strategy <- match.arg(extreme_strategy)
  return <- match.arg(return)

  # --- Input validation ---
  if (!is.data.frame(data)) {
    rlang::abort("calc_sds(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_calc_sds_error_data_type")
  }
  if (!is.character(vars)) {
    rlang::abort("calc_sds(): `vars` must be a character vector.",
                 class = "healthmarkers_calc_sds_error_vars_type")
  }
  if (length(vars) == 0) {
    rlang::abort("calc_sds(): `vars` must be non-empty.",
                 class = "healthmarkers_calc_sds_error_vars_empty")
  }
  if (anyNA(vars) || any(vars == "")) {
    rlang::abort("calc_sds(): `vars` must not contain NA or empty names.",
                 class = "healthmarkers_calc_sds_error_vars_bad_names")
  }
  if (anyDuplicated(vars)) {
    rlang::abort("calc_sds(): `vars` must be unique (no duplicates).",
                 class = "healthmarkers_calc_sds_error_vars_duplicate")
  }
  .ensure_cols_exist(data, vars, label = "data")
  if (!is.null(id_col)) .ensure_cols_exist(data, id_col, label = "data (id_col)")

  ref_ok <- .validate_ref(ref, vars)
  if (!ref_ok$ok) {
    rlang::abort(ref_ok$msg, class = "healthmarkers_calc_sds_error_ref")
  }

  if (!is.numeric(sds_cap) || length(sds_cap) != 1 || !is.finite(sds_cap) || sds_cap <= 0) {
    rlang::abort("calc_sds(): `sds_cap` must be a positive finite number.",
                 class = "healthmarkers_calc_sds_error_sds_cap")
  }
  if (!is.list(warn_thresholds)) {
    rlang::abort("calc_sds(): `warn_thresholds` must be a list with elements `na_prop` and `extreme_prop`.",
                 class = "healthmarkers_calc_sds_error_warn_thresholds_type")
  }
  na_prop <- warn_thresholds$na_prop %||% 0.05
  extreme_prop <- warn_thresholds$extreme_prop %||% 0.01
  if (!(is.numeric(na_prop) && length(na_prop) == 1 && is.finite(na_prop) && na_prop >= 0 && na_prop <= 1)) {
    rlang::abort("calc_sds(): `warn_thresholds$na_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_calc_sds_error_warn_na_prop")
  }
  if (!(is.numeric(extreme_prop) && length(extreme_prop) == 1 && is.finite(extreme_prop) && extreme_prop >= 0 && extreme_prop <= 1)) {
    rlang::abort("calc_sds(): `warn_thresholds$extreme_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_calc_sds_error_warn_extreme_prop")
  }
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    rlang::abort("calc_sds(): `verbose` must be a single logical value.",
                 class = "healthmarkers_calc_sds_error_verbose_type")
  }

  # --- Start messages ---
  if (isTRUE(verbose)) {
    total_rows <- nrow(data)
    msg_id <- if (is.null(id_col)) "" else paste0(" (id: ", id_col, ")")
    hm_inform(sprintf("calc_sds: starting on %d row(s), %d variable(s)%s",
                      total_rows, length(vars), msg_id), level = "inform")
    map_parts <- vapply(vars, function(k) sprintf("%s -> '%s'", k, k), character(1))
    hm_inform(sprintf("calc_sds(): col_map: %s", paste(map_parts, collapse = ", ")), level = "inform")
  }

  warns <- character(0)
  out <- data

  # --- Coerce non-numeric vars with warning ---
  for (v in vars) {
    if (!is.numeric(out[[v]])) {
      old <- out[[v]]
      suppressWarnings(out[[v]] <- as.numeric(out[[v]]))
      introduced_na <- sum(is.na(out[[v]]) & !is.na(old))
      w <- sprintf("variable `%s` was coerced to numeric; NAs introduced: %d", v, introduced_na)
      rlang::warn(w, class = "healthmarkers_calc_sds_warn_na_coercion")
      warns <- c(warns, w)
      if (isTRUE(verbose)) hm_inform(sprintf("- coerced `%s` to numeric", v), level = "debug")
    }
  }

  # --- NA handling strategy ---
  row_has_na <- rowSums(is.na(out[, vars, drop = FALSE])) > 0
  n_rows_in <- nrow(out)
  n_rows_na <- sum(row_has_na)
  prop_na_rows <- if (n_rows_in > 0) n_rows_na / n_rows_in else 0

  if (prop_na_rows > na_prop) {
    w <- sprintf("calc_sds(): high NA proportion among `vars`: %.1f%% of rows", 100 * prop_na_rows)
    rlang::warn(w, class = "healthmarkers_calc_sds_warn_na_prop_high")
    warns <- c(warns, w)
  }

  if (na_strategy == "error" && n_rows_na > 0) {
    rlang::abort(sprintf("Missing values found in `vars` for %d row(s); na_strategy='error'", n_rows_na),
                 class = "healthmarkers_calc_sds_error_na_strategy_missing")
  } else if (na_strategy == "omit" && n_rows_na > 0) {
    if (isTRUE(verbose)) hm_inform(sprintf("- omitting %d row(s) with NA across `vars`", n_rows_na), level = "inform")
    out <- out[!row_has_na, , drop = FALSE]
  } else if (isTRUE(verbose) && n_rows_na > 0) {
    hm_inform(sprintf("- keeping %d row(s) with NA across `vars`", n_rows_na), level = "inform")
  }

  # Recompute after omission
  n_rows_out <- nrow(out)
  omitted_rows <- n_rows_in - n_rows_out

  # --- Prepare reference lookups ---
  ref_map_mean <- setNames(ref$mean, ref$variable)
  ref_map_sd   <- setNames(ref$sd,   ref$variable)

  # Validate SD > 0 for all vars
  zero_sd_vars <- vars[ref_map_sd[vars] <= 0 | !is.finite(ref_map_sd[vars])]
  if (length(zero_sd_vars)) {
    rlang::abort(sprintf("Reference SD must be > 0 and finite for: %s", paste(zero_sd_vars, collapse = ", ")),
                 class = "healthmarkers_calc_sds_error_ref_sd")
  }

  # --- Compute SDS per variable ---
  if (isTRUE(verbose)) hm_inform(sprintf("Computing SDS for %d variable(s)...", length(vars)), level = "inform")
  per_var_summary <- data.frame(
    variable = vars,
    n_missing = integer(length(vars)),
    n_extreme = integer(length(vars)),
    stringsAsFactors = FALSE
  )

  total_extreme <- 0L

  for (i in seq_along(vars)) {
    v <- vars[i]
    mu <- ref_map_mean[[v]]
    sdv <- ref_map_sd[[v]]

    z <- (out[[v]] - mu) / sdv
    # Count missing (after strategy)
    per_var_summary$n_missing[i] <- sum(is.na(out[[v]]))

    if (isTRUE(check_extreme)) {
      is_extreme <- is.finite(z) & abs(z) > sds_cap
      n_extreme <- sum(is_extreme, na.rm = TRUE)
      total_extreme <- total_extreme + n_extreme

      if (n_extreme > 0) {
        if (identical(extreme_strategy, "error")) {
          rlang::abort(sprintf("Found %d SDS beyond +/-%g for `%s`", n_extreme, sds_cap, v),
                       class = "healthmarkers_calc_sds_error_extreme_sds")
        } else if (identical(extreme_strategy, "cap")) {
          z[is_extreme] <- sds_cap * sign(z[is_extreme])
          w <- sprintf("Capped %d SDS beyond +/-%g for `%s`", n_extreme, sds_cap, v)
           rlang::warn(w, class = "healthmarkers_calc_sds_warn_cap_extreme")
           warns <- c(warns, w)
        } else if (identical(extreme_strategy, "warn")) {
          w <- sprintf("Detected %d SDS beyond +/-%g for `%s` (not capped)", n_extreme, sds_cap, v)
           rlang::warn(w, class = "healthmarkers_calc_sds_warn_detect_extreme")
           warns <- c(warns, w)
        } else if (identical(extreme_strategy, "NA")) {
          z[is_extreme] <- NA_real_
        }
      }

      per_var_summary$n_extreme[i] <- n_extreme
    }

    out[[paste0(v, "_sds")]] <- z
  }

  # --- Data quality warning for extremes proportion ---
  prop_extreme <- if (n_rows_out > 0) total_extreme / (n_rows_out * length(vars)) else 0
  if (prop_extreme > extreme_prop) {
    w <- sprintf("calc_sds(): high proportion of extreme SDS: %.2f%% of computed cells", 100 * prop_extreme)
    rlang::warn(w, class = "healthmarkers_calc_sds_warn_extreme_prop_high")
    warns <- c(warns, w)
  }

  # --- Verbose completion summary ---
  if (isTRUE(verbose)) {
    hm_inform(level = "inform", msg = "calc_sds: completed")
    hm_inform(sprintf("- rows in:    %d", n_rows_in), level = "debug")
    hm_inform(sprintf("- rows out:   %d", n_rows_out), level = "debug")
    hm_inform(sprintf("- omitted:    %d (na_strategy = '%s')", omitted_rows, na_strategy), level = "debug")
    hm_inform(sprintf("- extremes:   %d (strategy = '%s', cap = %s)", total_extreme, extreme_strategy, as.character(sds_cap)), level = "debug")
    if (!is.null(id_col) && n_rows_out > 0 && anyDuplicated(out[[id_col]]) > 0) {
      hm_inform("- note: duplicate IDs detected", level = "debug")
    }
    hm_inform(level = "inform", msg = hm_result_summary(
      tibble::as_tibble(out)[paste0(vars, "_sds")], "calc_sds"
    ))
  }

  out_tbl <- tibble::as_tibble(out)

  if (return == "data") {
    return(out_tbl)
  } else {
    return(list(
      data = out_tbl,
      summary = list(
        rows_in = n_rows_in,
        rows_out = n_rows_out,
        omitted_rows = omitted_rows,
        total_extreme = total_extreme,
        per_var = per_var_summary
      ),
      warnings = unique(warns)
    ))
  }
}

# Internal helpers ----------------------------------------------------------

# Safe null-coalescing for lists
`%||%` <- function(x, y) if (is.null(x)) y else x

.ensure_cols_exist <- function(df, cols, label = "data") {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols)) {
    rlang::abort(
      sprintf("Missing column(s) in `%s`: %s", label, paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_calc_sds_error_missing_columns"
    )
  }
  invisible(TRUE)
}

.validate_ref <- function(ref, vars) {
  if (!is.data.frame(ref)) {
    return(list(ok = FALSE, msg = "calc_sds(): `ref` must be a data.frame with columns: variable, mean, sd"))
  }
  req <- c("variable","mean","sd")
  if (!all(req %in% names(ref))) {
    return(list(ok = FALSE, msg = "calc_sds(): `ref` must have columns: variable, mean, sd"))
  }
  if (!is.character(ref$variable)) {
    return(list(ok = FALSE, msg = "calc_sds(): `ref$variable` must be character"))
  }
  if (!is.numeric(ref$mean) || !is.numeric(ref$sd)) {
    return(list(ok = FALSE, msg = "calc_sds(): `ref$mean` and `ref$sd` must be numeric"))
  }
  if (anyDuplicated(ref$variable)) {
    return(list(ok = FALSE, msg = "calc_sds(): `ref` must have at most one row per variable"))
  }
  missing_vars <- setdiff(vars, ref$variable)
  if (length(missing_vars)) {
    return(list(ok = FALSE, msg = sprintf("calc_sds(): `ref` is missing stats for: %s", paste(missing_vars, collapse = ", "))))
  }
  rsub <- ref[match(vars, ref$variable), , drop = FALSE]
  if (any(!is.finite(rsub$mean)) || any(!is.finite(rsub$sd))) {
    return(list(ok = FALSE, msg = "calc_sds(): `ref` contains non-finite mean or sd"))
  }
  list(ok = TRUE, msg = "")
}
