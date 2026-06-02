
#' Compute sex-stratified standardized scores (SDS) for adiposity measures
#'
#' @description
#' Computes sex-specific SDS (z-scores) for selected anthropometric variables using
#' reference means and SDs provided separately for males and females.
#'
#' @param data Data frame or tibble with variables and a sex column
#' @param col_map Named list mapping:
#'   - sex: column name with sex values ("M","F","m","f", 1, 2)
#'   - vars: optional named list mapping reference variable names -> data column names.
#'           If omitted, expects the reference variable names to exist in `data`.
#' @param ref Named list with elements "M" and "F". Each is a named list of numeric
#'   vectors c(mean=, sd=) keyed by variable name, e.g.:
#'   list(
#'     M = list(BMI = c(mean=23, sd=3.5), waist = c(mean=85, sd=10)),
#'     F = list(BMI = c(mean=21, sd=3.0), waist = c(mean=75, sd=9))
#'   )
#' @param na_action One of:
#'   - "keep"  - keep rows with NA (propagates to outputs)
#'   - "omit"  - drop rows with NA in any required variable
#'   - "error" - abort if any required variable has NA
#' @param allow_partial If TRUE, skip variables absent in data (with a warning); if FALSE error
#' @param prefix Optional prefix for output columns (default "")
#' @param verbose Logical; when TRUE emit progress via package logger; by default logging is controlled by options(healthmarkers.verbose)
#'
#' @return A tibble with one SDS column per retained variable: \code{varname_SDS}, where \code{varname} is the original variable name (optionally prefixed by \code{prefix}).
#'
#' @references
#' \insertRef{who1995anthro}{HealthMarkers}
#'
#' @examples
#' ref <- list(
#'   M = list(BMI = c(mean=24.5, sd=3.8), waist = c(mean=88, sd=12)),
#'   F = list(BMI = c(mean=22.1, sd=4.2), waist = c(mean=76, sd=11))
#' )
#' df <- data.frame(BMI=c(25.2,21.8,27.1), waist=c(85,72,95), sex=c("M","F","M"))
#' adiposity_sds_strat(
#'   df,
#'   col_map = list(sex = "sex", vars = list(BMI = "BMI", waist = "waist")),
#'   ref = ref
#' )
#' @export
adiposity_sds_strat <- function(data,
                                col_map,
                                ref,
                                na_action = c("keep","omit","error"),
                                allow_partial = FALSE,
                                prefix = "",
                                verbose = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "adiposity_sds_strat"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)
  na_action <- match.arg(na_action)

  # Custom checks so tests see the expected error messages
  if (!is.list(col_map) || is.null(col_map$sex) || !nzchar(col_map$sex)) {
    rlang::abort("adiposity_sds_strat(): 'col_map$sex' is required.",
                 class = "healthmarkers_adiposds_error_colmap_sex")
  }
  sex_col <- col_map$sex
  if (!sex_col %in% names(data)) {
    rlang::abort(sprintf("adiposity_sds_strat(): sex column '%s' not found.", sex_col),
                 class = "healthmarkers_adiposds_error_sexcol_missing")
  }

  # Validate ref structure
  if (!is.list(ref) || !all(c("M","F") %in% names(ref))) {
    rlang::abort("adiposity_sds_strat(): `ref` must be a named list with elements 'M' and 'F'.",
                 class = "healthmarkers_adiposds_error_ref_structure")
  }
  vars_M <- names(ref$M); vars_F <- names(ref$F)
  if (is.null(vars_M) || is.null(vars_F) || any(vars_M == "") || any(vars_F == "")) {
    rlang::abort("adiposity_sds_strat(): reference variable names must be non-empty.",
                 class = "healthmarkers_adiposds_error_ref_names")
  }
  if (!identical(sort(vars_M), sort(vars_F))) {
    rlang::abort("adiposity_sds_strat(): ref$M and ref$F must define identical variables.",
                 class = "healthmarkers_adiposds_error_ref_var_mismatch")
  }
  vars <- vars_M

  # Validate each ref component
  check_ref_comp <- function(x) {
    if (!is.numeric(x) || is.null(names(x)) || !all(c("mean","sd") %in% names(x))) {
      rlang::abort("adiposity_sds_strat(): ref component must be numeric with names mean, sd.",
                   class = "healthmarkers_adiposds_error_ref_component")
    }
    if (!is.finite(x[["sd"]]) || x[["sd"]] <= 0) {
      rlang::abort("adiposity_sds_strat(): sd must be >0.",
                   class = "healthmarkers_adiposds_error_ref_sd")
    }
  }
  for (v in vars) {
    check_ref_comp(ref$M[[v]])
    check_ref_comp(ref$F[[v]])
  }

  # Build variable mapping with inference support; backward-compat col_map$vars
  flat_vars_map <- if (!is.null(col_map$vars)) col_map$vars else NULL
  cm      <- .hm_build_col_map(data, flat_vars_map, keys = vars, fn = fn_name)
  data    <- cm$data
  var_map <- cm$col_map

  # Sex normalization
  sex_raw <- data[[sex_col]]
  sex_norm <- ifelse(sex_raw %in% c("M","m",1), "M",
                ifelse(sex_raw %in% c("F","f",2), "F", NA_character_))
  bad_sex <- sum(is.na(sex_norm))
  if (bad_sex > 0) {
    rlang::abort(sprintf("adiposity_sds_strat(): %d rows have unmapped sex values.", bad_sex),
                 class = "healthmarkers_adiposds_error_sex_values")
  }

  # Ensure variable columns exist (respect allow_partial)
  # Check both: key present in var_map AND mapped column exists in data
  missing_in_data <- vars[!vapply(vars, function(v) {
    !is.null(var_map[[v]]) && isTRUE(var_map[[v]] %in% names(data))
  }, logical(1))]
  if (length(missing_in_data)) {
    if (isTRUE(allow_partial)) {
      hm_inform(sprintf(
        "adiposity_sds_strat(): skipping %d ref vars absent in data: %s",
        length(missing_in_data), paste(missing_in_data, collapse = ", ")
      ), level = "inform")
      keep_vars <- vars[vars %in% names(var_map)]
      vars <- keep_vars
      var_map <- var_map[vars]
    } else {
      rlang::abort(sprintf("adiposity_sds_strat(): variables not found in data: %s",
                           paste(missing_in_data, collapse = ", ")),
                   class = "healthmarkers_adiposds_error_missing_vars")
    }
  }
  if (!length(vars)) {
    rlang::abort("adiposity_sds_strat(): no variables remain to process.",
                 class = "healthmarkers_adiposds_error_no_vars")
  }

  .hm_log_cols(cm, var_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  %s", fn_name, paste(paste0(prefix, vars, "_SDS"), collapse = ", ")), level = "inform")

  # Coerce to numeric (warn on NA introduction)
  for (v in vars) {
    cn <- var_map[[v]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      intro <- sum(is.na(data[[cn]]) & !is.na(old))
      if (intro > 0) {
        rlang::warn(
          sprintf("adiposity_sds_strat(): column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
          class = "healthmarkers_adiposds_warn_na_coercion"
        )
      }
    }
  }

  # NA policy (across selected variables)
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(unname(unlist(var_map)), function(cn) is.na(data[[cn]])))
    if (any(any_na)) {
      rlang::abort("adiposity_sds_strat(): missing values present (na_action='error').",
                   class = "healthmarkers_adiposds_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(unname(unlist(var_map)), function(cn) is.na(data[[cn]])))
    data <- data[keep, , drop = FALSE]
    sex_norm <- sex_norm[keep]
  }
  if (nrow(data) == 0L) return(tibble::tibble())

  # Compute SDS
  out_list <- lapply(vars, function(v) {
    stats_M <- ref$M[[v]]
    stats_F <- ref$F[[v]]
    mu  <- ifelse(sex_norm == "M", stats_M[["mean"]], stats_F[["mean"]])
    sdv <- ifelse(sex_norm == "M", stats_M[["sd"]],   stats_F[["sd"]])
    (data[[var_map[[v]]]] - mu) / sdv
  })
  names(out_list) <- paste0(prefix, vars, "_SDS")
  out <- tibble::as_tibble(out_list)

  if (!is.null(id_col)) {
    id_vec <- data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }
  out
}

