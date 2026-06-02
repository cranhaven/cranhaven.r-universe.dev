# File: allostatic_load.R
#'
#' Allostatic Load Index
#'
#' Computes a composite Allostatic Load (AL) score by flagging biomarkers that
#' exceed user-specified high-risk thresholds (strict > when multiple biomarkers;
#' inclusive >= when only one biomarker). Aligned with HM-CS v3: structured
#' validation, diagnostic control, verbose reporting, and optional summary output.
#'
#' @param data data.frame or tibble of numeric biomarker columns.
#' @param thresholds named list of scalar numeric cutoffs (names must match columns).
#' @param col_map optional named list mapping keys in `thresholds` to column names in `data`.
#' @param na_action one of c("keep","omit","error") ("keep" treats NA as zero contribution).
#' @param return_summary logical; TRUE returns list(data, summary, warnings).
#' @param verbose logical; print progress messages via hm_inform() (also gated by options(healthmarkers.verbose)).
#'
#' @return tibble with AllostaticLoad or list when return_summary = TRUE.
#' @details
#' **API pattern note:** Unlike most HealthMarkers functions which follow the
#' standard `(data, col_map, ...)` signature, `allostatic_load()` uses
#' `(data, thresholds, col_map = NULL, ...)` because `thresholds` is a
#' domain-specific required argument that cannot be inferred from column names
#' alone. As a result, this function is not directly dispatchable through the
#' standard `all_health_markers()` registry; a custom wrapper that supplies
#' `thresholds` explicitly would be required.
#' @seealso \code{\link{adiposity_sds}}, \code{\link{adiposity_sds_strat}}
#'
#' @references \insertRef{seeman1997price}{HealthMarkers}
#'
#' @examples
#' df <- tibble::tibble(
#'   SBP = c(118, 142, 130),
#'   DBP = c(76, 92, 85),
#'   CRP = c(1.2, 4.8, 2.1)
#' )
#' thr <- list(SBP = 130, DBP = 85, CRP = 3)
#' allostatic_load(df, thresholds = thr, na_action = "keep", verbose = FALSE)
#'
#' # Single biomarker uses inclusive >= rule
#' allostatic_load(df, thresholds = list(CRP = 3))
#'
#' @export
allostatic_load <- function(
  data,
  thresholds,
  col_map = NULL,
  na_action = c("keep","omit","error"),
  return_summary = FALSE,
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "allostatic_load"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)
  na_action <- match.arg(na_action)

  # ---- Validate inputs ----
  if (!is.data.frame(data)) {
    rlang::abort("allostatic_load(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_allo_error_data_type")
  }
  if (length(thresholds) == 0L) {
    rlang::abort("allostatic_load(): `thresholds` must contain at least one element.",
                 class = "healthmarkers_allo_error_thr_empty")
  }
  if (!is.list(thresholds) || is.null(names(thresholds)) || any(!nzchar(names(thresholds)))) {
    rlang::abort("allostatic_load(): `thresholds` must be a named list of scalar numeric cutoffs.",
                 class = "healthmarkers_allo_error_thr_type")
  }
  if (any(duplicated(names(thresholds)))) {
    rlang::abort("allostatic_load(): duplicate names in `thresholds`.",
                 class = "healthmarkers_allo_error_thr_dupe")
  }
  if (!all(vapply(thresholds, function(x) is.numeric(x) && length(x) == 1L && is.finite(x), logical(1)))) {
    rlang::abort("allostatic_load(): each threshold must be a length-1 finite numeric.",
                 class = "healthmarkers_allo_error_thr_values")
  }

  vars <- names(thresholds)

  # Build effective var -> column mapping with inference support
  cm      <- .hm_build_col_map(data, col_map, keys = vars, fn = fn_name)
  data    <- cm$data
  var_map <- cm$col_map

  # Confirm required columns exist in data
  .hm_log_cols(cm, var_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  AllostaticLoad [%s]", fn_name, paste(vars, collapse = ", ")), level = "inform")
  missing_vars <- setdiff(vars, names(var_map))
  if (length(missing_vars)) {
    rlang::abort(
      paste0("allostatic_load(): missing required columns in data: ", paste(missing_vars, collapse = ", ")),
      class = "healthmarkers_allo_error_missing_columns"
    )
  }
  req_cols <- unname(unlist(var_map[vars], use.names = FALSE))

  # Coerce required columns to numeric; non-finite -> NA
  for (cn in req_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("allostatic_load(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced),
                    class = "healthmarkers_allo_warn_na_coercion")
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # ---- NA row policy (row-wise over required columns) ----
  rows_with_na <- if (length(req_cols)) {
    Reduce(`|`, lapply(req_cols, function(cn) is.na(data[[cn]])))
  } else rep(FALSE, nrow(data))

  if (na_action == "error" && any(rows_with_na)) {
    rlang::abort("allostatic_load(): missing/non-finite values present (na_action='error').",
                 class = "healthmarkers_allo_error_missing_values")
  } else if (na_action == "omit" && any(rows_with_na)) {
    data <- data[!rows_with_na, , drop = FALSE]
  }

  # Early return if no rows remain
  if (nrow(data) == 0L) {
    out0 <- tibble::tibble(AllostaticLoad = integer(0))
    return(if (return_summary) list(
      data = out0,
      summary = list(rows = 0L, biomarkers = length(vars), total_flags = 0L, mean_flags = NA_real_),
      warnings = character()
    ) else out0)
  }

  # ---- Compute flags and sum ----
  inclusive <- length(vars) == 1L
  hm_inform(level = "debug", msg = sprintf("allostatic_load(): computing (rule %s)", if (inclusive) ">=" else ">"))

  flag_cols <- lapply(vars, function(v) {
    cn <- var_map[[v]]
    x <- data[[cn]]
    x[!is.finite(x)] <- NA_real_
    th <- thresholds[[v]]
    as.integer(if (inclusive) x >= th else x > th)
  })

  flag_mat <- if (length(flag_cols)) {
    fm <- do.call(cbind, flag_cols)
    colnames(fm) <- vars
    fm
  } else {
    matrix(integer(0), nrow = nrow(data), ncol = 0)
  }

  out <- tibble::tibble(AllostaticLoad = as.integer(rowSums(flag_mat, na.rm = TRUE)))

  if (!is.null(id_col)) {
    id_vec <- data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  if (isTRUE(return_summary)) {
    return(list(
      data = out,
      summary = list(
        rows = nrow(data),
        biomarkers = length(vars),
        total_flags = sum(out$AllostaticLoad),
        mean_flags = mean(out$AllostaticLoad)
      ),
      warnings = character()
    ))
  }

  out
}

