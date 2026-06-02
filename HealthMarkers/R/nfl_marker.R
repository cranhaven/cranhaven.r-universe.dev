#' Neurofilament Light Chain (NfL) Biomarker
#'
#' Incorporates a neurofilament light chain (NfL) measurement into the analysis pipeline.
#' Placeholder for future NfL-based computations; returns provided values with input checks.
#'
#' @details
#' NfL is released during neuroaxonal injury; elevated levels in CSF or blood indicate
#' neuroaxonal damage and typically increase with age and in neurological diseases.
#' Interpretation requires context-specific and age-adjusted references. This function
#' simply returns the input NfL values (assumed in a single matrix/fluid, e.g., plasma pg/mL)
#' without classification.
#'
#' @param data A data.frame or tibble with an NfL concentration column.
#' @param col_map Named list with `nfl` indicating the NfL column name.
#' @param verbose Logical; if TRUE (default), emits progress messages.
#' @param na_action One of c("keep","omit","error","ignore","warn").
#'
#' @return A tibble with one column: nfl_value (numeric; same units as input).
#'
#' @examples
#' df <- data.frame(NfL = c(8.5, 14.2, 22.1))
#' nfl_marker(df)
#'
#' @references
#' \insertRef{simren2022nfl}{HealthMarkers}
#' \insertRef{disanto2017nfl}{HealthMarkers}
#'
#' @export
nfl_marker <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "nfl_marker"
  .hm_log_input(data, data_name, fn_name, verbose)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff
  id_col <- .hm_detect_id_col(data)

  # --- validate data and col_map (HM-CS style) -------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "nfl_marker(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_nfl_error_data_type"
    )
  }

  cm      <- .hm_build_col_map(data, col_map, "nfl", fn = "nfl_marker")
  data    <- cm$data
  col_map <- cm$col_map
  if (is.null(col_map)) col_map <- list()

  if (!is.list(col_map)) {
    rlang::abort(
      "nfl_marker(): `col_map` must be a named list.",
      class = "healthmarkers_nfl_error_colmap_type"
    )
  }

  if (!("nfl" %in% names(col_map))) {
    # Graceful NA only when nothing provided and inference found nothing
    if (length(cm$user_keys) == 0L && length(cm$inferred_keys) == 0L) {
      return(tibble::tibble(nfl_value = rep(NA_real_, nrow(data))))
    }
    rlang::abort(
      "nfl_marker(): missing or empty `col_map`.",
      class = "healthmarkers_nfl_error_missing_map"
    )
  }

  mapped <- unname(col_map[["nfl"]])
  if (is.null(mapped) || length(mapped) == 0L || !nzchar(as.character(mapped))) {
    rlang::abort(
      "nfl_marker(): missing col_map entry value for: nfl",
      class = "healthmarkers_nfl_error_bad_map_values"
    )
  }

  if (!(mapped %in% names(data))) {
    rlang::abort(
      paste0("nfl_marker(): missing required column in data: ", mapped),
      class = "healthmarkers_nfl_error_missing_columns"
    )
  }

  # --- verbose messages -----------------------------------------------------
  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  nfl_value  [passthrough of NfL input]", fn_name), level = "inform")

  # --- coerce to numeric; warn on NA introduction ---------------------------
  cn <- mapped
  if (!is.numeric(data[[cn]])) {
    old <- data[[cn]]
    suppressWarnings(new <- as.numeric(old))
    intro <- sum(is.na(new) & !is.na(old))
    if (intro > 0L) {
      rlang::warn(
        sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
        class = "healthmarkers_nfl_warn_na_coercion"
      )
    }
    data[[cn]] <- new
  }
  data[[cn]][!is.finite(data[[cn]])] <- NA_real_

  nfl_vals <- data[[cn]]

  # --- NA policy ------------------------------------------------------------
  any_na <- is.na(nfl_vals)

  if (na_action_raw == "warn" && any(any_na)) {
    rlang::warn(
      "Missing NfL values; output will be NA for those entries.",
      class = "healthmarkers_nfl_warn_missing_inputs"
    )
  }

  if (na_action_eff == "error" && any(any_na)) {
    rlang::abort(
      "nfl_marker(): required input contains missing values (na_action='error').",
      class = "healthmarkers_nfl_error_missing_values"
    )
  }

  keep <- if (na_action_eff == "omit") !any_na else rep(TRUE, length(nfl_vals))
  d_nfl <- nfl_vals[keep]

  # --- domain warnings: negative values -------------------------------------
  if (any(is.finite(d_nfl) & d_nfl < 0)) {
    rlang::warn(
      "nfl_marker(): negative NfL values detected; check input data.",
      class = "healthmarkers_nfl_warn_negative_values"
    )
  }

  out <- tibble::tibble(nfl_value = d_nfl)

  # --- pad back if not omitting ---------------------------------------------
  if (na_action_eff != "omit") {
    res <- tibble::tibble(nfl_value = rep(NA_real_, length(nfl_vals)))
    res$nfl_value[keep] <- out$nfl_value
    out <- res
  }

  # --- ID prepend + verbose results -----------------------------------------
  if (!is.null(id_col)) {
    id_vec <- if (na_action_eff == "omit") data[[id_col]][keep] else data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }

  out
}

