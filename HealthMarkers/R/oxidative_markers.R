#' Oxidative stress markers
#'
#' Computes GSH_GSSG_Ratio = reduced glutathione (GSH) / oxidized glutathione (GSSG).
#'
#' @param data Data frame with columns for GSH and GSSG (per col_map).
#' @param col_map Named list with required keys `GSH` and `GSSG`. Defaults assume
#'   column names match keys. Both columns must be in the same units (e.g., µmol/L).
#' @param na_action One of c("keep","omit","error").
#' @param verbose Logical; if `TRUE` (default), prints column mapping and a
#'   per-column results summary.
#' @return A tibble with column GSH_GSSG_Ratio. If an ID column is detected,
#'   it is prepended.
#'
#' @note
#' `GSH_GSSG_Ratio` is dimensionless only when `GSH` and `GSSG` are supplied in
#' the same units (typically µmol/L). The formula `GSH / GSSG` is a standard
#' biochemical redox ratio; no unit conversion is applied.
#'
#' @references
#' \insertRef{glutathione_redox_review}{HealthMarkers} (background review;
#'   GSH/GSSG is a standard biochemical redox ratio, not a formula from this paper)
#'
#' @examples
#' df <- data.frame(GSH = c(5, 3), GSSG = c(1, 0.5))
#' oxidative_markers(df, col_map = list(GSH="GSH", GSSG="GSSG"))
#'
#' @importFrom rlang abort warn inform
#' @importFrom tibble tibble
#' @export
oxidative_markers <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error"),
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "oxidative_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)
  id_col    <- .hm_detect_id_col(data)

  # HM-CS v2 validation
  hm_validate_inputs(
    data, col_map,
    required_keys = character(0),
    fn = "oxidative_markers"
  )
  cm      <- .hm_build_col_map(data, col_map, keys = c("GSH", "GSSG"), fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # Ensure mapped columns exist in data
  mapped <- unname(unlist(col_map[c("GSH","GSSG")], use.names = FALSE))
  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("oxidative_markers(): missing columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_oxidative_error_missing_columns"
    )
  }

  # Coerce required columns to numeric; warn if NAs introduced; set non-finite to NA
  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("oxidative_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug)
  for (cn in mapped) {
    x <- data[[cn]]; n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= 0.2 && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("oxidative_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # --- Verbose: column mapping
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: computing markers list
  if (isTRUE(verbose)) {
    hm_inform(
      sprintf("%s(): computing markers:\n  GSH_GSSG_Ratio  [GSH / GSSG]", fn_name),
      level = "inform"
    )
  }

  # NA policy
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("oxidative_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_oxidative_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (sum(!keep) > 0L)
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep)),
                level = if (isTRUE(verbose)) "inform" else "debug")
    data <- data[keep, , drop = FALSE]
  }

  hm_inform("oxidative_markers(): computing markers", level = "debug")

  # Accessors (post-filter)
  GSH  <- data[[col_map$GSH]]
  GSSG <- data[[col_map$GSSG]]

  # Safe division: NA on non-finite or zero denom
  sdiv <- function(a, b) {
    out <- a / b
    out[!is.finite(out)] <- NA_real_
    out
  }

  out <- tibble::tibble(GSH_GSSG_Ratio = sdiv(GSH, GSSG))

  # --- Prepend ID column if detected
  if (!is.null(id_col)) {
    id_vec        <- data[[id_col]][seq_len(nrow(out))]
    out[[id_col]] <- id_vec
    out           <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out           <- tibble::as_tibble(out)
  }

  # --- Verbose: results summary
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}
