#' Kynurenine/Tryptophan Ratio (KTR)
#'
#' Computes the ratio of kynurenine to tryptophan, a marker of IDO activity and immune activation.
#'
#' @details
#' KTR is calculated as Kyn (nmol/L) divided by Trp (mumol/L). Elevated KTR indicates
#' increased tryptophan catabolism via the kynurenine pathway, often reflecting
#' inflammation and cell-mediated immune activation.
#'
#' Inputs should already be in Kyn (nmol/L) and Trp (mumol/L).
#'
#' @param data A data.frame or tibble with kynurenine and tryptophan concentrations.
#' @param col_map Named list with:
#'   - kynurenine: column for kynurenine (nmol/L)
#'   - tryptophan: column for tryptophan (mumol/L)
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param verbose Logical; if `TRUE` (default), prints column mapping and a
#'   per-column results summary.
#' @return A tibble with one column: kyn_trp_ratio (numeric). If an ID column
#'   is detected, it is prepended.
#'
#' @examples
#' # columns named exactly as the required keys (auto-detected)
#' df <- data.frame(kynurenine = c(2500, 3100, 2700), tryptophan = c(55, 48, 62))
#' kyn_trp_ratio(df, verbose = FALSE)
#'
#' # non-standard column names require explicit col_map
#' df2 <- data.frame(Kyn_nM = c(2500, 3100, 2700), Trp_uM = c(55, 48, 62))
#' kyn_trp_ratio(df2, col_map = list(kynurenine = "Kyn_nM", tryptophan = "Trp_uM"),
#'              verbose = FALSE)
#'
#' @references
#' \insertRef{fuchs1998ktr}{HealthMarkers};
#' \insertRef{damerell2025kyn}{HealthMarkers} (clinical application in colorectal cancer)
#'
#' @export
kyn_trp_ratio <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name  <- "kyn_trp_ratio"
  .hm_log_input(data, data_name, fn_name, verbose)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff
  id_col   <- .hm_detect_id_col(data)

  # Validate
  if (!is.data.frame(data)) {
    rlang::abort("kyn_trp_ratio(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_ktr_error_data_type")
  }

  req <- c("kynurenine","tryptophan")
  cm      <- .hm_build_col_map(data, col_map, req, fn = "kyn_trp_ratio")
  data    <- cm$data
  col_map <- cm$col_map
  if (is.null(col_map)) col_map <- list()

  # Graceful NA only when nothing provided and inference found nothing
  if (length(cm$user_keys) == 0L && length(cm$inferred_keys) == 0L && length(col_map) == 0L) {
    return(tibble::tibble(kyn_trp_ratio = rep(NA_real_, nrow(data))))
  }

  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("kyn_trp_ratio(): `col_map` must be a named list.",
                 class = "healthmarkers_ktr_error_colmap_type")
  }

  req <- c("kynurenine","tryptophan")
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(paste0("kyn_trp_ratio(): missing col_map entries for: ",
                        paste(missing_keys, collapse = ", ")),
                 class = "healthmarkers_ktr_error_missing_map")
  }
  mapped <- unname(unlist(col_map[req], use.names = FALSE))
  if (any(!nzchar(mapped))) {
    bad <- req[!nzchar(unname(unlist(col_map[req])))]
    rlang::abort(paste0("kyn_trp_ratio(): missing col_map entries for: ",
                        paste(bad, collapse = ", ")),
                 class = "healthmarkers_ktr_error_bad_map_values")
  }
  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(paste0("kyn_trp_ratio(): missing required columns in data: ",
                        paste(missing_cols, collapse = ", ")),
                 class = "healthmarkers_ktr_error_missing_columns")
  }

  hm_inform("kyn_trp_ratio(): preparing inputs", level = if (isTRUE(verbose)) "inform" else "debug")

  # --- Verbose: col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: computing markers list
  if (isTRUE(verbose)) {
    hm_inform(
      sprintf("%s(): computing markers:\n  kyn_trp_ratio  [kynurenine (nmol/L) / tryptophan (mumol/L)]", fn_name),
      level = "inform"
    )
  }

  # Coerce numeric; warn if NAs introduced; sanitize
  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0L) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
                    class = "healthmarkers_ktr_warn_na_coercion")
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  kyn <- data[[col_map$kynurenine]]
  trp <- data[[col_map$tryptophan]]

  # NA policy
  any_na <- is.na(kyn) | is.na(trp)
  if (na_action_raw == "warn" && any(any_na)) {
    rlang::warn("Missing kynurenine or tryptophan; ratio will be NA for those entries.",
                class = "healthmarkers_ktr_warn_missing_inputs")
  }
  if (na_action_eff == "error" && any(any_na)) {
    rlang::abort("kyn_trp_ratio(): required inputs contain missing values (na_action='error').",
                 class = "healthmarkers_ktr_error_missing_values")
  }
  keep <- if (na_action_eff == "omit") !any_na else rep(TRUE, length(kyn))

  d_kyn <- kyn[keep]
  d_trp <- trp[keep]

  # Domain warnings: nonpositive tryptophan
  if (any(is.finite(d_trp) & d_trp <= 0)) {
    rlang::warn("kyn_trp_ratio(): nonpositive tryptophan detected; ratios set to NA for those rows.",
                class = "healthmarkers_ktr_warn_nonpositive_trp")
    d_trp[is.finite(d_trp) & d_trp <= 0] <- NA_real_
  }

  hm_inform("kyn_trp_ratio(): computing", level = "debug")

  ratio <- d_kyn / d_trp
  ratio[!is.finite(ratio)] <- NA_real_

  # High ratio warning
  if (any(is.finite(ratio) & ratio > 100)) {
    rlang::warn("kyn_trp_ratio(): very high ratios (>100) detected; check units (Kyn nmol/L, Trp mumol/L).",
                class = "healthmarkers_ktr_warn_ratio_high")
  }

  out <- tibble::tibble(kyn_trp_ratio = ratio)

  # Pad back to full length when not omitting (preserve row alignment)
  if (na_action_eff != "omit") {
    res <- tibble::tibble(kyn_trp_ratio = rep(NA_real_, length(kyn)))
    res$kyn_trp_ratio[keep] <- out$kyn_trp_ratio
    out <- res
  }

  # --- Prepend ID column if detected
  if (!is.null(id_col)) {
    out[[id_col]] <- if (na_action_eff == "omit") data[[id_col]][keep] else data[[id_col]]
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }

  # --- Verbose: results summary
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}

