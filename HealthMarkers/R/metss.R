
#' Calculate Metabolic Syndrome Severity Score (MetSSS)
#'
#' @title Metabolic Syndrome Severity Score (MetSSS)
#'
#' @description
#' Computes a continuous metabolic syndrome severity z-score using sex- and
#' race-specific standardized components and coefficients (factor-loading style).
#'
#' Behavior note:
#'  - Parameters are selected per-row based on each row's (race, sex) key.
#'  - All unique keys present in the data must have a matching entry in `params`.
#'
#' Required columns (no unit conversion performed):
#'  - waist (cm), bp_sys (mmHg), bp_dia (mmHg)
#'  - TG, HDL_c, glucose (mmol/L)
#'  - sex (1=male, 2=female)
#'  - race (one of "NHW","NHB","HW", or accepted synonyms; "HA" is recognised
#'    by the normaliser but has no default params — see `params` argument)
#'
#' @param data data.frame / tibble.
#' @param params Named list keyed by "RACE_SEX" (e.g. "NHW_M"). Each element:
#'   list(intercept, waist, TG, HDL, glucose, MAP) where each component (except intercept)
#'   is a named numeric vector c(mean=, sd=, coef=). Default parameters are provided
#'   for NHW, NHB, and HW (male and female); no default HA parameters are included
#'   because Gurka et al. (2014) did not publish HA-specific coefficients. Passing
#'   `race = "HA"` with default params will raise an error; supply custom params if needed.
#' @param verbose Logical; if TRUE, prints column mapping and computing messages.
#' @param na_action One of c("keep","omit","error","ignore","warn") for required-input NAs. Default "keep".
#' @param na_warn_prop Proportion (0-1) above which high-missingness warning fires when na_action='warn'. Default 0.2.
#' @param col_map Optional named list mapping canonical keys (`waist`, `TG`, `HDL`,
#'   `glucose`, `MAP`) to actual column names in `data`. If `NULL`, column names
#'   are inferred automatically.
#' @param diagnostics Logical; if TRUE (default) emit value/range diagnostic warnings
#'   (negative, out-of-range checks).
#'
#' @return tibble with one numeric column: MetSSS
#' @export 
#' @references
#' \insertRef{gurka2014metsss}{HealthMarkers}
#' \insertRef{deboer2015metss}{HealthMarkers} (clinical application)
#' \insertRef{gurka2017metss}{HealthMarkers} (clinical application)
#' \insertRef{deboer2018metss}{HealthMarkers} (clinical application)
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @examples
#' df <- data.frame(
#'   waist = 95, bp_sys = 120, bp_dia = 80, TG = 1.5, HDL_c = 1.2,
#'   glucose = 5.5, sex = 1, race = "NHW", age = 45
#' )
#' metss(df)
#' 
metss <- function(data,
                  params = list(
                    NHW_M = list(
                      intercept = -2.344,
                      waist     = c(mean = 94.0, sd = 12.4, coef = 0.846),
                      TG        = c(mean = 1.50, sd = 0.60, coef = 0.701),
                      HDL       = c(mean = 1.10, sd = 0.30, coef = -0.663),
                      glucose   = c(mean = 5.30, sd = 0.60, coef = 0.658),
                      MAP       = c(mean = 97.0, sd = 11.0, coef = 0.466)
                    ),
                    NHW_F = list(
                      intercept = -2.381,
                      waist     = c(mean = 89.7, sd = 14.8, coef = 0.817),
                      TG        = c(mean = 1.28, sd = 0.91, coef = 0.679),
                      HDL       = c(mean = 1.50, sd = 0.40, coef = -0.727),
                      glucose   = c(mean = 5.08, sd = 0.52, coef = 0.622),
                      MAP       = c(mean = 91.0, sd = 11.0, coef = 0.557)
                    ),
                    NHB_M = list(
                      intercept = -2.399,
                      waist     = c(mean = 92.8, sd = 13.1, coef = 0.830),
                      TG        = c(mean = 1.18, sd = 0.75, coef = 0.551),
                      HDL       = c(mean = 1.27, sd = 0.37, coef = -0.598),
                      glucose   = c(mean = 5.55, sd = 0.85, coef = 0.702),
                      MAP       = c(mean = 98.0, sd = 13.0, coef = 0.564)
                    ),
                    NHB_F = list(
                      intercept = -2.395,
                      waist     = c(mean = 96.4, sd = 16.4, coef = 0.858),
                      TG        = c(mean = 1.14, sd = 0.70, coef = 0.570),
                      HDL       = c(mean = 1.36, sd = 0.39, coef = -0.634),
                      glucose   = c(mean = 5.42, sd = 0.84, coef = 0.687),
                      MAP       = c(mean = 95.0, sd = 13.0, coef = 0.577)
                    ),
                    HW_M = list(
                      intercept = -2.377,
                      waist     = c(mean = 98.5, sd = 11.5, coef = 0.864),
                      TG        = c(mean = 1.95, sd = 1.19, coef = 0.724),
                      HDL       = c(mean = 1.13, sd = 0.30, coef = -0.620),
                      glucose   = c(mean = 5.67, sd = 0.90, coef = 0.624),
                      MAP       = c(mean = 97.0, sd = 11.0, coef = 0.448)
                    ),
                    HW_F = list(
                      intercept = -2.388,
                      waist     = c(mean = 97.9, sd = 14.2, coef = 0.858),
                      TG        = c(mean = 1.66, sd = 1.06, coef = 0.715),
                      HDL       = c(mean = 1.29, sd = 0.35, coef = -0.657),
                      glucose   = c(mean = 5.53, sd = 0.87, coef = 0.644),
                      MAP       = c(mean = 91.0, sd = 11.0, coef = 0.512)
                    )
                  ),
                  col_map = NULL,
                  verbose = TRUE,
                  na_action = c("keep","omit","error","ignore","warn"),
                  na_warn_prop = 0.2,
                  diagnostics = TRUE) {

  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "metss"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col <- .hm_detect_id_col(data)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action <- .na$na_action_eff

  .metss_validate_data_frame(data)
  .metss_validate_params(params)

  req <- c("waist","bp_sys","bp_dia","TG","HDL_c","glucose","sex","race")
  cm  <- .hm_build_col_map(data, col_map, keys = req, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map
  miss <- setdiff(req, names(data))
  if (length(miss)) {
    rlang::abort(paste0("metss(): missing required columns: ", paste(miss, collapse = ", ")),
                 class = "healthmarkers_metss_error_missing_columns")
  }

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  MetSSS  [factor-loading z-score, sex/race-specific params]", fn_name), level = "inform")

  # Coerce numerics and clean non-finite
  num_cols <- c("waist","bp_sys","bp_dia","TG","HDL_c","glucose")
  for (cn in num_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0)
        rlang::warn(sprintf("metss(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }
  if (!is.numeric(data$sex)) {
    old <- data$sex
    # Try to interpret character values first
    char_sex <- tolower(trimws(as.character(old)))
    data$sex <- ifelse(char_sex %in% c("m", "male", "1"), 1,
                ifelse(char_sex %in% c("f", "female", "2"), 2, NA_real_))
    introduced <- sum(is.na(data$sex) & !is.na(old))
    if (introduced > 0)
      rlang::warn(sprintf("metss(): 'sex' coerced to numeric; NAs introduced: %d", introduced))
  } else {
    # Already numeric, just ensure it's 1 or 2
    data$sex[!is.finite(data$sex)] <- NA_real_
  }

  # Missingness warnings only when na_action_raw == "warn"
  if (identical(na_action_raw, "warn")) {
    .metss_warn_high_missing(data, req, na_warn_prop = na_warn_prop)
  }
  # Basic domain diagnostics (optional)
  if (isTRUE(diagnostics)) {
    .metss_warn_value_diagnostics(data)
  }

  # NA policy
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (any(any_na))
      rlang::abort("metss(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_metss_error_missing_values")
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    hm_inform(sprintf("metss(): omitting %d rows with NA in required inputs", sum(!keep)), level = if (isTRUE(verbose)) "inform" else "debug")
    data <- data[keep, , drop = FALSE]
  }

  if (nrow(data) == 0L) return(tibble::tibble(MetSSS = numeric()))

  # Per-row key derivation
  key_vec <- .metss_key_from_data(data)
  unique_keys <- unique(key_vec[!is.na(key_vec)])   # NA rows scored as NA, not an error
  missing_keys <- setdiff(unique_keys, names(params))
  if (length(missing_keys)) {
    rlang::abort(
      sprintf("metss(): params missing keys for: %s. Available: %s",
              paste(missing_keys, collapse = ", "),
              paste(names(params), collapse = ", ")),
      class = "healthmarkers_metss_error_missing_param_key"
    )
  }
  if (length(unique_keys) > 1L) {
    rlang::warn(
      sprintf("metss(): multiple sex/race keys detected (%s); computing per-row using matched params.",
              paste(unique_keys, collapse = ", ")),
      class = "healthmarkers_metss_warn_multiple_keys"
    )
  }
  # Compute MetSSS per row

  MAP <- (2 * data$bp_dia + data$bp_sys) / 3
  MetSSS <- rep(NA_real_, nrow(data))

  for (k in unique_keys) {
    idx <- which(key_vec == k)
    p <- params[[k]]
    .metss_validate_param_entry(p, k)

    z_waist <- (data$waist[idx]   - p$waist["mean"])   / p$waist["sd"]
    z_TG    <- (data$TG[idx]      - p$TG["mean"])      / p$TG["sd"]
    z_HDL   <- (data$HDL_c[idx]   - p$HDL["mean"])     / p$HDL["sd"]
    z_glu   <- (data$glucose[idx] - p$glucose["mean"]) / p$glucose["sd"]
    z_MAP   <- (MAP[idx]          - p$MAP["mean"])     / p$MAP["sd"]

    MetSSS[idx] <- p$intercept +
      p$waist["coef"]   * z_waist +
      p$TG["coef"]      * z_TG +
      p$HDL["coef"]     * z_HDL +
      p$glucose["coef"] * z_glu +
      p$MAP["coef"]     * z_MAP
  }

  out <- tibble::tibble(MetSSS = as.numeric(MetSSS))

  if (!is.null(id_col)) {
    out[[id_col]] <- data[[id_col]]
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }
  out
}

# ---- helpers ----
.metss_validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    rlang::abort("metss(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_metss_error_data_type")
  }
  invisible(TRUE)
}
.metss_validate_params <- function(params) {
  if (!is.list(params) || is.null(names(params)) || any(names(params) == "")) {
    rlang::abort("metss(): `params` must be a named list keyed by 'RACE_SEX'.",
                 class = "healthmarkers_metss_error_params_type")
  }
  invisible(TRUE)
}
.metss_validate_param_entry <- function(p, key) {
  needed <- c("intercept","waist","TG","HDL","glucose","MAP")
  miss <- setdiff(needed, names(p))
  if (length(miss))
    rlang::abort(sprintf("metss(): params['%s'] missing entries: %s",
                         key, paste(miss, collapse = ", ")),
                 class = "healthmarkers_metss_error_params_entry_missing")
  if (!(is.numeric(p$intercept) && length(p$intercept) == 1L && is.finite(p$intercept)))
    rlang::abort(sprintf("metss(): params['%s']$intercept must be finite numeric scalar.", key),
                 class = "healthmarkers_metss_error_params_intercept")
  for (comp in c("waist","TG","HDL","glucose","MAP")) {
    v <- p[[comp]]
    if (!(is.numeric(v) && all(c("mean","sd","coef") %in% names(v))))
      rlang::abort(sprintf("metss(): params['%s']$%s must be named numeric with mean, sd, coef.", key, comp),
                   class = "healthmarkers_metss_error_params_component")
    if (v["sd"] <= 0 || any(!is.finite(v[c("mean","sd","coef")])))
      rlang::abort(sprintf("metss(): params['%s']$%s entries must be finite; sd>0.", key, comp),
                   class = "healthmarkers_metss_error_params_component_value")
  }
  invisible(TRUE)
}
.metss_key_from_data <- function(data) {
  race_raw <- toupper(as.character(data$race))
  race_norm <- ifelse(race_raw %in% c("NHW","WHITE"), "NHW",
                 ifelse(race_raw %in% c("NHB","BLACK"), "NHB",
                   ifelse(race_raw %in% c("HW","HISPANIC","HISP","H/L"), "HW",
                     ifelse(race_raw %in% c("HA","ASIAN"), "HA", NA_character_))))
  sex_norm <- .hm_normalize_sex(data$sex, to = "MF")
  # Return NA_character_ when sex or race is NA, so those rows are scored as NA
  ifelse(is.na(sex_norm) | is.na(race_norm), NA_character_,
         paste0(race_norm, "_", sex_norm))
}
.metss_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("metss(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}
.metss_warn_value_diagnostics <- function(df) {
  .chk_nonneg(df$TG,      "TG (mmol/L)")
  .chk_nonneg(df$HDL_c,   "HDL_c (mmol/L)")
  .chk_nonneg(df$glucose, "glucose (mmol/L)")
  .chk_positive(df$waist, "waist (cm)")
  .chk_range(df$bp_sys,   "bp_sys (mmHg)", 60, 300)
  .chk_range(df$bp_dia,   "bp_dia (mmHg)", 30, 200)
  bad_sex <- sum(is.finite(df$sex) & !(df$sex %in% c(1,2)))
  if (bad_sex > 0) rlang::warn(sprintf("metss(): 'sex' has %d values not in {1,2}.", bad_sex))
  race_raw <- toupper(as.character(df$race))
  bad_race <- sum(!(race_raw %in% c("NHW","NHB","HW","HA","WHITE","BLACK","HISPANIC","H/L","ASIAN","OTHER")))
  if (bad_race > 0) hm_inform(level = "debug", msg = sprintf("metss(): 'race' has %d unrecognized values.", bad_race))
  invisible(TRUE)
}
.chk_nonneg <- function(x,label){
  n <- sum(is.finite(x) & x < 0)
  if (n>0) rlang::warn(sprintf("metss(): '%s' has %d negative values; check units.", label, n))
  invisible(TRUE)
}
.chk_positive <- function(x,label){
  n <- sum(is.finite(x) & x <= 0)
  if (n>0) rlang::warn(sprintf("metss(): '%s' has %d non-positive values; check units.", label, n))
  invisible(TRUE)
}
.chk_range <- function(x,label,lo,hi){
  nlo <- sum(is.finite(x) & x < lo); nhi <- sum(is.finite(x) & x > hi)
  if ((nlo+nhi)>0) rlang::warn(sprintf("metss(): '%s' outside [%g,%g] in %d rows; check units.", label, lo, hi, nlo+nhi))
  invisible(TRUE)
}

