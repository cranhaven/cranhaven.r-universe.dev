
#' Calculate metabolic risk feature flags (pediatric-friendly thresholds)
#'
#' Compute four binary risk flags from routine clinical measures:
#' - dyslipidemia
#' - insulin_resistance
#' - hyperglycemia (prediabetes-range glycemia)
#' - hypertension (BP >=95th percentile via z > 1.64)
#'
#' By default, behavior matches prior implementation: required columns are
#' validated, NA values are kept (propagate to outputs), no extreme-value
#' checks or capping are applied, and a tibble with 0/1 factor flags is returned.
#'
#' Units and criteria (no automatic unit conversion):
#' - Lipids (mmol/L): total cholesterol > 5.2 OR LDL-C > 3.4 OR HDL-C < 1.0 OR
#'   triglycerides > 1.1 (age 0-9) OR > 1.5 (age 10-19) => dyslipidemia = 1.
#'   Note: no TG cutoff is applied for adults aged >= 20 years.
#' - Insulin resistance: z_HOMA > 1.28 (~=90th percentile) => insulin_resistance = 1.
#'   z_HOMA is a within-sample or external z-score of HOMA-IR (see Matthews et al. 1985).
#' - Hyperglycemia: fasting glucose in (5.6, 6.9) mmol/L OR HbA1c in (39, 47) mmol/mol
#'   => hyperglycemia = 1. Boundaries are EXCLUSIVE (open intervals); boundary values
#'   (exactly 5.6 or 6.9 mmol/L; exactly 39 or 47 mmol/mol) are not flagged.
#'   ADA criteria use inclusive lower bound (>= 5.6 mmol/L, >= 39 mmol/mol).
#' - Hypertension: either BP z-score > 1.64 (~=95th percentile) for systolic or diastolic
#'   => hypertension = 1.
#'
#' @param data A data.frame or tibble containing at least these numeric columns:
#'   - chol_total, chol_ldl, chol_hdl, triglycerides (mmol/L)
#'   - age_year (years)
#'   - z_HOMA (standardized HOMA-IR)
#'   - glucose (mmol/L)
#'   - HbA1c (mmol/mol; IFCC units)
#'   - bp_sys_z, bp_dia_z (BP z-scores)
#' @param col_map Optional named list to map required keys to column names in `data`.
#'   Keys: c("chol_total","chol_ldl","chol_hdl","triglycerides","age_year",
#'   "z_HOMA","glucose","HbA1c","bp_sys_z","bp_dia_z"). Default NULL (use same names).
#' @param na_action One of c("keep","omit","error","ignore","warn") controlling missing-data policy.
#'   - "keep": keep NA; outputs become NA where inputs are NA.
#'   - "omit": drop rows with NA in any required input.
#'   - "error": abort if any required input contains NA.
#'   - "ignore"/"warn": aliases of "keep"; "warn" also emits missingness diagnostics.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for high-missingness warnings. Default 0.2.
#' @param verbose Logical; if TRUE, prints column mapping and computing messages.
#'
#' @return A tibble with four factor columns (levels c("0","1")):
#' - dyslipidemia
#' - insulin_resistance
#' - hyperglycemia
#' - hypertension
#'
#' @note These flags are heuristic screening rules derived from published clinical
#'   guidelines. They are not validated diagnostic criteria and should not replace
#'   clinical judgment. The dyslipidemia and hypertension thresholds are designed
#'   for pediatric populations (ages 0-19); for adults >= 20, only TC, LDL-C, and
#'   HDL-C criteria contribute to the dyslipidemia flag.
#'
#' @seealso [liver_markers()], [lipid_markers()], [kidney_failure_risk()], [inflammatory_markers()]
#'
#' @references
#' \insertRef{nhlbi2011lipids}{HealthMarkers}
#' \insertRef{ada2024standards}{HealthMarkers}
#' \insertRef{flynn2017bp}{HealthMarkers}
#' \insertRef{matthews1985homa}{HealthMarkers}
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang abort warn inform
#' @importFrom dplyr transmute if_else
#' @importFrom tibble as_tibble
#'
#' @examples
#' df <- data.frame(
#'   chol_total = c(5.2, 6.4), chol_ldl = c(3.2, 4.1), chol_hdl = c(1.3, 1.0),
#'   triglycerides = c(1.8, 2.5), age_year = c(45, 60), z_HOMA = c(0.5, 1.2),
#'   glucose = c(5.5, 6.8), HbA1c = c(38, 46), bp_sys_z = c(0.2, 1.1),
#'   bp_dia_z = c(0.1, 0.9)
#' )
#' metabolic_risk_features(df)
#' @export
metabolic_risk_features <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  na_warn_prop = 0.2,
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "metabolic_risk_features"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col <- .hm_detect_id_col(data)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action <- .na$na_action_eff

  .mrf_validate_args(data, col_map, na_warn_prop)

  # Required keys and column mapping
  required_cols <- c(
    "chol_total", "chol_ldl", "chol_hdl", "triglycerides",
    "age_year", "z_HOMA", "glucose", "HbA1c", "bp_sys_z", "bp_dia_z"
  )

  cm      <- .hm_build_col_map(data, col_map, required_cols, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # Strict check: error if user-provided key points to column not in data
  if (length(cm$user_keys)) {
    bad_cols <- setdiff(unlist(col_map[cm$user_keys], use.names = FALSE), names(data))
    if (length(bad_cols))
      rlang::abort(paste0("metabolic_risk_features(): mapped columns not found in data: ", paste(bad_cols, collapse = ", ")),
                   class = "healthmarkers_mrf_error_missing_columns")
  }

  miss_keys <- setdiff(required_cols, names(col_map))
  if (length(miss_keys)) {
    rlang::abort(
      paste0("metabolic_risk_features(): missing col_map entries for: ", paste(miss_keys, collapse = ", ")),
      class = "healthmarkers_mrf_error_missing_map")
  }

  # Ensure mapped columns exist
  mapped_cols <- unlist(col_map[required_cols], use.names = FALSE)
  miss_cols <- setdiff(mapped_cols, names(data))
  if (length(miss_cols)) {
    rlang::abort(
      paste0("metabolic_risk_features(): mapped columns not found in data: ", paste(miss_cols, collapse = ", ")),
      class = "healthmarkers_mrf_error_missing_columns"
    )
  }

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf(
      "%s(): computing markers:\n  dyslipidemia       [chol_total, chol_ldl, chol_hdl, triglycerides, age_year]\n  insulin_resistance [z_HOMA]\n  hyperglycemia      [glucose, HbA1c]\n  hypertension       [bp_sys_z, bp_dia_z]",
      fn_name), level = "inform")

  # HM-CS v2: numeric coercion for required inputs; warn if NAs introduced; set non-finite to NA
  for (cn in mapped_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("metabolic_risk_features(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High missingness diagnostics only when na_action_raw == "warn"
  if (identical(na_action_raw, "warn")) {
    .mrf_warn_high_missing(data, mapped_cols, na_warn_prop = na_warn_prop)
  }

  # NA policy
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(required_cols, function(k) is.na(data[[col_map[[k]]]])))
    if (any(any_na)) {
      rlang::abort("metabolic_risk_features(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_mrf_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(required_cols, function(k) is.na(data[[col_map[[k]]]])))
    hm_inform(sprintf("metabolic_risk_features(): omitting %d rows with NA in required inputs", sum(!keep)), level = if (isTRUE(verbose)) "inform" else "debug")
    data <- data[keep, , drop = FALSE]
  }

  # Shorthand for mapped columns
  CT  <- data[[col_map$chol_total]]
  LDL <- data[[col_map$chol_ldl]]
  HDL <- data[[col_map$chol_hdl]]
  TG  <- data[[col_map$triglycerides]]
  AGE <- data[[col_map$age_year]]
  ZH  <- data[[col_map$z_HOMA]]
  GLU <- data[[col_map$glucose]]
  A1c <- data[[col_map$HbA1c]]
  SBPz <- data[[col_map$bp_sys_z]]
  DBPz <- data[[col_map$bp_dia_z]]

  # Compute flags
  out <- dplyr::transmute(
    data,
    dyslipidemia = factor(
      dplyr::if_else(
        CT > 5.2 |
          LDL > 3.4 |
          HDL < 1.0 |
          (TG > 1.1 & AGE %in% 0:9) |
          (TG > 1.5 & AGE %in% 10:19),
        1L, 0L, missing = NA_integer_
      ),
      levels = c(0L, 1L)
    ),
    insulin_resistance = factor(
      dplyr::if_else(ZH > 1.28, 1L, 0L, missing = NA_integer_),
      levels = c(0L, 1L)
    ),
    hyperglycemia = factor(
      dplyr::if_else(
        (GLU > 5.6 & GLU < 6.9) |
          (A1c > 39 & A1c < 47),
        1L, 0L, missing = NA_integer_
      ),
      levels = c(0L, 1L)
    ),
    hypertension = factor(
      dplyr::if_else(SBPz > 1.64 | DBPz > 1.64, 1L, 0L, missing = NA_integer_),
      levels = c(0L, 1L)
    )
  )

  out <- tibble::as_tibble(out)
  if (!is.null(id_col)) {
    out[[id_col]] <- data[[id_col]]
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }
  out
}

# ---- internal helpers (not exported) -----------------------------------------

.mrf_validate_args <- function(data, col_map, na_warn_prop) {
  if (!is.data.frame(data)) {
    rlang::abort("metabolic_risk_features(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_mrf_error_data_type")
  }
  if (!is.null(col_map)) {
    if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
      rlang::abort("metabolic_risk_features(): `col_map` must be a named list when supplied.",
                   class = "healthmarkers_mrf_error_colmap_type")
    }
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L &&
        is.finite(na_warn_prop) && na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("metabolic_risk_features(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_mrf_error_na_warn_prop")
  }
  invisible(TRUE)
}

.mrf_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("metabolic_risk_features(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    if (grepl("chol|triglycerides", cn, ignore.case = TRUE)) {
      if (sum(is.finite(x) & x > 30) > 0L) {
        rlang::warn(sprintf("metabolic_risk_features(): '%s' has very large values (>30 mmol/L); check units (mmol/L expected).", cn))
      }
      if (sum(is.finite(x) & x < 0) > 0L) {
        rlang::warn(sprintf("metabolic_risk_features(): '%s' contains negative values; check input.", cn))
      }
    }
    if (identical(cn, "HbA1c")) {
      if (sum(is.finite(x) & x <= 14) > 0L) {
        rlang::warn("metabolic_risk_features(): 'HbA1c' appears to be in percent for some rows; expected mmol/mol.")
      }
    }
    if (identical(cn, "glucose")) {
      if (sum(is.finite(x) & x > 40) > 0L) {
        rlang::warn("metabolic_risk_features(): 'glucose' values > 40 mmol/L detected; check units.")
      }
      if (sum(is.finite(x) & x < 0) > 0L) {
        rlang::warn("metabolic_risk_features(): 'glucose' has negative values; check input.")
      }
    }
    if (grepl("bp_.*_z$", cn)) {
      if (sum(is.finite(x) & (x < -5 | x > 5)) > 0L) {
        rlang::warn(sprintf("metabolic_risk_features(): '%s' z-scores outside [-5,5]; check scaling.", cn))
      }
    }
  }
  invisible(TRUE)
}

.mrf_default_extreme_rules <- function() {
  list(
    chol_total   = c(0.5, 20),
    chol_ldl     = c(0.1, 15),
    chol_hdl     = c(0.1, 5),
    triglycerides= c(0.1, 30),
    age_year     = c(0, 120),
    z_HOMA       = c(-5, 5),
    glucose      = c(2, 40),
    HbA1c        = c(15, 200),
    bp_sys_z     = c(-5, 5),
    bp_dia_z     = c(-5, 5)
  )
}

.mrf_extreme_scan <- function(df, col_map, rules, required) {
  count <- 0L
  flags <- list()
  for (nm in intersect(names(rules), required)) {
    cn <- col_map[[nm]]
    if (!cn %in% names(df)) next
    x <- df[[cn]]
    rng <- rules[[nm]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

.mrf_cap_inputs <- function(df, flags, col_map, rules) {
  for (cn in names(flags)) {
    rn <- names(col_map)[match(cn, unlist(col_map, use.names = FALSE))]; rn <- rn[!is.na(rn)][1]
    if (is.na(rn) || is.null(rules[[rn]])) next
    rng <- rules[[rn]]; x <- df[[cn]]; bad <- flags[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

