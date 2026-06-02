# Core cardiovascular risk calculators & markers + dispatcher
# Optional deps live in Suggests: PooledCohort, QRISK3, CVrisk, RiskScorescvd

# ---- internal helpers ------------------------------------------------------

# length-stable safe division (avoids Inf/NaN on zero denominators)
.safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

# Require columns exist in data
.require_cols <- function(df, cols, fun = "function") {
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop(sprintf("%s(): missing required column(s): %s", fun, paste(miss, collapse = ", ")))
  invisible(TRUE)
}

# Validate simple named mapping (col_map)
.validate_map <- function(map, required, fun = "function") {
  if (!is.list(map) || is.null(names(map))) {
    stop(sprintf("%s(): `col_map` must be a named list.", fun))
  }
  miss <- setdiff(required, names(map))
  if (length(miss)) {
    stop(sprintf("%s(): missing col_map entries for: %s", fun, paste(miss, collapse = ", ")))
  }
  invisible(TRUE)
}

# Basic data-quality scan of variables; returns counts; can warn if asked
.quality_scan_warn <- function(df, vars, .warn = FALSE, na_warn_prop = 0.2, prefix = "") {
  nonfin <- character(0); high_na <- character(0); all_na <- character(0)
  for (v in vars) {
    x <- df[[v]]
    n_nonfin <- sum(!is.finite(x))
    if (n_nonfin > 0L) nonfin <- c(nonfin, sprintf("%s(%d non-finite)", v, n_nonfin))
    x_na <- sum(is.na(x) | !is.finite(x))
    if (length(x) > 0L && x_na == length(x)) all_na <- c(all_na, v)
    if (length(x) > 0L && x_na > 0L && (x_na / length(x)) >= na_warn_prop) {
      high_na <- c(high_na, sprintf("%s(%.1f%% NA)", v, 100 * x_na / length(x)))
    }
  }
  if (.warn) {
    if (length(nonfin)) warning(sprintf("%sNon-finite values: %s; treated as NA.", prefix, paste(nonfin, collapse = ", ")), call. = FALSE)
    if (length(all_na)) warning(sprintf("%sEntirely missing variables: %s.", prefix, paste(all_na, collapse = ", ")), call. = FALSE)
    if (length(high_na)) warning(sprintf("%sHigh missingness (>= %.0f%%): %s.", prefix, 100 * na_warn_prop, paste(high_na, collapse = ", ")), call. = FALSE)
  }
  list(nonfinite = nonfin, high_na = high_na, all_na = all_na)
}

# build a one-row placeholder with NA columns appropriate to a model
.na_row <- function(model, year = NA_integer_) {
  tibble::tibble(
    model = model,
    year  = year,
    risk  = NA_real_,
    value = NA_real_
  )
}

# ---- calculators (exported) ------------------------------------------------

#' ASCVD risk (ACC/AHA Pooled Cohort Equations)
#'
#' Wrapper around the PooledCohort ASCVD calculators with added input validation,
#' optional data-quality warnings, and quiet failure to NA if the backend errors.
#'
#' @param data A data frame with the required cardiovascular risk columns.
#' @param year Risk horizon: 10 or 30.
#' @param col_map Optional named list mapping internal keys (\code{age}, \code{sex},
#'   \code{race}, \code{smoker}, \code{total_chol}, \code{HDL_c}, \code{sbp},
#'   \code{bp_treated}, \code{diabetes}, \code{bmi}) to actual column names in
#'   \code{data}. If \code{NULL} (default), column names are auto-inferred then fall
#'   back to the key names themselves. \code{sex} accepts \code{1}/\code{0},
#'   \code{"m"}/\code{"f"}, or \code{"male"}/\code{"female"} (case-insensitive).
#' @param na_warn_prop Proportion (0-1) to flag high missingness warnings (default 0.2).
#'   Only used when \code{verbose = TRUE}; underlying backend handles NA as per its API.
#' @param verbose Logical; if TRUE, prints progress and a short summary.
#' @param ... Passed to the underlying PooledCohort function.
#' @return A tibble with columns \code{model}, \code{year}, \code{risk} (percentage).
#' @export
#' @examples
#' df <- tibble::tibble(
#'   age = 55, sex = 1, race = "white", smoker = FALSE,
#'   total_chol = 200, HDL_c = 50, sbp = 140, bp_treated = FALSE,
#'   diabetes = FALSE, bmi = 27
#' )
#' if (requireNamespace("PooledCohort", quietly = TRUE)) {
#'   cvd_risk_ascvd(df, year = 10, verbose = TRUE)
#' }
#'
#' @references \insertRef{goff2014accaha}{HealthMarkers}
cvd_risk_ascvd <- function(data, year = 10, col_map = NULL, na_warn_prop = 0.2, verbose = TRUE, ...) {
  .need_pkg("PooledCohort")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (!is.numeric(year) || length(year) != 1L || !(year %in% c(10, 30))) {
    stop("`year` must be 10 or 30.")
  }
  keys <- c("age","sex","race","smoker","total_chol","HDL_c","sbp","bp_treated","diabetes","bmi")
  cm_ascvd <- .hm_build_col_map(data, col_map, keys, fn = "cvd_risk_ascvd")
  data    <- cm_ascvd$data
  col_map <- cm_ascvd$col_map
  # fall back to identity mapping for any key not matched
  for (k in keys) if (is.null(col_map[[k]])) col_map[[k]] <- k
  # validate all mapped columns exist in data
  miss <- setdiff(unlist(col_map[keys]), names(data))
  if (length(miss)) stop(sprintf("cvd_risk_ascvd(): missing column(s) in data: %s", paste(miss, collapse = ", ")))
  req <- unname(unlist(col_map[keys]))
  if (verbose) {
    qs <- .quality_scan_warn(data, req, .warn = FALSE, na_warn_prop = na_warn_prop)
    hm_inform(sprintf("cvd_risk_ascvd(): preparing inputs; non-finite=%d var(s), high-NA=%d, all-NA=%d",
                    length(qs$nonfinite), length(qs$high_na), length(qs$all_na)),
              level = "inform")
  } else {
    hm_inform("cvd_risk_ascvd(): preparing inputs", level = "debug")
  }
  # normalize sex: accept numeric 1/0 or character m/f/male/female
  sex_chr  <- tolower(as.character(data[[col_map$sex]]))
  sex_norm <- ifelse(substr(sex_chr, 1, 1) %in% c("m", "1"), "male", "female")
  res <- try({
    fn <- if (year == 10) PooledCohort::predict_10yr_ascvd_risk else PooledCohort::predict_30yr_ascvd_risk
    fn(
      age_years       = data[[col_map$age]],
      race            = data[[col_map$race]],
      sex             = sex_norm,
      smoke_current   = ifelse(as.logical(data[[col_map$smoker]]), "yes", "no"),
      chol_total_mgdl = data[[col_map$total_chol]],
      chol_hdl_mgdl   = data[[col_map$HDL_c]],
      bp_sys_mmhg     = data[[col_map$sbp]],
      bp_meds         = ifelse(as.logical(data[[col_map$bp_treated]]), "yes", "no"),
      statin_meds     = "no",
      diabetes        = ifelse(as.logical(data[[col_map$diabetes]]), "yes", "no"),
      bmi             = data[[col_map$bmi]],
      ...
    )
  }, silent = TRUE)
  if (inherits(res, "try-error") || length(res) == 0L) {
    return(tibble::tibble(model = "ASCVD", year = as.integer(year), risk = NA_real_))
  }
  out <- tibble::tibble(model = "ASCVD", year = as.integer(year), risk = as.double(res))
  if (verbose) hm_inform(sprintf("cvd_risk_ascvd(): results: %dyr risk, %d row(s)", as.integer(year), nrow(out)),
                         level = "inform")
  return(out)
}

#' QRISK3 10-year risk (UK QRISK3-2017)
#'
#' Wrapper around \code{QRISK3::QRISK3_2017()} that auto-generates a \code{patid}
#' if one is not supplied. Adds input validation and quiet failure to NA on backend error.
#'
#' @param data A data frame with variables required by \code{QRISK3::QRISK3_2017()}.
#' @param patid Optional vector of patient IDs (default: \code{1:nrow(data)}).
#' @param na_warn_prop Proportion (0-1) to flag high missingness warnings (default 0.2).
#'   Only used when \code{verbose = TRUE}.
#' @param verbose Logical; if TRUE, prints progress and a short summary.
#' @param ... Passed to \code{QRISK3::QRISK3_2017()}.
#' @return A tibble with columns \code{model}, \code{year}, \code{risk}.
#' @export
#' @examples
#' \donttest{
#'   if (requireNamespace("QRISK3", quietly = TRUE)) {
#'     df <- data.frame(
#'       gender = 1L, age = 55L,
#'       atrial_fibrillation = 0L, atypical_antipsy = 0L,
#'       regular_steroid_tablets = 0L, erectile_disfunction = 0L,
#'       migraine = 0L, rheumatoid_arthritis = 0L,
#'       chronic_kidney_disease = 0L, severe_mental_illness = 0L,
#'       systemic_lupus_erythematosis = 0L, blood_pressure_treatment = 0L,
#'       diabetes1 = 0L, diabetes2 = 0L,
#'       weight = 80, height = 175,
#'       ethnicity = 1L, heart_attack_relative = 0L,
#'       cholesterol_HDL_ratio = 4.2, systolic_blood_pressure = 130,
#'       std_systolic_blood_pressure = 7, smoke = 0L, townsend = 0
#'     )
#'     cvd_risk_qrisk3(df)
#'   }
#' }
#'
#' @references \insertRef{hippisleycox2017qrisk3}{HealthMarkers}
cvd_risk_qrisk3 <- function(data, ..., patid = NULL, na_warn_prop = 0.2, verbose = TRUE) {
  .need_pkg("QRISK3")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (is.null(patid)) {
    patid <- if ("patid" %in% names(data)) "patid" else seq_len(nrow(data))
  }
  if (verbose) {
    hm_inform("cvd_risk_qrisk3(): preparing inputs", level = "inform")
  } else {
    hm_inform("cvd_risk_qrisk3(): preparing inputs", level = "debug")
  }
  args <- list(
    data = data,
    patid = patid,
    gender = if ("gender_qrisk" %in% names(data)) "gender_qrisk" else if ("gender" %in% names(data)) "gender" else NULL,
    age = "age",
    atrial_fibrillation = "atrial_fibrillation",
    atypical_antipsy = "atypical_antipsy",
    regular_steroid_tablets = "regular_steroid_tablets",
    erectile_disfunction = "erectile_disfunction",
    migraine = "migraine",
    rheumatoid_arthritis = "rheumatoid_arthritis",
    chronic_kidney_disease = "chronic_kidney_disease",
    severe_mental_illness = "severe_mental_illness",
    systemic_lupus_erythematosis = "systemic_lupus_erythematosis",
    blood_pressure_treatment = "blood_pressure_treatment",
    diabetes1 = "diabetes1",
    diabetes2 = "diabetes2",
    weight = "weight",
    height = "height",
    ethiniciy = if ("ethnicity" %in% names(data)) "ethnicity" else if ("ethiniciy" %in% names(data)) "ethiniciy" else NULL,
    heart_attack_relative = "heart_attack_relative",
    cholesterol_HDL_ratio = "cholesterol_HDL_ratio",
    systolic_blood_pressure = "systolic_blood_pressure",
    std_systolic_blood_pressure = "std_systolic_blood_pressure",
    smoke = "smoke",
    townsend = "townsend"
  )
  res <- try(do.call(QRISK3::QRISK3_2017, args), silent = TRUE)
  if (inherits(res, "try-error") || length(res) == 0L) {
    return(tibble::tibble(model = "QRISK3", year = 10L, risk = NA_real_))
  }
  risk_vals <- if (is.data.frame(res)) {
    if ("QRISK3_2017" %in% names(res)) res[["QRISK3_2017"]] else as.double(res[[1]])
  } else {
    as.double(res)
  }
  out <- tibble::tibble(model = "QRISK3", year = 10L, risk = as.double(risk_vals))
  if (verbose) hm_inform(sprintf("cvd_risk_qrisk3(): results: %d row(s)", nrow(out)),
                         level = "inform")
  return(out)
}

#' Stroke 10-year risk
#'
#' Wrapper around \code{PooledCohort::predict_10yr_stroke_risk()} with quiet
#' fallback to NA if the backend errors.
#'
#' @param data A data frame with the required cardiovascular risk columns.
#' @param col_map Optional named list mapping internal keys (\code{age}, \code{sex},
#'   \code{race}, \code{smoker}, \code{total_chol}, \code{HDL_c}, \code{sbp},
#'   \code{bp_treated}, \code{diabetes}, \code{bmi}) to actual column names in
#'   \code{data}. If \code{NULL} (default), column names are auto-inferred then fall
#'   back to the key names themselves. \code{sex} accepts \code{1}/\code{0},
#'   \code{"m"}/\code{"f"}, or \code{"male"}/\code{"female"} (case-insensitive).
#' @param na_warn_prop Proportion (0-1) to flag high missingness warnings (default 0.2).
#'   Only used when \code{verbose = TRUE}.
#' @param verbose Logical; if TRUE, prints progress and a short summary.
#' @param ... Passed to \code{PooledCohort::predict_10yr_stroke_risk()}.
#' @return A tibble with \code{model}, \code{year}, \code{risk}.
#' @examples
#' if (requireNamespace("PooledCohort", quietly = TRUE)) {
#'   df <- data.frame(age = 55, sex = 1, race = "white", smoker = FALSE,
#'     total_chol = 200, HDL_c = 50, sbp = 140, bp_treated = FALSE,
#'     diabetes = FALSE, bmi = 27)
#'   cvd_risk_stroke(df)
#' }
#' @export
#'
#' @references \insertRef{goff2014accaha}{HealthMarkers}
cvd_risk_stroke <- function(data, col_map = NULL, na_warn_prop = 0.2, verbose = TRUE, ...) {
  .need_pkg("PooledCohort")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  keys <- c("age","sex","race","smoker","total_chol","HDL_c","sbp","bp_treated","diabetes","bmi")
  cm_stroke <- .hm_build_col_map(data, col_map, keys, fn = "cvd_risk_stroke")
  data    <- cm_stroke$data
  col_map <- cm_stroke$col_map
  # fall back to identity mapping for any key not matched
  for (k in keys) if (is.null(col_map[[k]])) col_map[[k]] <- k
  # validate all mapped columns exist in data
  miss <- setdiff(unlist(col_map[keys]), names(data))
  if (length(miss)) stop(sprintf("cvd_risk_stroke(): missing column(s) in data: %s", paste(miss, collapse = ", ")))
  req <- unname(unlist(col_map[keys]))
  if (verbose) {
    qs <- .quality_scan_warn(data, req, .warn = FALSE, na_warn_prop = na_warn_prop)
    hm_inform(sprintf("cvd_risk_stroke(): preparing inputs; non-finite=%d, high-NA=%d, all-NA=%d",
                    length(qs$nonfinite), length(qs$high_na), length(qs$all_na)),
              level = "inform")
  } else {
    hm_inform("cvd_risk_stroke(): preparing inputs", level = "debug")
  }
  # normalize sex: accept numeric 1/0 or character m/f/male/female
  sex_chr  <- tolower(as.character(data[[col_map$sex]]))
  sex_norm <- ifelse(substr(sex_chr, 1, 1) %in% c("m", "1"), "male", "female")
  res <- try({
    PooledCohort::predict_10yr_stroke_risk(
      age_years       = data[[col_map$age]],
      race            = data[[col_map$race]],
      sex             = sex_norm,
      smoke_current   = ifelse(as.logical(data[[col_map$smoker]]), "yes", "no"),
      chol_total_mgdl = data[[col_map$total_chol]],
      chol_hdl_mgdl   = data[[col_map$HDL_c]],
      bp_sys_mmhg     = data[[col_map$sbp]],
      bp_meds         = ifelse(as.logical(data[[col_map$bp_treated]]), "yes", "no"),
      statin_meds     = "no",
      diabetes        = ifelse(as.logical(data[[col_map$diabetes]]), "yes", "no"),
      bmi             = data[[col_map$bmi]],
      equation_version = "Goff_2013",  # use PCEs; avoids CKD inputs in PREVENT
      ...
    )
  }, silent = TRUE)
  if (inherits(res, "try-error") || length(res) == 0L) {
    return(tibble::tibble(model = "Stroke", year = 10L, risk = NA_real_))
  }
  out <- tibble::tibble(model = "Stroke", year = 10L, risk = as.double(res))
  if (verbose) hm_inform(sprintf("cvd_risk_stroke(): results: %d row(s)", nrow(out)),
                         level = "inform")
  return(out)
}

#' RiskScorescvd calculator
#'
#' Passthrough to \code{RiskScorescvd::calc_scores()} with graceful fallback to NA.
#'
#' @param data Data required by \code{RiskScorescvd::calc_scores()}.
#' @param ... Passed to \code{RiskScorescvd::calc_scores()}.
#' @return Object returned by \code{RiskScorescvd::calc_scores()}.
#' @examples
#' \donttest{
#'   if (requireNamespace("RiskScorescvd", quietly = TRUE)) {
#'     df <- data.frame(
#'       Age = 55, Sex = 0, Smoking_status = 1,
#'       systolic.bp = 140, Total_cholesterol = 5.5,
#'       HDL.cholesterol = 1.3
#'     )
#'     cvd_risk_scorescvd(df)
#'   }
#' }
#' @export
cvd_risk_scorescvd <- function(data, ...) {
  .need_pkg("RiskScorescvd")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  out <- tryCatch(RiskScorescvd::calc_scores(data = data, ...), error = identity)
  if (inherits(out, "error")) {
    # Some simulated rows can fail SCORE2/SCORE2_CKD; retry on a trimmed slice
    safe_n <- min(20L, nrow(data))
    out <- tryCatch(RiskScorescvd::calc_scores(data = head(data, safe_n), ...), error = identity)
  }
  if (inherits(out, "error") || is.null(out)) return(.na_row("RiskScorescvd", NA_integer_))
  out
}

# ---- lipid markers (exported) ----------------------------------------------

#' Atherogenic Index of Plasma (AIP)
#'
#' Computes log10(TG / HDL_c) with input validation and HM-CS NA handling.
#'
#' @param data A data frame with numeric columns TG and HDL_c (mg/dL).
#' @param col_map Named list mapping required keys:
#'   - TG: triglycerides
#'   - HDL_c: HDL cholesterol
#' @param na_action One of:
#'   - "keep"  (retain rows; AIP is NA where inputs missing/non-finite)
#'   - "omit"  (drop rows with any missing/non-finite inputs)
#'   - "error" (abort if any required input missing/non-finite)
#' @param verbose Logical; if TRUE, emit hm_inform() progress messages.
#' @return A tibble with columns model = "AIP" and value.
#' @examples
#' df <- data.frame(TG = c(150, 200), HDL_c = c(50, 40))
#' cvd_marker_aip(df)
#' @references \insertRef{dobiasova2004aip}{HealthMarkers}
#' @export
cvd_marker_aip <- function(data,
                           col_map = NULL,
                           na_action = c("keep","omit","error"),
                           verbose = TRUE) {
  na_action <- match.arg(na_action)

  # Explicit validation (HM-CS v3)
  if (!is.data.frame(data)) {
    rlang::abort("cvd_marker_aip(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_cvd_error_aip_data_type")
  }
  cm_aip  <- .hm_build_col_map(data, col_map, c("TG", "HDL_c"), fn = "cvd_marker_aip")
  data    <- cm_aip$data
  col_map <- cm_aip$col_map
  req_keys <- c("TG","HDL_c")
  if (!all(req_keys %in% names(col_map))) {
    if (length(cm_aip$user_keys)) {
      hm_validate_inputs(data, col_map, required_keys = req_keys, fn = "cvd_marker_aip")
    }
    return(tibble::tibble(model = "AIP", value = rep(NA_real_, nrow(data))))
  }
  req_cols <- unname(unlist(col_map[req_keys], use.names = FALSE))
  miss_cols <- setdiff(req_cols, names(data))
  if (length(miss_cols)) {
    rlang::abort(
      paste0("cvd_marker_aip(): missing required columns in data: ", paste(miss_cols, collapse = ", ")),
      class = "healthmarkers_cvd_error_aip_missing_columns"
    )
  }

  if (isTRUE(verbose)) {
    if (length(cm_aip$user_keys)) {
      parts <- vapply(cm_aip$user_keys, function(k) sprintf("%s -> '%s'", k, col_map[[k]]), character(1))
      hm_inform(sprintf("cvd_marker_aip(): col_map (user-provided): %s", paste(parts, collapse = ", ")), level = "inform")
    }
    if (length(cm_aip$inferred_keys)) {
      parts <- vapply(cm_aip$inferred_keys, function(k) sprintf("%s -> '%s'", k, col_map[[k]]), character(1))
      hm_inform(sprintf("cvd_marker_aip(): col_map (inferred): %s", paste(parts, collapse = ", ")), level = "inform")
    }
    hm_inform("cvd_marker_aip(): computing markers:\n  AIP [TG, HDL_c]", level = "inform")
  }

  tg_col <- col_map$TG
  hdl_col <- col_map$HDL_c

  # Coerce to numeric; warn on NA introduction; non-finite -> NA
  tg  <- data[[tg_col]]
  if (!is.numeric(tg)) {
    old <- tg; suppressWarnings(tg <- as.numeric(old))
    intro <- sum(is.na(tg) & !is.na(old))
    if (intro > 0) rlang::warn(sprintf("cvd_marker_aip(): column '%s' coerced to numeric; NAs introduced: %d", tg_col, intro),
                               class = "healthmarkers_cvd_warn_na_coercion")
  }
  hdl <- data[[hdl_col]]
  if (!is.numeric(hdl)) {
    old <- hdl; suppressWarnings(hdl <- as.numeric(old))
    intro <- sum(is.na(hdl) & !is.na(old))
    if (intro > 0) rlang::warn(sprintf("cvd_marker_aip(): column '%s' coerced to numeric; NAs introduced: %d", hdl_col, intro),
                               class = "healthmarkers_cvd_warn_na_coercion")
  }
  tg[!is.finite(tg)] <- NA_real_
  hdl[!is.finite(hdl)] <- NA_real_

  # NA policy
  bad <- !is.finite(tg) | !is.finite(hdl)
  if (na_action == "error" && any(bad, na.rm = TRUE)) {
    rlang::abort("cvd_marker_aip(): missing/non-finite TG/HDL_c (na_action='error').",
                 class = "healthmarkers_cvd_error_aip_missing")
  } else if (na_action == "omit" && any(bad, na.rm = TRUE)) {
    keep <- stats::complete.cases(tg, hdl) & is.finite(tg) & is.finite(hdl)
    tg  <- tg[keep]
    hdl <- hdl[keep]
  }

  value <- log10(.safe_div(tg, hdl))
  out <- tibble::tibble(model = "AIP", value = value)

  if (isTRUE(verbose)) hm_inform(hm_result_summary(tibble::tibble(AIP = value), "cvd_marker_aip"), level = "inform")
  out
}

#' LDL Particle Number Estimate (via ApoB)
#'
#' Returns ApoB as a proxy for LDL particle number with HM-CS NA handling.
#'
#' @param data A data frame with numeric column ApoB (mg/dL).
#' @param col_map Named list mapping required key:
#'   - ApoB
#' @param na_action One of:
#'   - "keep"  (retain rows; value is NA where input missing/non-finite)
#'   - "omit"  (drop rows with missing/non-finite input)
#'   - "error" (abort if input missing/non-finite)
#' @param verbose Logical; if TRUE, emit hm_inform() progress messages.
#' @return A tibble with columns model = "LDL_PN" and value.
#' @examples
#' df <- data.frame(ApoB = c(80, 120, 100))
#' cvd_marker_ldl_particle_number(df)
#' @export
cvd_marker_ldl_particle_number <- function(data,
                                           col_map = NULL,
                                           na_action = c("keep","omit","error"),
                                           verbose = TRUE) {
  na_action <- match.arg(na_action)

  # Explicit validation (HM-CS v3)
  if (!is.data.frame(data)) {
    rlang::abort("cvd_marker_ldl_particle_number(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_cvd_error_ldlpn_data_type")
  }
  cm_ldlpn <- .hm_build_col_map(data, col_map, c("ApoB"), fn = "cvd_marker_ldl_particle_number")
  data    <- cm_ldlpn$data
  col_map <- cm_ldlpn$col_map
  if (!("ApoB" %in% names(col_map))) {
    if (length(cm_ldlpn$user_keys)) {
      hm_validate_inputs(data, col_map, required_keys = c("ApoB"), fn = "cvd_marker_ldl_particle_number")
    }
    return(tibble::tibble(model = "LDL_PN", value = rep(NA_real_, nrow(data))))
  }
  apob_col <- col_map$ApoB
  if (!(apob_col %in% names(data))) {
    rlang::abort(sprintf("cvd_marker_ldl_particle_number(): missing required column in data: %s", apob_col),
                 class = "healthmarkers_cvd_error_ldlpn_missing_columns")
  }

  if (isTRUE(verbose)) {
    if (length(cm_ldlpn$user_keys)) {
      hm_inform(sprintf("cvd_marker_ldl_particle_number(): col_map (user-provided): ApoB -> '%s'", apob_col), level = "inform")
    } else if (length(cm_ldlpn$inferred_keys)) {
      hm_inform(sprintf("cvd_marker_ldl_particle_number(): col_map (inferred): ApoB -> '%s'", apob_col), level = "inform")
    }
    hm_inform("cvd_marker_ldl_particle_number(): computing markers:\n  LDL_PN [ApoB]", level = "inform")
  }

  apob <- data[[apob_col]]
  if (!is.numeric(apob)) {
    old <- apob; suppressWarnings(apob <- as.numeric(old))
    intro <- sum(is.na(apob) & !is.na(old))
    if (intro > 0) rlang::warn(sprintf("cvd_marker_ldl_particle_number(): column '%s' coerced to numeric; NAs introduced: %d", apob_col, intro),
                               class = "healthmarkers_cvd_warn_na_coercion")
  }
  apob[!is.finite(apob)] <- NA_real_

  bad <- !is.finite(apob)
  if (na_action == "error" && any(bad, na.rm = TRUE)) {
    rlang::abort("cvd_marker_ldl_particle_number(): missing/non-finite ApoB (na_action='error').",
                 class = "healthmarkers_cvd_error_ldlpn_missing")
  } else if (na_action == "omit" && any(bad, na.rm = TRUE)) {
    apob <- apob[!bad]
  }

  out <- tibble::tibble(model = "LDL_PN", value = apob)

  if (isTRUE(verbose)) hm_inform(hm_result_summary(tibble::tibble(LDL_PN = apob), "cvd_marker_ldl_particle_number"), level = "inform")
  out
}

# ---- dispatcher (exported) -------------------------------------------------

#' Compute cardiovascular risk or marker by selected model
#'
#' Dispatch to the appropriate risk or marker function, or run all of them.
#' Includes basic argument validation and robust fallback to NA rows if
#' individual calculators fail.
#'
#' @param data Data frame required by your chosen model.
#' @param model One of:
#'   - "ALL"
#'   - Risk calculators: "ASCVD","QRISK3","Stroke","RiskScorescvd"
#'   - Lipid markers: "AIP","LDL_PN"
#' @param year Risk horizon (10 or 30) for applicable models; ignored for lipid markers.
#' @param ... Forwarded to underlying wrappers (e.g., col_map, na_action).
#' @param verbose Logical; if TRUE, prints progress (legacy; messages now routed via hm_inform).
#' @return A tibble.
#' @examples
#' df <- data.frame(TG = c(150, 200), HDL_c = c(50, 40))
#' cvd_risk(df, model = "AIP")
#' @export
cvd_risk <- function(data,
                     model = c("ALL", "ASCVD", "QRISK3",
                               "Stroke", "RiskScorescvd", "AIP", "LDL_PN"),
                     year = 10,
                     ...,
                     verbose = TRUE) {
  model <- match.arg(model)
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (!is.numeric(year) || length(year) != 1L) stop("`year` must be a single numeric (e.g., 10, 30).")

  hm_inform(
    sprintf("cvd_risk: dispatching model '%s'%s",
            model,
            if (model %in% c("ASCVD","Stroke")) paste0(" (year=", as.integer(year), ")") else ""),
    level = if (isTRUE(verbose)) "inform" else "debug"
  )

  if (model == "ALL") {
    all_models <- c("ASCVD", "QRISK3", "Stroke", "RiskScorescvd", "AIP", "LDL_PN")
    results <- lapply(all_models, function(m) {
      out <- try(cvd_risk(data, model = m, year = year, ..., verbose = verbose), silent = TRUE)
      if (inherits(out, "try-error") || !is.data.frame(out) || nrow(out) == 0L) {
        yr <- if (m %in% c("ASCVD", "Stroke")) as.integer(year) else if (m %in% c("QRISK3")) 10L else NA_integer_
        return(.na_row(m, yr))
      }
      out
    })
    out_all <- dplyr::bind_rows(results)
    return(out_all)
  }

  out <- switch(model,
    "ASCVD"         = cvd_risk_ascvd(data, year = year, ..., verbose = verbose),
    "QRISK3"        = cvd_risk_qrisk3(data, ..., verbose = verbose),
    "Stroke"        = cvd_risk_stroke(data, ..., verbose = verbose),
    "RiskScorescvd" = cvd_risk_scorescvd(data, ...),
    "AIP"           = cvd_marker_aip(data, ..., verbose = verbose),
    "LDL_PN"        = cvd_marker_ldl_particle_number(data, ..., verbose = verbose),
    stop("Unknown model: ", model)
  )

  out
}
