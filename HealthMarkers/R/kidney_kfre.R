#' Kidney Failure Risk Equation (KFRE, 2- and 5-year risk)
#'
#' Compute 2- and 5-year risk of end-stage kidney disease using the original
#' 4-variable KFRE (Tangri et al., 2011) with optional data-quality diagnostics,
#' and verbose progress reporting.
#'
#' This function preserves prior behavior by default:
#' - Inputs are taken as-is; NA values propagate to outputs (na_action = "keep").
#' - No capping or out-of-range checks are applied.
#'
#' Units (no automatic conversion):
#' - age: years; sex: 1 = male, 2 = female
#' - eGFR: mL/min/1.73 m^2
#' - UACR: mg/g (albumin-to-creatinine ratio)
#'
#' Details
#' - Prognostic index: `PI = 0.220 x log(age) + (-0.556) x log(eGFR) + 0.451 x log(UACR) + 0.391 x (male)`
#'   where male = 1 if sex == 1, else 0.
#' - Baseline survival: S0(2y) = 0.934, S0(5y) = 0.881 (Tangri 2011).
#' - Risks: KFRE_t = 1 - (S0_t ^ exp(PI)).
#' - The 2016 JAMA study provides a large, multinational validation of the KFRE in humans.
#' - This implementation computes the original 4-variable linear predictor and does not
#'   apply recalibration or alternative coefficient sets.
#'
#' @param data A data.frame or tibble containing at least the columns mapped in `col_map`.
#' @param col_map Named list mapping:
#'   - `age`  -> age in years
#'   - `sex`  -> sex code (1 = male, 2 = female)
#'   - `eGFR` -> estimated GFR (mL/min/1.73 m^2)
#'   - `UACR` -> urine albumin-to-creatinine ratio (mg/g)
#' @param na_action One of c("keep","error","omit","warn"). Default "keep" to preserve previous behavior:
#'   - "keep": propagate NA/NaN through logs and outputs.
#'   - "error": abort if any required input contains missing values.
#'   - "omit": drop rows with NA in required inputs before computation.
#'   - "warn": like "keep" but emits high-missingness warnings.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for high-missingness warnings. Default 0.2.
#' @param verbose Logical; if TRUE, prints stepwise messages and a completion summary. Default TRUE.
#'
#' @return A tibble with:
#' - `KFRE_2yr` risk (0-1) at 2 years
#' - `KFRE_5yr` risk (0-1) at 5 years
#' @seealso [inflammatory_markers()], [iAge()], [impute_missing()]
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   age  = c(65, 72),
#'   sex  = c(1, 2),          # 1 = male, 2 = female
#'   eGFR = c(45, 22),        # mL/min/1.73 m^2
#'   UACR = c(300, 1200)      # mg/g
#' )
#' # Default behavior (NA propagate, no extreme checks)
#' kidney_failure_risk(
#'   data = df,
#'   col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
#' )
#'
#' # With verbose output
#' \donttest{
#' kidney_failure_risk(
#'   data = df,
#'   col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
#'   verbose = TRUE
#' )
#' }
#'
#' @references
#' \insertRef{tangri2011kfre}{HealthMarkers}
#' \insertRef{tangri2016meta}{HealthMarkers}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
kidney_failure_risk <- function(data,
                                col_map = list(
                                  age  = "age",
                                  sex  = "sex",
                                  eGFR = "eGFR",
                                  UACR = "UACR"
                                ),
                                na_action = c("keep", "error", "omit", "warn"),
                                na_warn_prop = 0.2,
                                verbose = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "kidney_failure_risk"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col <- .hm_detect_id_col(data)
  na_action <- match.arg(na_action)
  .kfre_validate_args(data, col_map, na_warn_prop)
  # Optional package-level validation; keep test-facing messages stable
  req_keys <- c("age", "sex", "eGFR", "UACR")
  hm_validate_inputs(data, col_map, required_keys = req_keys, fn = "kidney_failure_risk")
  # Check data columns
  req <- req_keys
  mapped_cols <- unlist(col_map[req_keys], use.names = FALSE)
  missing_cols <- setdiff(mapped_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      message = paste0("missing required columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_kfre_error_missing_columns"
    )
  }
  if (isTRUE(verbose)) {
    map_parts <- vapply(req_keys, function(k) sprintf("%s -> '%s'", k, col_map[[k]]), character(1))
    hm_inform(sprintf("%s(): col_map: %s", fn_name, paste(map_parts, collapse = ", ")), level = "inform")
    hm_inform(sprintf("%s(): computing markers:\n  KFRE_2yr  [age, sex, eGFR, UACR -- 2-year kidney failure risk]\n  KFRE_5yr  [age, sex, eGFR, UACR -- 5-year kidney failure risk]", fn_name), level = "inform")
  }
  # Column classes
  if (!is.numeric(data[[col_map$age]]))  rlang::abort("kidney_failure_risk(): 'age' column must be numeric.",  class = "healthmarkers_kfre_error_nonnumeric_age")
  if (!is.numeric(data[[col_map$eGFR]])) rlang::abort("kidney_failure_risk(): 'eGFR' column must be numeric.", class = "healthmarkers_kfre_error_nonnumeric_egfr")
  if (!is.numeric(data[[col_map$UACR]])) rlang::abort("kidney_failure_risk(): 'UACR' column must be numeric.", class = "healthmarkers_kfre_error_nonnumeric_uacr")
  # Normalize sex: accepts M/F/male/female/1/2/0 -> 1=male, 2=female
  sex_orig <- data[[col_map$sex]]
  sex_norm <- .hm_normalize_sex(sex_orig, to = "12", fn = "kidney_failure_risk")
  unrecognized <- !is.na(sex_orig) & is.na(sex_norm)
  if (any(unrecognized)) {
    rlang::abort(
      "kidney_failure_risk(): 'sex' must be coded as 1=male or 2=female (or M/F/male/female); unrecognized value(s) found.",
      class = "healthmarkers_kfre_error_invalid_sex"
    )
  }
  data[[col_map$sex]] <- sex_norm
  # Missingness policy
  .kfre_warn_high_missing(
    data,
    unlist(col_map[req], use.names = FALSE),
    na_warn_prop = na_warn_prop,
    do_warn_high_missing = identical(na_action, "warn")
  )
  keep <- rep(TRUE, nrow(data))
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(req, function(k) is.na(data[[col_map[[k]]]])))
    if (any(any_na)) {
      rlang::abort("kidney_failure_risk(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_kfre_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(k) is.na(data[[col_map[[k]]]])))
    hm_inform(sprintf("kidney_failure_risk(): omitting %d rows with NA in required inputs", sum(!keep)), level = if (isTRUE(verbose)) "inform" else "debug")
    data <- data[keep, , drop = FALSE]
  }
  # Extract inputs
  age <- data[[col_map$age]]
  sex <- data[[col_map$sex]]
  eGFR <- data[[col_map$eGFR]]
  UACR <- data[[col_map$UACR]]
  # Sex coding (already normalized to 1/2 above)
  male <- ifelse(sex == 1, 1L, 0L)
  # Prognostic index and risk
  pi <- 0.220 * log(age) +
    (-0.556) * log(eGFR) +
    0.451 * log(UACR) +
    0.391 * male
  S0_2 <- 0.934
  S0_5 <- 0.881
  KFRE_2yr <- 1 - (S0_2^exp(pi))
  KFRE_5yr <- 1 - (S0_5^exp(pi))
  out <- tibble::tibble(
    KFRE_2yr = KFRE_2yr,
    KFRE_5yr = KFRE_5yr
  )
  if (!is.null(id_col)) {
    id_vec <- if (na_action == "omit") data[[id_col]][keep] else data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }
  return(out)
}
# ---- internal helpers (not exported) -----------------------------------------
.kfre_validate_args <- function(data, col_map, na_warn_prop) {
  if (!is.data.frame(data)) {
    rlang::abort("kidney_failure_risk(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_kfre_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("kidney_failure_risk(): `col_map` must be a named list.",
                 class = "healthmarkers_kfre_error_colmap_type")
  }
  needed <- c("age","sex","eGFR","UACR")
  missing_keys <- setdiff(needed, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(paste0("kidney_failure_risk(): you must supply col_map entries for: ", paste(missing_keys, collapse = ", ")),
                 class = "healthmarkers_kfre_error_missing_map")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("kidney_failure_risk(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_kfre_error_na_warn_prop")
  }
  invisible(TRUE)
}
.kfre_warn_high_missing <- function(df, cols, na_warn_prop = 0.2, do_warn_high_missing = TRUE) {
  for (cn in cols) {
    x <- df[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    # High-missingness warning only when na_action = 'warn'
    if (isTRUE(do_warn_high_missing) && pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("kidney_failure_risk(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    # Diagnostic warnings for domain issues (always on; tests rely on these)
    if (grepl("eGFR", cn, ignore.case = TRUE)) {
      neg <- sum(is.finite(x) & x <= 0)
      if (neg > 0L) rlang::warn(sprintf("kidney_failure_risk(): '%s' contains %d non-positive values; log() undefined.", cn, neg))
    }
    if (grepl("UACR", cn, ignore.case = TRUE)) {
      nonpos <- sum(is.finite(x) & x <= 0)
      if (nonpos > 0L) rlang::warn(sprintf("kidney_failure_risk(): '%s' contains %d non-positive values; log() undefined.", cn, nonpos))
      if (sum(is.finite(x) & x > 30000) > 0L) {
        rlang::warn(sprintf("kidney_failure_risk(): '%s' has very large values (>30000 mg/g); check units (mg/g vs mg/mmol).", cn))
      }
    }
  }
  invisible(TRUE)
}
.kfre_default_extreme_rules <- function() {
  list(
    age  = c(18, 120),
    eGFR = c(1, 200),     # mL/min/1.73 m^2
    UACR = c(0.1, 10000)  # mg/g
  )
}
.kfre_extreme_scan <- function(age, eGFR, UACR, rules) {
  flags <- list(
    age  = is.finite(age)  & (age  < rules$age[1]  | age  > rules$age[2]),
    eGFR = is.finite(eGFR) & (eGFR < rules$eGFR[1] | eGFR > rules$eGFR[2]),
    UACR = is.finite(UACR) & (UACR < rules$UACR[1] | UACR > rules$UACR[2])
  )
  list(count = sum(flags$age) + sum(flags$eGFR) + sum(flags$UACR), flags = flags)
}
.kfre_cap_inputs <- function(age, eGFR, UACR, flags, rules) {
  if (any(flags$age))  { age[flags$age]   <- pmin(pmax(age[flags$age],   rules$age[1]),  rules$age[2]) }
  if (any(flags$eGFR)) { eGFR[flags$eGFR] <- pmin(pmax(eGFR[flags$eGFR], rules$eGFR[1]), rules$eGFR[2]) }
  if (any(flags$UACR)) { UACR[flags$UACR] <- pmin(pmax(UACR[flags$UACR], rules$UACR[1]), rules$UACR[2]) }
  list(age = age, eGFR = eGFR, UACR = UACR)
}

