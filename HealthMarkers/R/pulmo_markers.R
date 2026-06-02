
#' Calculate pulmonary function markers (FEV1/FVC, z-scores, percent predicted, LLN, etc.)
#'
#' Uses the `rspiro` reference equations to compute predicted normals,
#' z-scores, percent predicted and lower limits of normal (LLN) for FEV1, FVC,
#' and the FEV1/FVC ratio.
#'
#' Inputs are validated, missingness is handled via `na_action`, and heights are
#' auto-detected as cm when any height > 3; otherwise interpreted as metres (no
#' automatic unit conversion beyond that heuristic, preserving prior behavior).
#'
#' @param data A data.frame or tibble with columns:
#'   - `age`       (numeric): years
#'   - `sex`       (character or numeric): "male"/"female" (case-insensitive) or codes 1/2 or 0/1
#'   - `height`    (numeric): cm or m (auto-detected)
#'   - `ethnicity` (character): e.g. "Caucasian", "African-American", "NE Asian", "SE Asian", "Other/Mixed"
#'   - `fev1`      (numeric): observed FEV1 in L
#'   - `fvc`       (numeric): observed FVC in L
#' @param equation One of `c("GLI","GLIgl","NHANES3")` (see `rspiro` for details).
#'   GLIgl ignores ethnicity.
#' @param na_action One of `c("keep","omit","error")` for handling missing values in
#'   required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness warnings on
#'   required inputs. Default 0.2.
#' @param col_map Optional named list mapping canonical keys (`age`, `sex`, `height`,
#'   `ethnicity`, `fev1`, `fvc`) to actual column names in `data`. If `NULL`,
#'   column names are inferred automatically.
#' @param verbose Logical; if `TRUE` prints progress messages and a completion summary.
#'
#' @return A tibble with columns:
#'   - `fev1_pred`, `fev1_z`,   `fev1_pctpred`, `fev1_LLN`
#'   - `fvc_pred`,  `fvc_z`,    `fvc_pctpred`,  `fvc_LLN`
#'   - `fev1_fvc_ratio`, `fev1_fvc_pred`, `fev1_fvc_z`, `fev1_fvc_pctpred`,
#'     `fev1_fvc_LLN` (`NA` if the equation lacks native FEV1/FVC support in rspiro)
#'
#' @seealso rspiro
#'
#' @note
#' `fev1_fvc_z`, `fev1_fvc_pred`, and `fev1_fvc_LLN` are computed via
#' rspiro's native FEV1/FVC parameter (equivalent to `param = "FEV1FVC"`).
#' If that parameter is not supported by the installed rspiro version or
#' equation, these columns fall back gracefully: `fev1_fvc_z` and
#' `fev1_fvc_LLN` become `NA`; `fev1_fvc_pred` falls back to
#' `fev1_pred / fvc_pred`.
#'
#' @references
#' \insertRef{quanjer2012}{HealthMarkers}
#' \insertRef{hankinson1999spirometry}{HealthMarkers}
#' \insertRef{bowerman2023gli}{HealthMarkers} (race-neutral GLI global equations and interpretation framework; used by rspiro's `GLIgl` equation)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#'
#' @examples
#' if (requireNamespace("rspiro", quietly = TRUE)) {
#'   df <- data.frame(
#'     age = c(40, 55), sex = c("male", "female"),
#'     height = c(175, 162), ethnicity = c("Caucasian", "Caucasian"),
#'     fev1 = c(3.5, 2.4), fvc = c(4.4, 3.1)
#'   )
#'   pulmo_markers(df)
#' }
#' @export
pulmo_markers <- function(data,
                          col_map = NULL,
                          equation = c("GLI", "GLIgl", "NHANES3"),
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2,
                          verbose = TRUE) {
  # HM-CS v2: validate inputs (data-frame only here)
  hm_validate_inputs(data, col_map, required_keys = character(0), fn = "pulmo_markers")
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "pulmo_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)

  # Custom equation check (clear error message on unknown)
  eq <- as.character(equation)[1]
  if (!eq %in% c("GLI", "GLIgl", "NHANES3")) {
    rlang::abort(
      sprintf("pulmo_markers(): unsupported equation '%s'. Choose one of: GLI, GLIgl, NHANES3.", eq),
      class = "healthmarkers_pulmo_error_equation"
    )
  }

  na_action <- match.arg(na_action)

  hm_inform(sprintf("pulmo_markers(): preparing inputs [%s]", eq),
            level = if (isTRUE(verbose)) "inform" else "debug")

  # Basic validation and coercion
  .pm_validate_df(data)
  req <- c("age", "sex", "height", "ethnicity", "fev1", "fvc")
  cm  <- .hm_build_col_map(data, col_map, keys = req, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map
  miss <- setdiff(req, names(data))
  if (length(miss)) {
    rlang::abort(
      paste0("pulmo_markers(): missing required columns: ", paste(miss, collapse = ", ")),
      class = "healthmarkers_pulmo_error_missing_columns"
    )
  }

  data <- .pm_coerce_numeric(data, cols = c("age","height","fev1","fvc"))

  # High-missingness diagnostics on required inputs (debug)
  .pm_high_missing_diag(data, req, na_warn_prop = na_warn_prop)

  # NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("pulmo_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_pulmo_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (sum(!keep) > 0L)
      hm_inform(sprintf("pulmo_markers(): omitting %d rows with NA in required inputs", sum(!keep)),
                level = "inform")
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      fev1_pred = numeric(), fev1_z = numeric(), fev1_pctpred = numeric(), fev1_LLN = numeric(),
      fvc_pred  = numeric(), fvc_z  = numeric(), fvc_pctpred  = numeric(), fvc_LLN  = numeric(),
      fev1_fvc_ratio = numeric(), fev1_fvc_pred = numeric(), fev1_fvc_z = numeric(),
      fev1_fvc_pctpred = numeric(), fev1_fvc_LLN = numeric()
    ))
  }

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  fev1_pred, fev1_z, fev1_pctpred, fev1_LLN [age, height, sex, ethnicity, fev1]\n  fvc_pred, fvc_z, fvc_pctpred, fvc_LLN [age, height, sex, ethnicity, fvc]\n  fev1_fvc_ratio, fev1_fvc_pred, fev1_fvc_z, fev1_fvc_pctpred, fev1_fvc_LLN [fev1, fvc]", fn_name), level = "inform")

  # Map sex and ethnicity to rspiro codes (male=1, female=2; GLI: 1-5 ethnic groups)
  sex_code <- .pm_map_sex(data$sex)
  eth_code <- .pm_map_ethnicity(data$ethnicity)

  # Height auto-detection: any height > 3 -> cm
  height_m <- data$height
  if (any(height_m > 3, na.rm = TRUE)) {
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = "pulmo_markers(): converting height from cm to m")
    height_m <- height_m / 100
  }
  # Basic plausibility on height
  if (any(is.finite(height_m) & height_m <= 0, na.rm = TRUE)) {
    rlang::warn("pulmo_markers(): non-positive heights detected; outputs will be NA for those rows.")
  }

  age <- data$age
  fev1 <- data$fev1
  fvc  <- data$fvc

  hm_inform(level = "debug", msg = sprintf("pulmo_markers(): sex=%s; eth=%s",
                        paste(sort(unique(sex_code)), collapse = ","),
                        paste(sort(unique(eth_code)), collapse = ",")))

  # Lookup reference functions from rspiro
  ns <- tryCatch(asNamespace("rspiro"), error = function(e) NULL)
  if (is.null(ns)) {
    rlang::abort("pulmo_markers(): package 'rspiro' is required but not installed.",
                 class = "healthmarkers_pulmo_error_missing_pkg")
  }
  f_pred   <- paste0("pred_",   eq)
  f_zscore <- paste0("zscore_", eq)
  f_lln    <- paste0("LLN_",    eq)
  if (!all(vapply(c(f_pred, f_zscore, f_lln), exists, logical(1), envir = ns, inherits = FALSE))) {
    rlang::abort(
      sprintf("pulmo_markers(): equation '%s' not supported by installed 'rspiro'.", eq),
      class = "healthmarkers_pulmo_error_equation"
    )
  }
  pred_fun   <- get(f_pred, envir = ns)
  zscore_fun <- get(f_zscore, envir = ns)
  lln_fun    <- get(f_lln, envir = ns)

  hm_inform("pulmo_markers(): computing markers", level = "debug")

  # Safe division helper (defined before rspiro calls)
  safe_div <- function(num, den) {
    out <- num / den
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Observed FEV1/FVC ratio (needed for rspiro FEV1FVC z-score calls)
  obs_ratio <- safe_div(fev1, fvc)

  # Compute predicted, z-scores, LLN by equation
  if (eq == "GLIgl") {
    fev1_pred  <- pred_fun(age, height_m, sex_code, param = "FEV1")
    fvc_pred   <- pred_fun(age, height_m, sex_code, param = "FVC")
    ratio_pred <- tryCatch(pred_fun(age, height_m, sex_code, param = "FEV1FVC"),
                           error = function(e) safe_div(fev1_pred, fvc_pred))
    fev1_z     <- zscore_fun(age, height_m, sex_code, FEV1 = fev1)
    fvc_z      <- zscore_fun(age, height_m, sex_code, FVC = fvc)
    ratio_z    <- tryCatch(zscore_fun(age, height_m, sex_code, FEV1FVC = obs_ratio),
                           error = function(e) rep(NA_real_, length(obs_ratio)))
    fev1_LLN   <- lln_fun(age, height_m, sex_code, param = "FEV1")
    fvc_LLN    <- lln_fun(age, height_m, sex_code, param = "FVC")
    ratio_LLN  <- tryCatch(lln_fun(age, height_m, sex_code, param = "FEV1FVC"),
                           error = function(e) rep(NA_real_, length(age)))
  } else {
    fev1_pred  <- pred_fun(age, height_m, sex_code, eth_code, param = "FEV1")
    fvc_pred   <- pred_fun(age, height_m, sex_code, eth_code, param = "FVC")
    ratio_pred <- tryCatch(pred_fun(age, height_m, sex_code, eth_code, param = "FEV1FVC"),
                           error = function(e) safe_div(fev1_pred, fvc_pred))
    fev1_z     <- zscore_fun(age, height_m, sex_code, eth_code, FEV1 = fev1)
    fvc_z      <- zscore_fun(age, height_m, sex_code, eth_code, FVC = fvc)
    ratio_z    <- tryCatch(zscore_fun(age, height_m, sex_code, eth_code, FEV1FVC = obs_ratio),
                           error = function(e) rep(NA_real_, length(obs_ratio)))
    fev1_LLN   <- lln_fun(age, height_m, sex_code, eth_code, param = "FEV1")
    fvc_LLN    <- lln_fun(age, height_m, sex_code, eth_code, param = "FVC")
    ratio_LLN  <- tryCatch(lln_fun(age, height_m, sex_code, eth_code, param = "FEV1FVC"),
                           error = function(e) rep(NA_real_, length(age)))
  }

  # Percent predicted
  fev1_pctpred <- 100 * safe_div(fev1, fev1_pred)
  fvc_pctpred  <- 100 * safe_div(fvc,  fvc_pred)
  ratio_pct    <- 100 * safe_div(obs_ratio, ratio_pred)

  out <- tibble::tibble(
    fev1_pred        = as.numeric(fev1_pred),
    fev1_z           = as.numeric(fev1_z),
    fev1_pctpred     = as.numeric(fev1_pctpred),
    fev1_LLN         = as.numeric(fev1_LLN),
    fvc_pred         = as.numeric(fvc_pred),
    fvc_z            = as.numeric(fvc_z),
    fvc_pctpred      = as.numeric(fvc_pctpred),
    fvc_LLN          = as.numeric(fvc_LLN),
    fev1_fvc_ratio   = as.numeric(obs_ratio),
    fev1_fvc_pred    = as.numeric(ratio_pred),
    fev1_fvc_z       = as.numeric(ratio_z),
    fev1_fvc_pctpred = as.numeric(ratio_pct),
    fev1_fvc_LLN     = as.numeric(ratio_LLN)
  )

  if (!is.null(id_col)) {
    id_vec <- data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) hm_inform(hm_result_summary(out, fn_name), level = "inform")

  out
}

# ---- internal helpers ---------------------------------------------------------

.pm_validate_df <- function(df) {
  if (!is.data.frame(df)) {
    rlang::abort("pulmo_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_pulmo_error_data_type")
  }
  invisible(TRUE)
}

.pm_coerce_numeric <- function(df, cols) {
  for (cn in cols) {
    if (!is.numeric(df[[cn]])) {
      old <- df[[cn]]
      suppressWarnings(df[[cn]] <- as.numeric(old))
      introduced_na <- sum(is.na(df[[cn]]) & !is.na(old))
      if (introduced_na > 0) {
        rlang::warn(sprintf("pulmo_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      }
    }
  }
  df
}

.pm_high_missing_diag <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]
    if (length(x) == 0L) next
    pna <- sum(is.na(x)) / length(x)
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("pulmo_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.pm_map_sex <- function(sex) {
  s <- tolower(as.character(sex))
  out <- rep(NA_integer_, length(s))

  # numeric forms first
  num <- suppressWarnings(as.numeric(s))
  out[!is.na(num) & num %in% c(1, 2)] <- as.integer(num[!is.na(num) & num %in% c(1, 2)])
  out[!is.na(num) & num %in% c(0, 1) & is.na(out)] <- as.integer(num[!is.na(num) & num %in% c(0, 1)]) + 1L

  # character forms
  out[s %in% c("male","m")] <- 1L
  out[s %in% c("female","f")] <- 2L

  bad <- sum(is.na(out))
  if (bad > 0) rlang::warn(sprintf("pulmo_markers(): 'sex' has %d unmapped values; set to NA.", bad))
  out
}

.pm_map_ethnicity <- function(eth) {
  e <- tolower(as.character(eth))
  out <- ifelse(
    e %in% c("caucasian","white","european","non-hispanic white"), 1L,
    ifelse(e %in% c("african-american","black","african american"), 2L,
      ifelse(e %in% c("ne asian","northeast asian","east asian"), 3L,
        ifelse(e %in% c("se asian","southeast asian"), 4L, 5L)
      )
    )
  )
  # Treat obviously "other"/"mixed"/"hispanic" as code 5
  out[e %in% c("other","mixed","other/mixed","hispanic","latino","south asian","indian")] <- 5L
  bad <- sum(is.na(out))
  if (bad > 0) rlang::warn(sprintf("pulmo_markers(): 'ethnicity' has %d unmapped values; set to 'Other/Mixed' (5).", bad))
  out[is.na(out)] <- 5L
  out
}

