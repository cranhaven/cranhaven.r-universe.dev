
#' Calculate OGTT-based insulin sensitivity indices
#'
#' Given glucose & insulin at 0, 30, 120 min (plus weight, BMI, age, sex),
#' computes:
#' - Isi_120
#' - Cederholm_index
#' - Gutt_index
#' - Avignon_Si0
#' - Avignon_Si120
#' - Avignon_Sim
#' - Modified_stumvoll
#' - Stumvoll_Demographics
#' - Matsuda_AUC
#' - Matsuda_ISI
#' - BigttSi
#' - Ifc_inv
#' - HIRI_inv
#' - Belfiore_isi_gly
#'
#' Units assumed:
#' - OGTT glucose in mmol/L (internally converted to mg/dL via *18 for select indices)
#' - OGTT insulin in pmol/L (internally converted to muU/mL via /6 for select indices)
#' - weight in kg; BMI in kg/m^2; age in years; sex coded 1 = male, 2 = female
#'
#' Notes
#' - Conversions mirror existing implementation to preserve outputs. Some
#'   formulas intentionally use unconverted inputs (as in prior code).
#' - Modified_stumvoll and Stumvoll_Demographics use raw pmol/L and mmol/L
#'   as published in Stumvoll et al. (2000). BigttSi likewise uses raw units.
#' - Matsuda_AUC is a non-standard AUC-based variant; the original Matsuda
#'   index (Matsuda_ISI) uses time-point means, not AUCs.
#' - Cederholm_index uses log(I0 + I120) as implemented (sum, not mean);
#'   Gutt_index uses log((I0 + I120)/2) (mean). This mirrors published
#'   implementations; the difference is a constant log(2) offset.
#' - Ifc_inv and HIRI_inv are derived composite proxies not attributed to a
#'   single formula publication; treat as research tools.
#' - Logs are safe: log(x) becomes NA when x <= 0 or non-finite.
#'
#' @param data A data.frame or tibble containing at least the columns mapped by `col_map`.
#' @param col_map Named list mapping:
#'   - G0, G30, G120 -> glucose at 0, 30, 120 min (mmol/L)
#'   - I0, I30, I120 -> insulin at 0, 30, 120 min (pmol/L)
#'   - weight -> body weight (kg)
#'   - bmi -> body-mass index (kg/m^2)
#'   - age -> age (years)
#'   - sex -> sex (1 = male, 2 = female)
#' @param normalize One of c("none","z","inverse","range","robust") used by normalize_vec().
#' @param verbose Logical; if `TRUE` (default), prints column mapping, the list
#'   of indices being computed, and a per-column results summary.
#' @param na_action One of c("keep","omit","error") for missing/non-finite required inputs. Default "keep".
#' @param na_warn_prop Proportion (0-1) for high-missingness diagnostics (debug). Default 0.2.
#'
#' @return A tibble with the OGTT-based index columns listed above. If an ID
#'   column is detected in `data` (e.g. `id`, `IID`, `participant_id`), it is
#'   prepended as the first output column.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across everything
#' @export
#'
#' @seealso [fasting_is()], [normalize_vec()]
#'
#' @references
#' \insertRef{matsuda1999ogtt}{HealthMarkers}
#' \insertRef{gutt2000isi}{HealthMarkers}
#' \insertRef{stumvoll2000ogtt}{HealthMarkers}
#' \insertRef{hansen2007bigtt}{HealthMarkers}
#' \insertRef{avignon1999ogtt}{HealthMarkers}
#' \insertRef{belfiore1998insulin}{HealthMarkers}
#' \insertRef{matthews1985homa}{HealthMarkers}
#' \insertRef{suleman2024is}{HealthMarkers} (genetic epidemiology study reviewing IS indices)
#'
#' @examples
#' df <- tibble::tibble(
#'   G0 = 5.5, I0 = 60,
#'   G30 = 7.8, I30 = 90,
#'   G120 = 6.2, I120 = 50,
#'   weight = 70, bmi = 24, age = 30, sex = 1
#' )
#' ogtt_is(
#'   df,
#'   col_map = list(
#'     G0 = "G0", I0 = "I0",
#'     G30 = "G30", I30 = "I30",
#'     G120 = "G120", I120 = "I120",
#'     weight = "weight", bmi = "bmi",
#'     age = "age", sex = "sex"
#'   ),
#'   normalize = "none",
#'   verbose = TRUE
#' )
ogtt_is <- function(data,
                    col_map = NULL,
                    normalize = "none",
                    verbose = TRUE,
                    na_action = c("keep","omit","error"),
                    na_warn_prop = 0.2) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name    <- "ogtt_is"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action  <- match.arg(na_action)
  id_col     <- .hm_detect_id_col(data)
  .ogtt_validate_args(normalize, na_warn_prop, verbose)

  req_keys <- c("G0","I0","G30","I30","G120","I120","weight","bmi","age","sex")
  cm      <- .hm_build_col_map(data, col_map, req_keys, fn = "ogtt_is")
  data    <- cm$data
  col_map <- cm$col_map

  # HM-CS v2: required keys validation
  hm_validate_inputs(
    data, col_map,
    required_keys = req_keys,
    fn = "ogtt_is"
  )

  # --- Verbose: col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: computing markers list
  if (isTRUE(verbose)) {
    indices <- list(
      c("Isi_120",            "G120, I120"),
      c("Cederholm_index",    "G0, G120, I0, I120, weight"),
      c("Gutt_index",         "G0, G120, I0, I120, weight"),
      c("Avignon_Si0",        "G0, I0, weight"),
      c("Avignon_Si120",      "G120, I120, weight"),
      c("Avignon_Sim",        "Si0, Si120"),
      c("Modified_stumvoll",  "I0, I120, G120"),
      c("Stumvoll_Demog.",    "bmi, I120, age"),
      c("Matsuda_AUC",        "G0, I0, G_AUC, I_AUC"),
      c("Matsuda_ISI",        "G0, I0, G_mean, I_mean"),
      c("BigttSi",            "I0, I30, I120, G0, G30, G120, sex, bmi"),
      c("Ifc_inv",            "I0, I120"),
      c("HIRI_inv",           "G0, G30, I0, I30"),
      c("Belfiore_isi_gly",   "I_AUC, G_AUC")
    )
    status <- vapply(indices, function(x) sprintf("  %-22s [%s]", x[1], x[2]), character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # Coerce required columns to numeric if needed (warn on NAs introduced)
  keys_to_check <- c("G0","I0","G30","I30","G120","I120","weight","bmi","age","sex")
  for (k in keys_to_check) {
    cn <- col_map[[k]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("ogtt_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug verbosity)
  .ogtt_high_missing_diag(
    data,
    vars = unname(unlist(col_map[keys_to_check], use.names = FALSE)),
    na_warn_prop = na_warn_prop
  )

  # NA policy on required inputs
  used_cols <- unname(unlist(col_map[keys_to_check], use.names = FALSE))
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("ogtt_is(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_ogtt_is_error_missing_values")
    }
  } else if (na_action == "omit" && length(used_cols)) {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep)),
                level = "inform")
    data <- data[keep, , drop = FALSE]
  }

  hm_inform(sprintf("%s(): converting units", fn_name), level = "debug")

  # 1) Extract & convert raw inputs
  G0   <- data[[col_map$G0]]   * 18 # mg/dL
  G30  <- data[[col_map$G30]]  * 18
  G120 <- data[[col_map$G120]] * 18
  I0   <- data[[col_map$I0]]   / 6  # muU/mL
  I30  <- data[[col_map$I30]]  / 6
  I120 <- data[[col_map$I120]] / 6

  wt   <- data[[col_map$weight]]
  bmi  <- data[[col_map$bmi]]
  age  <- data[[col_map$age]]
  # Normalize sex to 1=male, 0=female for formula use
  sex  <- .hm_normalize_sex(data[[col_map$sex]], to = "10", fn = "ogtt_is")

  hm_inform(sprintf("%s(): computing indices", fn_name), level = "debug")

  # Helpers: safe log and safe division
  lg <- function(x) .ogtt_log(x)
  dv <- function(a, b) .ogtt_safe_div(a, b)

  # 2) Areas under curve & means (trapezoid with intervals 0-30-120)
  I_AUC <- 0.5 * ((I0 + I30) * 30 + (I30 + I120) * 90)
  G_AUC <- 0.5 * ((G0 + G30) * 30 + (G30 + G120) * 90)
  I_mean <- rowMeans(cbind(I0, I30, I120), na.rm = TRUE)
  G_mean <- rowMeans(cbind(G0, G30, G120), na.rm = TRUE)

  # 3) Compute indices
  out <- tibble::tibble(
    Isi_120 = dv(10000, (G120 * I120)),
    Cederholm_index = dv(75000 + (G0 - G120) * 1.15 * 180 * 0.19 * wt,
                         120 * ((G0 + G120) / 2) * lg(I0 + I120)),
    Gutt_index = dv(75000 + (G0 - G120) * 0.19 * wt,
                    120 * ((G0 + G120) / 2) * lg((I0 + I120) / 2)),
    Avignon_Si0   = dv(1e8, (G0 * I0)   * wt * 150),
    Avignon_Si120 = dv(1e8, (G120 * I120) * wt * 150),
    Avignon_Sim   = (Avignon_Si0 + Avignon_Si120) / 2,
    Modified_stumvoll = 0.156 -
      0.0000459 * data[[col_map$I120]] -
      0.000321  * data[[col_map$I0]]   -
      0.00541   * data[[col_map$G120]],
    Stumvoll_Demographics = 0.222 -
      0.00333   * bmi -
      0.0000779 * data[[col_map$I120]] -
      0.000422  * age,
    Matsuda_AUC = dv(10000, sqrt(G0 * I0 * G_AUC * I_AUC)),
    Matsuda_ISI = dv(10000, sqrt(G0 * I0 * G_mean * I_mean)),
    BigttSi = exp(
      4.90 -
        0.00402 * data[[col_map$I0]]   -
        0.000565* data[[col_map$I30]]  -
        0.00127 * data[[col_map$I120]] -
        0.152   * data[[col_map$G0]]   -
        0.00871 * data[[col_map$G30]]  -
        0.0373  * data[[col_map$G120]] -
        ifelse(sex == 1, 0.145, 0) -
        0.0376  * bmi
    ),
    Ifc_inv = -lg(dv(data[[col_map$I120]], data[[col_map$I0]])),
    HIRI_inv = -(((G0 + G30) / 2) / 100 * ((I0 + I30) / 2)),
    Belfiore_isi_gly = dv(2, (I_AUC * G_AUC) + 1)
  )

  # 4) Normalize if requested
  out <- dplyr::mutate(
    out,
    dplyr::across(
      dplyr::everything(),
      ~ HealthMarkers::normalize_vec(.x, method = normalize)
    )
  )

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

# ---- internal helpers (not exported) -----------------------------------------

.ogtt_validate_args <- function(normalize, na_warn_prop, verbose) {
  ok_norm <- normalize %in% c("none","z","inverse","range","robust")
  if (!ok_norm) rlang::abort("`normalize` must be one of: 'none','z','inverse','range','robust'.",
                             class = "healthmarkers_ogtt_is_error_normalize")
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("`na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_ogtt_is_error_na_warn_prop")
  }
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    rlang::abort("`verbose` must be a single logical value.",
                 class = "healthmarkers_ogtt_is_error_verbose")
  }
  invisible(TRUE)
}

.ogtt_high_missing_diag <- function(df, vars, na_warn_prop = 0.2) {
  if (!length(vars)) return(invisible(TRUE))
  for (v in vars) {
    x <- df[[v]]
    n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("ogtt_is(): column '%s' has high missingness (%.1f%%).", v, 100 * pna))
    }
  }
  invisible(TRUE)
}

.ogtt_safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

.ogtt_log <- function(x) {
  y <- x
  y[!(is.finite(y) & y > 0)] <- NA_real_
  out <- suppressWarnings(log(y))
  out[!is.finite(out)] <- NA_real_
  out
}


