
#' Compute tracer/DXA-based insulin sensitivity indices
#'
#' Uses stable isotope tracer infusion rates and DXA-measured fat mass
#' to compute peripheral and adipose insulin sensitivity and related metrics.
#'
#' Modes:
#' - Adipose-only indices when only adipose-related keys are mapped (no OGTT glucose/insulin time series)
#' - Full indices otherwise
#'
#' Expected units:
#' - Glucose: mmol/L (internally converted to mg/dL when needed)
#' - Insulin: pmol/L (internally converted to muU/mL via /6)
#' - TG: mmol/L (to mg/dL via *88.57); HDL-c: mmol/L (to mg/dL via *38.67)
#' - Tracer rates: mumol/min
#' - Fat mass, weight: kg; BMI: kg/m^2
#'
#' @param data A data.frame or tibble containing raw measurements.
#' @param col_map Named list with entries (depending on mode):
#'   Adipose-only required:
#'     - I0: fasting insulin (pmol/L)
#'     - rate_glycerol, rate_palmitate: tracer rates (mumol/min)
#'     - fat_mass, weight, bmi: body composition
#'     - HDL_c: HDL cholesterol (mmol/L)
#'   Full mode additionally requires:
#'     - G0, G30, G120: glucose (mmol/L)
#'     - I30, I120: insulin (pmol/L)
#'     - TG: triglycerides (mmol/L)
#'     - FFA: free fatty acids (mmol/L)
#' @param normalize Ignored (kept for backward compatibility).
#' @param na_action One of c("keep","omit","error") for NA handling on required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness warnings on required inputs. Default 0.2.
#' @param verbose Logical; if TRUE, prints progress messages and a completion summary.
#'
#' @return
#' - Adipose-only tibble columns: LIRI_inv, Lipo_inv, ATIRI_inv
#' - Full-mode tibble columns: I_AUC, FFA_AUC, tracer_palmitate_SI, tracer_glycerol_SI, LIRI_inv, Lipo_inv, ATIRI_inv
#'
#' @examples
#' df <- data.frame(
#'   I0 = c(60, 75), rate_glycerol = c(2.1, 2.8), rate_palmitate = c(1.8, 2.3),
#'   fat_mass = c(18, 24), weight = c(72, 85), BMI = c(24, 29),
#'   HDL_c = c(1.3, 1.1)
#' )
#' col_map <- list(I0="I0", rate_glycerol="rate_glycerol",
#'                 rate_palmitate="rate_palmitate", fat_mass="fat_mass",
#'                 weight="weight", bmi="BMI", HDL_c="HDL_c")
#' tracer_dxa_is(df, col_map = col_map)
#'
#' @note
#' `tracer_palmitate_SI` and `tracer_glycerol_SI` are simple rate/fat_mass
#' ratios; the Steele (1959) non-steady-state tracer equation is **not**
#' implemented here. The LIRI formula coefficients (-0.091, 0.4, 0.346,
#' -0.408, 0.435) are attributed to Gastaldelli et al. but the paper cited
#' (`gastaldelli2004betacell`) covers beta-cell dysfunction, not LIRI
#' derivation; the primary LIRI source should be verified. In adipose-only
#' mode (I30 absent) the mean-insulin term uses I0 twice as a fallback.
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
#' @references
#' \insertRef{groop1989tracer}{HealthMarkers} (tracer lipolysis methodology; background)
#' \insertRef{steele1959glucose}{HealthMarkers} (tracer dilution theory; Steele equation not directly implemented — background)
#' \insertRef{roden1996ffa}{HealthMarkers} (FFA-induced insulin resistance mechanism; background)
#' \insertRef{gastaldelli2004betacell}{HealthMarkers} (beta-cell dysfunction context; LIRI formula source unverified — background)
#' \insertRef{karpe2011ffa}{HealthMarkers} (FFA and insulin resistance review; background)
#' \insertRef{petersen2007muscleinsulin}{HealthMarkers} (muscle insulin resistance and metabolic syndrome; background)
#' \insertRef{santomauro1999acipimox}{HealthMarkers} (FFA lowering and insulin sensitivity; background)
tracer_dxa_is <- function(data, col_map = NULL,
                          normalize = NULL,
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2,
                          verbose = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "tracer_dxa_is"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)
  na_action <- match.arg(na_action)

  if (!is.null(normalize)) {
    # maintain signature compatibility; explicitly ignore
    # rlang::warn("tracer_dxa_is(): `normalize` argument is ignored.")
  }

  if (!is.data.frame(data)) {
    rlang::abort("tracer_dxa_is(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_tracer_error_data_type")
  }
  if (isTRUE(verbose)) hm_inform("tracer_dxa_is(): preparing inputs", level = "inform")
  else hm_inform("tracer_dxa_is(): preparing inputs", level = "debug")

  adipose_keys <- c("I0", "rate_palmitate", "rate_glycerol", "fat_mass", "weight", "HDL_c", "bmi")
  all_tracer_keys <- c(adipose_keys, "G0","G30","G120","I30","I120","TG","FFA")
  cm      <- .hm_build_col_map(data, col_map, all_tracer_keys, fn = "tracer_dxa_is")
  data    <- cm$data
  col_map <- cm$col_map
  # Adipose-only when all adipose keys present and not all OGTT keys present
  adipose_only <- all(adipose_keys %in% names(col_map)) &&
    !all(c("G0", "I30") %in% names(col_map))

  full_keys <- c(adipose_keys, "G0","G30","G120","I30","I120","TG","FFA")

  required_keys <- if (adipose_only) adipose_keys else full_keys
  hm_validate_inputs(data, col_map, required_keys = required_keys, fn = "tracer_dxa_is")

  # Validate data contains the mapped columns
  mapped_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(mapped_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("tracer_dxa_is(): missing required columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_tracer_error_missing_columns"
    )
  }

  # Coerce mapped numeric columns to numeric; warn on NAs introduced
  to_num_keys <- intersect(required_keys, c(
    "G0","G30","G120","I0","I30","I120","TG","HDL_c","FFA",
    "rate_glycerol","rate_palmitate","fat_mass","weight","bmi"
  ))
  for (key in to_num_keys) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("tracer_dxa_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    # Set non-finite to NA for safety
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness warnings on required inputs
  .tx_warn_high_missing(data, mapped_cols, na_warn_prop)
  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose)) {
    marker_list <- if (adipose_only) "LIRI_inv, Lipo_inv, ATIRI_inv" else "I_AUC, FFA_AUC, tracer_palmitate_SI, tracer_glycerol_SI, LIRI_inv, Lipo_inv, ATIRI_inv"
    hm_inform(sprintf("%s(): computing markers:\n  %s", fn_name, marker_list), level = "inform")
  }

  # NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("tracer_dxa_is(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_tracer_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (sum(!keep) > 0L)
      hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
                msg   = sprintf("tracer_dxa_is(): omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    if (adipose_only) {
      return(tibble::tibble(
        LIRI_inv  = numeric(),
        Lipo_inv  = numeric(),
        ATIRI_inv = numeric()
      ))
    } else {
      return(tibble::tibble(
        I_AUC               = numeric(),
        FFA_AUC             = numeric(),
        tracer_palmitate_SI = numeric(),
        tracer_glycerol_SI  = numeric(),
        LIRI_inv            = numeric(),
        Lipo_inv            = numeric(),
        ATIRI_inv           = numeric()
      ))
    }
  }

  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }
  safe_log10 <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log10(y))
    out[!is.finite(out)] <- NA_real_
    out
  }

  if (adipose_only) {
    if (isTRUE(verbose)) hm_inform("tracer_dxa_is(): adipose-only indices", level = "inform")

    I0_u <- data[[col_map$I0]] / 6
    Lipo_inv <- -1 * (data[[col_map$rate_glycerol]] * I0_u)
    ATIRI_inv <- -1 * (data[[col_map$rate_palmitate]] * I0_u)
    LIRI_inv <- -1 * (
      -0.091 +
        safe_log10((I0_u + I0_u) / 2 * 6) * 0.4 + # use I0 twice if I30 missing; preserve behavior
        safe_log10((safe_div(data[[col_map$fat_mass]], data[[col_map$weight]], "fm_wt") * 100)) * 0.346 -
        safe_log10(data[[col_map$HDL_c]] * 38.67) * 0.408 +
        safe_log10(data[[col_map$bmi]]) * 0.435
    )

    out <- tibble::tibble(
      LIRI_inv  = as.numeric(LIRI_inv),
      Lipo_inv  = as.numeric(Lipo_inv),
      ATIRI_inv = as.numeric(ATIRI_inv)
    )
  } else {
    if (isTRUE(verbose)) hm_inform("tracer_dxa_is(): computing indices", level = "inform")

    # Unit conversions
    I0_u   <- data[[col_map$I0]]   / 6
    I30_u  <- data[[col_map$I30]]  / 6
    I120_u <- data[[col_map$I120]] / 6
    # Glucose present but not used directly in current formulas; keep conversions in case of future indices
    G0_mg   <- data[[col_map$G0]]   * 18
    G30_mg  <- data[[col_map$G30]]  * 18
    G120_mg <- data[[col_map$G120]] * 18
    TG_mg   <- data[[col_map$TG]]   * 88.57
    HDL_mg  <- data[[col_map$HDL_c]] * 38.67
    FFA_val <- data[[col_map$FFA]]

    # AUCs (minutes): insulin trapezoid (0-30, 30-120); FFA flat (single measure over 0-120)
    I_AUC <- 0.5 * ((I0_u + I30_u) * 30 + (I30_u + I120_u) * 90)
    FFA_AUC <- 0.5 * (FFA_val + FFA_val) * 120

    # Tracer sensitivity per kg fat mass
    tracer_palmitate_SI <- safe_div(data[[col_map$rate_palmitate]], data[[col_map$fat_mass]], "palmitate_fm")
    tracer_glycerol_SI  <- safe_div(data[[col_map$rate_glycerol]],  data[[col_map$fat_mass]], "glycerol_fm")

    # Adipose indices
    # Note: safe logs avoid non-positive values; units preserved as in prior code
    LIRI_inv <- -1 * (
      -0.091 +
        safe_log10((I0_u + I30_u) / 2 * 6) * 0.4 +
        safe_log10((safe_div(data[[col_map$fat_mass]], data[[col_map$weight]], "fm_wt") * 100)) * 0.346 -
        safe_log10(HDL_mg) * 0.408 +
        safe_log10(data[[col_map$bmi]]) * 0.435
    )
    Lipo_inv <- -1 * (data[[col_map$rate_glycerol]] * I0_u)
    ATIRI_inv <- -1 * (data[[col_map$rate_palmitate]] * I0_u)

    out <- tibble::tibble(
      I_AUC               = as.numeric(I_AUC),
      FFA_AUC             = as.numeric(FFA_AUC),
      tracer_palmitate_SI = as.numeric(tracer_palmitate_SI),
      tracer_glycerol_SI  = as.numeric(tracer_glycerol_SI),
      LIRI_inv            = as.numeric(LIRI_inv),
      Lipo_inv            = as.numeric(Lipo_inv),
      ATIRI_inv           = as.numeric(ATIRI_inv)
    )
  }

  # Consolidated zero-denominator warning
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("tracer_dxa_is(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (!is.null(id_col)) {
    # id_col comes from original data (before omit); need the kept rows
    id_vec <- data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}

# ---- internal helpers ---------------------------------------------------------

.tx_warn_high_missing <- function(data, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf(
        "tracer_dxa_is(): column '%s' has high missingness (%.1f%%).",
        cn, 100 * pna
      ))
    }
  }
  invisible(TRUE)
}

.tx_default_extreme_rules <- function(adipose_only) {
  # Very broad plausibility bounds; keys match col_map keys
  base <- list(
    I0             = c(0, 5000),   # pmol/L
    I30            = c(0, 5000),
    I120           = c(0, 5000),
    G0             = c(0, 40),     # mmol/L
    G30            = c(0, 40),
    G120           = c(0, 40),
    TG             = c(0, 50),     # mmol/L
    HDL_c          = c(0, 10),     # mmol/L
    FFA            = c(0, 5),      # mmol/L
    rate_glycerol  = c(0, 10000),  # mumol/min
    rate_palmitate = c(0, 10000),  # mumol/min
    fat_mass       = c(0.1, 200),  # kg
    weight         = c(1, 400),    # kg
    bmi            = c(5, 100)     # kg/m^2
  )
  if (isTRUE(adipose_only)) {
    base[c("I0","rate_glycerol","rate_palmitate","fat_mass","weight","HDL_c","bmi")]
  } else {
    base
  }
}

