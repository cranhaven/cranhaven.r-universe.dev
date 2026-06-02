
#' Compute anthropometric obesity & adiposity indices
#'
#' Calculates a comprehensive set of body shape and adiposity indices:
#' * BMI and WHO BMI categories
#' * Waist-to-hip ratio (WHR) and optional WHR adjusted for BMI (WHRadjBMI)
#' * Waist-to-height ratio (WHtR)
#' * Abdominal Volume Index (AVI)
#' * Body Adiposity Index (BAI)
#' * A Body Shape Index (ABSI)
#' * Body Roundness Index (BRI)
#' * Conicity Index (CI)
#' * (Optional) Relative Fat Mass (RFM)
#'
#' Units assumed (no automatic conversion beyond the specified weight/height options):
#' - weight: kg (or lb if weight_unit = "lb")
#' - height: m (or cm if height_unit = "cm")
#' - waist, hip: cm
#'
#' Note: WHtR, ABSI, BRI, CI, and RFM all require waist in the same unit as
#' height (metres). The function converts waist internally (waist_cm / 100)
#' for these five indices; users should always supply waist in cm.
#' - sex: 0 = male, 1 = female (only required if include_RFM = TRUE)
#'
#' @param data A data.frame or tibble containing the input columns.
#' @param weight Unquoted column name for weight.
#' @param height Unquoted column name for height.
#' @param waist Unquoted column name for waist circumference.
#' @param hip Unquoted column name for hip circumference.
#' @param sex   (Optional) Unquoted column name for sex, coded 0=male, 1=female; required if include_RFM=TRUE.
#' @param weight_unit   One of c("kg","lb"); if "lb", converts weight to kg by *0.45359237.
#' @param height_unit   One of c("cm","m"); if "cm", converts height to metres by /100.
#' @param adjust_WHR    Logical; if TRUE, adds a column WHRadjBMI as residuals from WHR ~ BMI.
#' @param include_RFM   Logical; if TRUE, computes Relative Fat Mass (requires sex column).
#' @param na_action One of c("keep","omit","error") for handling NA in required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness warnings for required inputs. Default 0.2.
#' @param verbose Logical; if TRUE, prints column mapping and computing messages.
#'
#' @return A tibble with only the computed indices (slim output):
#' * weight_kg, height_m (unit-normalised intermediates),
#' * BMI, BMI_cat,
#' * WHR, WHRadjBMI (if `adjust_WHR = TRUE`),
#' * waist_to_height_ratio, waist_to_BMI_ratio, weight_to_height_ratio,
#' * AVI, BAI, ABSI, BRI, CI,
#' * RFM (if `include_RFM = TRUE`).
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang enquo quo_name quo_is_null abort warn inform
#' @importFrom stats lm resid
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   wt     = c(70, 80),  # kg
#'   ht     = c(175, 165),# cm
#'   waist  = c(80, 90),  # cm
#'   hip    = c(100, 95), # cm
#'   sex    = c(0, 1)
#' )
#' obesity_indices(
#'   df,
#'   weight       = wt,
#'   height       = ht,
#'   waist        = waist,
#'   hip          = hip,
#'   sex          = sex,
#'   weight_unit  = "kg",
#'   height_unit  = "cm",
#'   adjust_WHR   = TRUE,
#'   include_RFM  = TRUE,
#'   verbose      = TRUE
#' )
#' @references
#' \insertRef{quetelet1842}{HealthMarkers}
#' \insertRef{who1995physicalstatus}{HealthMarkers}
#' \insertRef{guerrero1999avi}{HealthMarkers}
#' \insertRef{bergman2011bai}{HealthMarkers}
#' \insertRef{krakauer2012absi}{HealthMarkers}
#' \insertRef{thomas2013bri}{HealthMarkers}
#' \insertRef{valdez1991ci}{HealthMarkers}
#' \insertRef{woolcott2018rfm}{HealthMarkers}
#' \insertRef{calle1999bmi}{HealthMarkers}
#' \insertRef{freedman2012bai}{HealthMarkers}
#' \insertRef{he2013absi}{HealthMarkers}
#' \insertRef{maessen2014bri}{HealthMarkers}
obesity_indices <- function(data,
                            weight,
                            height,
                            waist,
                            hip,
                            sex = NULL,
                            weight_unit = c("kg", "lb"),
                            height_unit = c("cm", "m"),
                            adjust_WHR = FALSE,
                            include_RFM = FALSE,
                            na_action = c("keep","omit","error"),
                            na_warn_prop = 0.2,
                            verbose = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "obesity_indices"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col <- .hm_detect_id_col(data)
  # Match args
  weight_unit <- match.arg(weight_unit)
  height_unit <- match.arg(height_unit)
  na_action <- match.arg(na_action)

  # Capture and quote arguments
  wt_q <- rlang::enquo(weight);  wt_name  <- rlang::quo_name(wt_q)
  ht_q <- rlang::enquo(height);  ht_name  <- rlang::quo_name(ht_q)
  wst_q<- rlang::enquo(waist);   wst_name <- rlang::quo_name(wst_q)
  hp_q <- rlang::enquo(hip);     hp_name  <- rlang::quo_name(hp_q)
  sx_q <- rlang::enquo(sex);     sx_name  <- if (!rlang::quo_is_null(sx_q)) rlang::quo_name(sx_q) else NULL

  # Validate data frame and required columns
  .oi_validate_data_and_cols(data, c(wt_name, ht_name, wst_name, hp_name), include_RFM, sx_name)
  .oi_warn_high_missing(data, c(wt_name, ht_name, wst_name, hp_name, if (include_RFM) sx_name else NULL), na_warn_prop)
  if (isTRUE(verbose)) {
    col_mapping_str <- paste(
      c(sprintf("weight -> '%s'", wt_name), sprintf("height -> '%s'", ht_name),
        sprintf("waist -> '%s'", wst_name), sprintf("hip -> '%s'", hp_name),
        if (!is.null(sx_name)) sprintf("sex -> '%s'", sx_name) else NULL),
      collapse = ", ")
    hm_inform(sprintf("%s(): col_map: %s", fn_name, col_mapping_str), level = "inform")
    hm_inform(sprintf(
      "%s(): computing markers:\n  BMI, BMI_cat, WHR, WHtR, AVI, BAI, ABSI, BRI, CI%s",
      fn_name,
      paste0(if (isTRUE(adjust_WHR)) ", WHRadjBMI" else "", if (isTRUE(include_RFM)) ", RFM" else "")),
      level = "inform")
  }

  # NA policy on required inputs
  used_cols <- c(wt_name, ht_name, wst_name, hp_name, if (include_RFM) sx_name else NULL)
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("obesity_indices(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_obesity_error_missing_values")
    }
  } else if (na_action == "omit") {
    if (length(used_cols)) {
      keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
      hm_inform(sprintf("obesity_indices(): omitting %d rows with NA in required inputs", sum(!keep)),
                level = if (isTRUE(verbose)) "inform" else "debug")
      data <- data[keep, , drop = FALSE]
    }
  }
  # Unit-normalized base fields
  out <- dplyr::mutate(
    data,
    weight_kg = dplyr::case_when(
      weight_unit == "kg" ~ !!wt_q,
      weight_unit == "lb" ~ !!wt_q * 0.45359237
    ),
    height_m = dplyr::case_when(
      height_unit == "m" ~ !!ht_q,
      height_unit == "cm" ~ !!ht_q / 100
    )
  )

  # Compute BMI and category using normalized units
  out <- dplyr::mutate(
    out,
    BMI = weight_kg / (height_m^2),
    BMI_cat = dplyr::case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Normal weight",
      BMI < 30 ~ "Overweight",
      BMI < 35 ~ "Obesity Class I",
      BMI < 40 ~ "Obesity Class II",
      BMI >= 40 ~ "Obesity Class III",
      TRUE ~ NA_character_
    )
  )

  # Safe division helper with denominator-zero tracking
  denom_zero <- new.env(parent = emptyenv()); denom_zero$counts <- list()
  safe_div <- function(num, den, label) {
    res <- rep(NA_real_, length(num))
    ok <- !is.na(num) & !is.na(den)
    zero_den <- ok & (den == 0)
    denom_zero$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    valid <- ok & !zero_den
    res[valid] <- num[valid] / den[valid]
    res[!is.finite(res)] <- NA_real_
    res
  }
  # Safe power roots
  safe_pow <- function(x, p) {
    z <- rep(NA_real_, length(x))
    ok <- !is.na(x) & x > 0
    z[ok] <- x[ok]^p
    z
  }

  # Pull raw columns for waist/hip/sex
  wst <- out[[wst_name]]
  hip <- out[[hp_name]]
  sex_vec <- if (!is.null(sx_name)) out[[sx_name]] else NULL

  # Derived indices
  WHR <- safe_div(wst, hip, "WHR")
  WHtR <- safe_div(wst, out$height_m * 100, "waist_to_height_ratio")  # both in cm
  waist_BMI <- safe_div(wst, out$BMI, "waist_to_BMI_ratio")
  wt_ht <- safe_div(out$weight_kg, out$height_m, "weight_to_height_ratio")

  AVI <- (2 * (wst^2) + 0.7 * (wst - hip)^2) / 1000
  BAI <- ifelse(out$height_m > 0, hip / (out$height_m^1.5) - 18, NA_real_)

  denom_absi <- (safe_pow(out$BMI, 2/3)) * (safe_pow(out$height_m, 1/2))
  ABSI <- safe_div(wst / 100, denom_absi, "ABSI")  # waist to metres (Krakauer 2012)

  # BRI: waist converted to metres; ratio must be dimensionless (Thomas et al. 2013)
  ratio <- ifelse(out$height_m > 0, (wst / 100) / (2 * pi * out$height_m), NA_real_)
  BRI <- 364.2 - 365.5 * sqrt(pmax(0, 1 - (ratio^2)))

  # CI: waist converted to metres per Valdez (1991)
  denom_ci <- 0.109 * sqrt(safe_div(out$weight_kg, out$height_m, "CI_internal"))
  CI <- safe_div(wst / 100, denom_ci, "CI")

  # Attach derived columns
  out$WHR <- WHR
  out$waist_to_height_ratio <- WHtR
  out$waist_to_BMI_ratio <- waist_BMI
  out$weight_to_height_ratio <- wt_ht
  out$AVI <- as.numeric(AVI)
  out$BAI <- as.numeric(BAI)
  out$ABSI <- as.numeric(ABSI)
  out$BRI <- as.numeric(BRI)
  out$CI  <- as.numeric(CI)

  # WHR adjusted for BMI
  if (isTRUE(adjust_WHR)) {
    # Fit only on rows with finite WHR and BMI
    fit_df <- out[is.finite(out$WHR) & is.finite(out$BMI), c("WHR","BMI")]
    if (nrow(fit_df) >= 2L && stats::var(fit_df$BMI, na.rm = TRUE) > 0) {
      res <- stats::resid(stats::lm(WHR ~ BMI, data = fit_df))
      # Map residuals back to full rows (NA where not used in fit)
      WHRadj <- rep(NA_real_, nrow(out))
      WHRadj[as.numeric(rownames(fit_df))] <- as.numeric(res)
      out$WHRadjBMI <- WHRadj
    } else {
      rlang::warn("obesity_indices(): insufficient data to compute WHRadjBMI; coercing to NA.")
      out$WHRadjBMI <- NA_real_
    }
  }

  # Relative Fat Mass
  if (isTRUE(include_RFM)) {
    # Sex for RFM is expected as 0=male, 1=female; use directly without normalization
    sex_01 <- suppressWarnings(as.numeric(out[[sx_name]]))
    invalid_sex <- !is.na(sex_01) & !(sex_01 %in% c(0, 1))
    if (any(invalid_sex)) {
      rlang::warn(
        sprintf("obesity_indices(): 'sex' contains %d values not in {0,1}; RFM set to NA for those rows. Encode sex as 0=male, 1=female.", sum(invalid_sex)),
        class = "healthmarkers_obesity_warn_invalid_sex_rfm"
      )
      sex_01[invalid_sex] <- NA_real_
    }
    denom_rfm <- wst / 100  # convert waist from cm to metres (height_m already in m)
    RFM <- 64 - 20 * safe_div(out$height_m, denom_rfm, "RFM") + 12 * sex_01
    out$RFM <- as.numeric(RFM)
  }

  # Consolidated denominator-zero warning
  dz <- denom_zero$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz)
    nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("obesity_indices(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  # Slim output: return only computed index columns
  index_cols <- c(
    "weight_kg", "height_m", "BMI", "BMI_cat",
    "WHR",
    if (isTRUE(adjust_WHR)) "WHRadjBMI",
    "waist_to_height_ratio", "waist_to_BMI_ratio", "weight_to_height_ratio",
    "AVI", "BAI", "ABSI", "BRI", "CI",
    if (isTRUE(include_RFM)) "RFM"
  )
  slim <- out[, intersect(index_cols, names(out)), drop = FALSE]

  if (!is.null(id_col)) {
    slim[[id_col]] <- data[[id_col]]
    slim <- slim[, c(id_col, setdiff(names(slim), id_col)), drop = FALSE]
    slim <- tibble::as_tibble(slim)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(slim, fn_name), level = "inform") }
  slim
}

# ---- internal helpers ---------------------------------------------------------

.oi_validate_data_and_cols <- function(data, required, include_RFM, sx_name) {
  if (!is.data.frame(data)) {
    rlang::abort("obesity_indices(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_obesity_error_data_type")
  }
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("obesity_indices(): missing required columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_obesity_error_missing_columns"
    )
  }
  # Validate numeric types
  for (cn in required) {
    if (!is.numeric(data[[cn]])) {
      rlang::abort(sprintf("obesity_indices(): column '%s' must be numeric.", cn),
                   class = "healthmarkers_obesity_error_type")
    }
  }
  if (isTRUE(include_RFM)) {
    if (is.null(sx_name)) {
      rlang::abort("obesity_indices(): 'sex' must be provided to compute RFM",
                   class = "healthmarkers_obesity_error_missing_sex")
    }
    # Accept character sex (M/F/male/female/1/2/0); normalize to 0=male, 1=female for RFM
    if (!is.numeric(data[[sx_name]]) ||
        any(is.finite(data[[sx_name]]) & !(data[[sx_name]] %in% c(0, 1)))) {
      data[[sx_name]] <- .hm_normalize_sex(data[[sx_name]], to = "01", fn = "obesity_indices")
    }
  }
  invisible(TRUE)
}

.oi_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  cols <- cols[!is.null(cols)]
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("obesity_indices(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.oi_default_extreme_rules <- function() {
  list(
    weight_kg = c(20, 400),
    height_m  = c(1.2, 2.5),
    waist     = c(30, 200),  # cm
    hip       = c(30, 200)   # cm
  )
}

.oi_extreme_scan <- function(df, cols, rules) {
  count <- 0L
  flags <- list()
  for (cn in cols) {
    if (!cn %in% names(df)) next
    key <- if (cn %in% names(rules)) cn else cn
    rng <- rules[[key]]
    if (is.null(rng)) next
    x <- df[[cn]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

.oi_cap_inputs <- function(df, flags, rules) {
  for (cn in names(flags)) {
    rng <- rules[[cn]]; if (is.null(rng)) next
    x <- df[[cn]]; bad <- flags[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

