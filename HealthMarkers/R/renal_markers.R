
#' Calculate a Suite of Renal Function, Injury, and Excretion Markers
#'
#' Given routine blood and urine assays, `renal_markers()` computes:
#'   - eGFR_cr: CKD-EPI creatinine equation (2009 variant; race factor retained to preserve prior behavior)
#'   - eGFR_cys: CKD-EPI cystatin C equation (if `cystatin_C` provided)
#'   - eGFR_combined: CKD-EPI combined creatinine+cystatin C (if both provided)
#'   - BUN_Cr_ratio: Blood urea nitrogen / serum creatinine
#'   - FE_Urea: Fractional excretion of urea (%)
#'   - NGAL, KIM1, NAG, Beta2Micro, IL18, L_FABP: pass-through urinary injury markers (if mapped)
#'
#' Robust validation is applied, including NA handling (`na_action`), high-missingness
#' diagnostics, safe divisions with a consolidated zero-denominator warning, and an
#' optional input extremes scan/cap. New arguments are appended for backward compatibility.
#'
#' Expected units (no automatic conversion performed):
#' - creatinine (serum): mg/dL
#' - cystatin C (serum): mg/L
#' - BUN (serum): mg/dL
#' - urea_serum, urea_urine: mg/dL
#' - creatinine_urine: mg/dL
#'
#' @param data A data.frame or tibble with renal lab data.
#' @param col_map Named list mapping:
#'   - creatinine -> serum creatinine (mg/dL)
#'   - age -> age (years)
#'   - sex -> sex indicator (1 = male, 0 = female). Also accepts "male"/"female".
#'   - race -> race ("white", "black", or "other"). Also accepts common aliases.
#'   - BUN -> blood urea nitrogen (mg/dL)
#'   - optional cystatin_C -> serum cystatin C (mg/L)
#'   - optional urea_serum -> serum urea (mg/dL)
#'   - optional creatinine_urine -> urine creatinine (mg/dL)
#'   - optional urea_urine -> urine urea (mg/dL)
#'   - optional NGAL, KIM1, NAG, beta2_micro, IL18, L_FABP -> urine injury markers
#' @param na_action One of `c("keep","omit","error")` for handling missing values in
#'   required inputs. Default "keep".
#' @param na_warn_prop Proportion (0-1) threshold for high-missingness diagnostics.
#'   Default 0.2.
#' @param verbose Logical; if `TRUE` (default), prints step-by-step progress
#'   including column mapping, optional input availability, physiological range
#'   information (informational only, values are not altered), the list of
#'   markers being computed, and a per-column results summary.
#'
#' @return A tibble with computed renal markers:
#'   `eGFR_cr`, `eGFR_cys`, `eGFR_combined`, `BUN_Cr_ratio`, `FE_Urea`,
#'   `NGAL`, `KIM1`, `NAG`, `Beta2Micro`, `IL18`, `L_FABP`.
#'   If an ID column is detected in `data` (e.g. `id`, `IID`, `participant_id`),
#'   it is prepended as the first output column.
#'
#' @examples
#' df <- tibble::tibble(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14)
#' cm <- list(creatinine = "Cr", age = "Age", sex = "Sex", race = "Race", BUN = "BUN")
#' renal_markers(df, cm)
#'
#' @note
#' `eGFR_cr` uses the 2009 CKD-EPI creatinine equation (Levey et al. 2009) with the
#' Black-race multiplier (\eqn{\times 1.159}) retained. The 2021 race-free CKD-EPI
#' equations (Inker et al., NEJM 2021) are not yet implemented; the `race` input
#' is accepted for forward compatibility and used only for the 2009 race factor.
#' `eGFR_cys` and `eGFR_combined` use Inker et al. (2012); note that
#' `eGFR_combined` applies its own sex (\eqn{\times 1.008} female) and race
#' (\eqn{\times 1.145} Black) multipliers, which differ from those of `eGFR_cr`.
#' `NGAL`, `KIM1`, `NAG`, `Beta2Micro`, `IL18`, and `L_FABP` are
#' **pass-through** columns — values are returned as-is with no formula applied.
#'
#' @references
#' \insertRef{levey2009ckdepi}{HealthMarkers}
#' \insertRef{inker2012cys}{HealthMarkers}
#' \insertRef{waikar2009feu}{HealthMarkers} (FE_Urea formula source; bib content: Kaplan and Kohn 1992)
#' \insertRef{parikh2011ngal}{HealthMarkers} (clinical context; NGAL is a pass-through biomarker)
#' \insertRef{vaidya2010kim1}{HealthMarkers} (KIM-1 biomarker qualification; pass-through)
#' \insertRef{portilla2008lfabp}{HealthMarkers} (L-FABP as AKI biomarker; pass-through)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
renal_markers <- function(data,
                          col_map      = NULL,
                          na_action    = c("keep", "omit", "error"),
                          na_warn_prop = 0.2,
                          verbose      = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "renal_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data))
    stop(sprintf("%s(): `data` must be a data.frame or tibble.", fn_name), call. = FALSE)

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)

  all_keys <- c("creatinine", "age", "sex", "race", "BUN",
                "cystatin_C", "urea_serum", "creatinine_urine", "urea_urine",
                "NGAL", "KIM1", "NAG", "beta2_micro", "IL18", "L_FABP")
  cm      <- .hm_build_col_map(data, col_map, all_keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # --- Verbose: col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # 1) Validate mapping and data presence
  required <- c("creatinine", "age", "sex", "race", "BUN")
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    rlang::abort(
      paste0(fn_name, "(): missing col_map entries for: ", paste(missing_map, collapse = ", ")),
      class = "healthmarkers_renal_error_missing_map"
    )
  }
  hm_validate_inputs(data, col_map, required_keys = required, fn = fn_name)
  req_cols <- unname(unlist(col_map[required], use.names = FALSE))
  not_in_df <- setdiff(req_cols, names(data))
  if (length(not_in_df)) {
    rlang::abort(
      sprintf("%s(): mapped column(s) not found in data: %s", fn_name, paste(not_in_df, collapse = ", ")),
      class = "healthmarkers_renal_error_missing_columns"
    )
  }
  .rm_validate_df_numeric(data, col_map, cols = c("creatinine", "age", "BUN",
                                                  "cystatin_C", "urea_serum",
                                                  "creatinine_urine", "urea_urine"),
                          warn = TRUE)

  # 2) High-missingness diagnostics
  .rm_warn_high_missing(data, col_map[required], na_warn_prop = na_warn_prop)

  # 3) NA policy on required inputs
  used_cols <- unname(unlist(col_map[required], use.names = FALSE))
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("renal_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_renal_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (sum(!keep) > 0L)
      hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
                msg   = sprintf("renal_markers(): omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      eGFR_cr = numeric(), eGFR_cys = numeric(), eGFR_combined = numeric(),
      BUN_Cr_ratio = numeric(), FE_Urea = numeric(),
      NGAL = numeric(), KIM1 = numeric(), NAG = numeric(),
      Beta2Micro = numeric(), IL18 = numeric(), L_FABP = numeric()
    ))
  }

  # --- Verbose: optional inputs block
  opt_keys <- c("cystatin_C", "urea_serum", "creatinine_urine", "urea_urine",
                "NGAL", "KIM1", "NAG", "beta2_micro", "IL18", "L_FABP")
  if (isTRUE(verbose)) {
    present_opt <- opt_keys[vapply(opt_keys, function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    missing_opt <- setdiff(opt_keys, present_opt)
    idx_deps <- list(
      eGFR_cys      = c("cystatin_C"),
      eGFR_combined = c("cystatin_C"),
      FE_Urea       = c("urea_serum", "creatinine_urine", "urea_urine")
    )
    na_indices <- character(0)
    for (idx in names(idx_deps)) {
      still_missing <- setdiff(idx_deps[[idx]], present_opt)
      if (length(still_missing))
        na_indices <- c(na_indices,
          sprintf("  %s -> NA  [missing: %s]", idx, paste(still_missing, collapse = ", ")))
    }
    lines <- sprintf("%s(): optional inputs", fn_name)
    if (length(present_opt))
      lines <- c(lines, sprintf("  present:  %s", paste(present_opt, collapse = ", ")))
    if (length(missing_opt))
      lines <- c(lines, sprintf("  missing:  %s", paste(missing_opt, collapse = ", ")))
    if (length(na_indices))
      lines <- c(lines, "  indices -> NA:", na_indices)
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  # --- Verbose: physiological range check (informational, values not altered)
  if (isTRUE(verbose)) {
    rules     <- .rm_default_extreme_rules()
    flags     <- .rm_extreme_scan(data, col_map, rules)
    ex_count  <- sum(vapply(flags, function(v) sum(v, na.rm = TRUE), integer(1)))
    if (ex_count > 0L) {
      details <- vapply(names(flags), function(nm) {
        nb <- sum(flags[[nm]], na.rm = TRUE)
        if (nb > 0L) sprintf("  %s: %d value(s) outside plausible range", nm, nb) else ""
      }, character(1))
      details <- details[nzchar(details)]
      hm_inform(
        paste(c(sprintf("%s(): range note (informational, values not altered):", fn_name),
                details), collapse = "\n"),
        level = "inform"
      )
    }
  }

  # --- Verbose: computing markers
  if (isTRUE(verbose)) {
    has_cys  <- !is.null(col_map[["cystatin_C"]]) && col_map[["cystatin_C"]] %in% names(data)
    has_fe   <- all(c("urea_serum","creatinine_urine","urea_urine") %in% {
      opt_keys[vapply(opt_keys, function(k) {
        !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
      }, logical(1))]
    })
    status <- c(
      "  eGFR_cr        [creatinine, age, sex, race]",
      sprintf("  eGFR_cys       %s", if (has_cys) "[cystatin_C, age, sex]" else "NA [cystatin_C missing]"),
      sprintf("  eGFR_combined  %s", if (has_cys) "[creatinine, cystatin_C, age, sex, race]" else "NA [cystatin_C missing]"),
      "  BUN_Cr_ratio   [BUN, creatinine]",
      sprintf("  FE_Urea        %s", if (has_fe) "[urea_serum, urea_urine, creatinine_urine, creatinine]" else "NA [urea/urine inputs missing]")
    )
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # 5) Pull and normalize inputs (sex/race mapping)
  Cr   <- data[[col_map$creatinine]] # mg/dL
  age  <- data[[col_map$age]]
  sexi <- .rm_map_sex(data[[col_map$sex]]) # 1 male, 0 female
  race <- .rm_map_race(data[[col_map$race]]) # "black","white","other"
  BUN  <- data[[col_map$BUN]]

  # Optional inputs
  Cys <- if ("cystatin_C" %in% names(col_map)) data[[col_map$cystatin_C]] else NULL
  Urea_s <- if ("urea_serum" %in% names(col_map)) data[[col_map$urea_serum]] else NULL
  Cr_u   <- if ("creatinine_urine" %in% names(col_map)) data[[col_map$creatinine_urine]] else NULL
  Urea_u <- if ("urea_urine" %in% names(col_map)) data[[col_map$urea_urine]] else NULL

  # 6) Helpers: safe division with zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # 7) eGFR via CKD-EPI creatinine (2009)
  kappa <- ifelse(sexi == 1, 0.9, 0.7)
  alpha <- ifelse(sexi == 1, -0.411, -0.329)
  min_ratio <- pmin(Cr / kappa, 1)
  max_ratio <- pmax(Cr / kappa, 1)
  factor_race   <- ifelse(race == "black", 1.159, 1)   # Levey 2009: Black race multiplier
  factor_sex_cr <- ifelse(sexi == 1, 1, 1.018)          # Levey 2009: female multiplier
  eGFR_cr <- 141 * (min_ratio^alpha) * (max_ratio^-1.209) *
    (0.993^age) * factor_race * factor_sex_cr

  # 8) eGFR via cystatin C & combined equations
  if (!is.null(Cys)) {
    min_cys <- pmin(Cys / 0.8, 1)
    max_cys <- pmax(Cys / 0.8, 1)
    cys_sex <- ifelse(sexi == 1, 1, 0.932)
    eGFR_cys <- 133 * (min_cys^-0.499) * (max_cys^-1.328) *
      (0.996^age) * cys_sex
    # combined creatinine + cystatin (Inker 2012; sex/race factors differ from eGFR_cr)
    factor_sex_comb  <- ifelse(sexi == 1, 1, 1.008)   # Inker 2012: female multiplier
    factor_race_comb <- ifelse(race == "black", 1.145, 1)  # Inker 2012: race multiplier
    eGFR_combined <- 135 *
      (min_ratio^alpha) * (max_ratio^-0.601) *
      (min_cys^-0.375) * (max_cys^-0.711) *
      (0.995^age) * factor_race_comb * factor_sex_comb
  } else {
    eGFR_cys <- NA_real_
    eGFR_combined <- NA_real_
  }

  # 9) BUN/Cr ratio
  BUN_Cr_ratio <- safe_div(BUN, Cr, "BUN_Cr_ratio")

  # 10) Fractional excretion of urea (%)
  if (!is.null(Urea_s) && !is.null(Cr_u) && !is.null(Urea_u)) {
    FE_Urea <- 100 * safe_div(safe_div(Urea_u, Urea_s, "FE_Urea_UreaS"),
                              safe_div(Cr_u, Cr, "FE_Urea_CrS"),
                              "FE_Urea_final")
  } else {
    FE_Urea <- NA_real_
  }

  # 11) Urine injury markers (pass-through if mapped)
  get_mark <- function(key) {
    if (key %in% names(col_map) && col_map[[key]] %in% names(data)) {
      as.numeric(data[[ col_map[[key]] ]])
    } else {
      rep(NA_real_, nrow(data))
    }
  }
  NGAL        <- get_mark("NGAL")
  KIM1        <- get_mark("KIM1")
  NAG         <- get_mark("NAG")
  Beta2Micro  <- get_mark("beta2_micro")
  IL18        <- get_mark("IL18")
  L_FABP      <- get_mark("L_FABP")

  out <- tibble::tibble(
    eGFR_cr        = as.numeric(eGFR_cr),
    eGFR_cys       = as.numeric(eGFR_cys),
    eGFR_combined  = as.numeric(eGFR_combined),
    BUN_Cr_ratio   = as.numeric(BUN_Cr_ratio),
    FE_Urea        = as.numeric(FE_Urea),
    NGAL           = as.numeric(NGAL),
    KIM1           = as.numeric(KIM1),
    NAG            = as.numeric(NAG),
    Beta2Micro     = as.numeric(Beta2Micro),
    IL18           = as.numeric(IL18),
    L_FABP         = as.numeric(L_FABP)
  )

  # 12) Emit consolidated zero-denominator warning if any
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("renal_markers(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

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

# --------------------- internal helpers ---------------------------------------

.rm_validate_df_numeric <- function(data, col_map, cols, warn = TRUE) {
  for (key in cols) {
    if (!(key %in% names(col_map))) next
    cn <- col_map[[key]]
    if (!(cn %in% names(data))) next
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      if (isTRUE(warn)) {
        introduced <- sum(is.na(data[[cn]]) & !is.na(old))
        if (introduced > 0) rlang::warn(sprintf("renal_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
  }
  invisible(TRUE)
}

.rm_warn_high_missing <- function(data, mapped_names, na_warn_prop = 0.2) {
  cols <- unname(unlist(mapped_names, use.names = FALSE))
  for (cn in cols) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("renal_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.rm_map_sex <- function(sex) {
  s <- tolower(as.character(sex))
  out <- rep(NA_integer_, length(s))
  # numeric-like mapping
  is_num <- suppressWarnings(!is.na(as.numeric(s)))
  out[is_num & s %in% c("1")] <- 1L
  out[is_num & s %in% c("0", "2")] <- 0L
  # character mapping
  out[s %in% c("male","m")] <- 1L
  out[s %in% c("female","f","fm","woman","girl")] <- 0L
  bad <- sum(is.na(out))
  if (bad > 0) rlang::warn(sprintf("renal_markers(): 'sex' has %d unmapped values; set to NA.", bad))
  out
}

.rm_map_race <- function(race) {
  r <- tolower(as.character(race))
  out <- ifelse(
    r %in% c("black","african-american","african american","aa","blk"), "black",
    ifelse(r %in% c("white","caucasian","european","non-hispanic white"), "white", "other")
  )
  out[is.na(out) | out == ""] <- "other"
  out
}

.rm_default_extreme_rules <- function() {
  list(
    creatinine       = c(0.1, 15),    # mg/dL
    BUN              = c(1, 200),     # mg/dL
    cystatin_C       = c(0.2, 8),     # mg/L
    urea_serum       = c(1, 300),     # mg/dL
    creatinine_urine = c(1, 500),     # mg/dL
    urea_urine       = c(1, 2000)     # mg/dL
  )
}

.rm_extreme_scan <- function(data, col_map, rules) {
  flags <- list()
  for (key in names(rules)) {
    if (!(key %in% names(col_map))) next
    cn <- col_map[[key]]
    if (!(cn %in% names(data))) next
    rng <- rules[[key]]
    x <- data[[cn]]
    flags[[key]] <- is.finite(x) & (x < rng[1] | x > rng[2])
  }
  flags
}



