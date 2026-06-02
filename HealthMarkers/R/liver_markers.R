
#' Compute liver-related indices (FLI, NFS, APRI, FIB-4, BARD, ALBI, MELD-XI) with validation and diagnostics
#'
#' Given routine labs and anthropometry, computes:
#' - FLI      - Fatty Liver Index (Bedogni et al. 2006)
#' - NFS      - NAFLD Fibrosis Score (Angulo et al. 2007)
#' - APRI     - AST-to-Platelet Ratio Index
#' - FIB4     - Fibrosis-4 Index
#' - BARD     - BMI-AST/ALT-Diabetes score
#' - ALBI     - Albumin-Bilirubin score
#' - MELD_XI  - MELD excluding INR
#'
#' Enhancements:
#' - Robust input validation (columns present, types) with informative errors.
#' - Configurable NA policy and optional extreme-value scanning/capping.
#' - Data-quality warnings (high missingness, non-positive logs, zero denominators).
#' - Verbose stepwise progress and completion summary.
#'
#' Units (no automatic conversion):
#' - BMI: kg/m^2; Waist: cm; TG: mg/dL; GGT/AST/ALT: U/L; Platelets: 10^9/L; Albumin: g/L; Bilirubin: mg/dL; Creatinine: mg/dL.
#' - ALBI uses bilirubin in mumol/L internally (converted as bilirubin (mg/dL) * 17.1).
#'
#' @param data A data.frame or tibble containing your liver and anthropometry data.
#' @param col_map Named list mapping these keys -> column names in `data`:
#'   - `BMI` (kg/m^2), `waist` (cm), `TG` (mg/dL), `GGT` (U/L),
#'   - `age` (years), `AST` (U/L), `ALT` (U/L), `platelets` (10^9/L),
#'   - `albumin` (g/L), `diabetes` (0/1 or logical),
#'   - `bilirubin` (mg/dL), `creatinine` (mg/dL).
#' @param verbose Logical; if `TRUE` (default), prints column mapping, input
#'   availability, physiological range information (informational only, values
#'   not altered), the list of markers being computed with their inputs, and a
#'   per-column results summary.
#' @param na_action One of `c("keep","omit","error")` controlling missing-data
#'   policy. Default "keep".
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for
#'   high-missingness warnings. Default 0.2.
#'
#' @return A tibble with one column per marker: `FLI`, `NFS`, `APRI`, `FIB4`,
#'   `BARD`, `ALBI`, `MELD_XI`. If an ID column is detected in `data` (e.g.
#'   `id`, `IID`, `participant_id`), it is prepended as the first output column.
#'
#' @details
#' Formulas 
#' - FLI      = logistic(0.953*ln(TG) + 0.139*BMI + 0.718*ln(GGT) + 0.053*waist - 15.745) * 100
#' - NFS      = -1.675 + 0.037*age + 0.094*BMI + 1.13*diabetes + 0.99*(AST/ALT) - 0.013*platelets - 0.066*albumin
#'   (albumin in g/L; Angulo 2007 published coefficient -0.66 was for g/dL, divided by 10 here)
#' - APRI     = (AST / 40) / platelets * 100; assumes AST upper limit of normal = 40 U/L
#' - FIB-4    = (age * AST) / (platelets * sqrt(ALT))
#' - BARD     = +1 if BMI>=28, +2 if AST/ALT>=0.8, +1 if diabetes present; sum in 0-4
#' - ALBI     = 0.66*log10(bilirubin (mumol/L)) - 0.0852*albumin (g/L)
#' - MELD-XI  = 5.11*ln(bilirubin (mg/dL)) + 11.76*ln(creatinine (mg/dL)) + 9.44
#'
#' @seealso [inflammatory_markers()], [kidney_failure_risk()], [iAge()]
#'
#' @examples
#' # Quick smoke-test
#' df <- data.frame(ALT = 25, AST = 20, BMI = 24, platelets = 250)
#' liver_markers(df, verbose = FALSE)
#'
#' \donttest{
#' library(tibble)
#' df <- tibble(
#'   BMI           = 24,
#'   waist         = 80,
#'   TG = 150,
#'   GGT           = 30,
#'   age           = 45,
#'   AST           = 25,
#'   ALT           = 20,
#'   platelets     = 250,
#'   albumin       = 42,
#'   diabetes      = FALSE,
#'   bilirubin     = 1.0,
#'   creatinine    = 0.9
#' )
#' liver_markers(df)
#' liver_markers(df, verbose = FALSE)
#' }
#'
#' @references
#' \insertRef{bedogni2006fli}{HealthMarkers}
#' \insertRef{angulo2007nfs}{HealthMarkers}
#' \insertRef{wai2003apri}{HealthMarkers}
#' \insertRef{sterling2006fib4}{HealthMarkers}
#' \insertRef{harrison2008bard}{HealthMarkers}
#' \insertRef{johnson2015albi}{HealthMarkers}
#' \insertRef{heuman2007meldxi}{HealthMarkers}
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
liver_markers <- function(data,
                          col_map      = NULL,
                          verbose      = TRUE,
                          na_action    = c("keep", "omit", "error"),
                          na_warn_prop = 0.2) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "liver_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data))
    stop(sprintf("%s(): `data` must be a data.frame or tibble.", fn_name), call. = FALSE)

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)

  required <- c("BMI","waist","TG","GGT","age","AST","ALT",
                "platelets","albumin","diabetes","bilirubin","creatinine")

  # --- Build col_map (infer from dictionary + materialise aliases) ---
  all_lm_keys <- c(required, "weight", "height")
  cm      <- .hm_build_col_map(data, col_map, all_lm_keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # Strict check: error if a user-provided mapping points to a column not in data
  if (length(cm$user_keys)) {
    bad_cols <- setdiff(
      unlist(col_map[cm$user_keys], use.names = FALSE),
      names(data))
    if (length(bad_cols))
      rlang::abort(
        paste0(fn_name, "(): mapped columns not found in data: ", paste(bad_cols, collapse = ", ")),
        class = "healthmarkers_liver_error_missing_columns")
  }

  # --- Pre-computation: BMI from weight + height if absent
  if ((is.null(col_map[["BMI"]]) || !(col_map[["BMI"]] %in% names(data))) &&
      all(c("weight", "height") %in% names(data))) {
    h   <- as.numeric(data[["height"]])
    w   <- as.numeric(data[["weight"]])
    h_m <- ifelse(is.finite(h) & h > 3, h / 100, h)
    data[["BMI"]] <- w / (h_m ^ 2)
    col_map[["BMI"]] <- "BMI"
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): pre-computation: BMI computed from weight, height", fn_name),
                level = "inform")
  }

  # Additional robust validation (structural only)
  .lm_validate_args(data, if (length(col_map) > 0L) col_map else list(placeholder = "x"),
                    na_warn_prop, NULL)

  # Helper: pull column as numeric vector, NA if key unavailable
  .lm_col <- function(k) {
    cn <- col_map[[k]]
    if (is.null(cn)) rep(NA_real_, nrow(data)) else data[[cn]]
  }

  avail_cols <- unlist(col_map[intersect(required, names(col_map))], use.names = FALSE)

  # --- Verbose: col_map (user-provided and inferred)
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # HM-CS v3: coerce to numeric for all available required inputs
  for (cn in avail_cols) {
    if (identical(cn, col_map[["diabetes"]])) next
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }
  # Missingness warnings only when na_action == "warn"
  if (identical(na_action, "warn")) {
    .lm_warn_high_missing(data, avail_cols, na_warn_prop = na_warn_prop)
  }

  # NA policy (only against available columns)
  if (identical(na_action, "error")) {
    avail_req <- intersect(required, names(col_map))
    any_na <- Reduce(`|`, lapply(avail_req, function(k) is.na(data[[col_map[[k]]]])))
    if (any(any_na)) {
      rlang::abort("liver_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_liver_error_missing_values")
    }
  } else if (identical(na_action, "omit")) {
    avail_req <- intersect(required, names(col_map))
    keep <- !Reduce(`|`, lapply(avail_req, function(k) is.na(data[[col_map[[k]]]])))
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep)),
                level = "inform")
    data <- data[keep, , drop = FALSE]
  } # "keep" leaves NA as-is

  # --- Verbose: inputs present/absent and which markers will be NA
  if (isTRUE(verbose)) {
    avail_keys <- names(col_map)[vapply(names(col_map), function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    absent_keys <- setdiff(required, avail_keys)
    idx_deps <- list(
      FLI     = c("BMI", "waist", "TG", "GGT"),
      NFS     = c("age", "BMI", "diabetes", "AST", "ALT", "platelets", "albumin"),
      APRI    = c("AST", "platelets"),
      FIB4    = c("age", "AST", "platelets", "ALT"),
      BARD    = c("BMI", "AST", "ALT", "diabetes"),
      ALBI    = c("bilirubin", "albumin"),
      MELD_XI = c("bilirubin", "creatinine")
    )
    na_indices <- character(0)
    for (idx in names(idx_deps)) {
      miss_for_idx <- setdiff(idx_deps[[idx]], avail_keys)
      if (length(miss_for_idx))
        na_indices <- c(na_indices,
          sprintf("  %s -> NA  [missing: %s]", idx, paste(miss_for_idx, collapse = ", ")))
    }
    lines <- sprintf("%s(): optional inputs", fn_name)
    if (length(avail_keys))
      lines <- c(lines, sprintf("  present:  %s", paste(intersect(required, avail_keys), collapse = ", ")))
    if (length(absent_keys))
      lines <- c(lines, sprintf("  missing:  %s", paste(absent_keys, collapse = ", ")))
    if (length(na_indices))
      lines <- c(lines, "  indices -> NA:", na_indices)
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  # --- Verbose: physiological range check (informational, values not altered)
  if (isTRUE(verbose)) {
    rules_info <- .lm_default_extreme_rules()
    ex_info    <- .lm_extreme_scan(data, col_map, rules_info, required)
    if (ex_info$count > 0L) {
      details <- vapply(names(ex_info$flags), function(cn) {
        nb <- sum(ex_info$flags[[cn]], na.rm = TRUE)
        if (nb > 0L) sprintf("  %s: %d value(s) outside plausible range", cn, nb) else ""
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
    avail_keys <- names(col_map)[vapply(names(col_map), function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    idx_deps2 <- list(
      FLI     = c("BMI", "waist", "TG", "GGT"),
      NFS     = c("age", "BMI", "diabetes", "AST", "ALT", "platelets", "albumin"),
      APRI    = c("AST", "platelets"),
      FIB4    = c("age", "AST", "platelets", "ALT"),
      BARD    = c("BMI", "AST", "ALT", "diabetes"),
      ALBI    = c("bilirubin", "albumin"),
      MELD_XI = c("bilirubin", "creatinine")
    )
    status <- vapply(names(idx_deps2), function(idx) {
      miss_in <- setdiff(idx_deps2[[idx]], avail_keys)
      if (length(miss_in) == 0L)
        sprintf("  %-10s [%s]", idx, paste(idx_deps2[[idx]], collapse = ", "))
      else
        sprintf("  %-10s NA [missing: %s]", idx, paste(miss_in, collapse = ", "))
    }, character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # Pull vectors (NA vector when column unavailable)
  BMI        <- .lm_col("BMI")
  waist      <- .lm_col("waist")
  TG         <- .lm_col("TG")
  GGT        <- .lm_col("GGT")
  age        <- .lm_col("age")
  AST        <- .lm_col("AST")
  ALT        <- .lm_col("ALT")
  platelets  <- .lm_col("platelets")
  albumin    <- .lm_col("albumin")
  bilirubin  <- .lm_col("bilirubin")
  creatinine <- .lm_col("creatinine")

  # Diabetes: default to 0 (no diabetes) when column absent
  diabetes_x <- {
    cn <- col_map[["diabetes"]]
    if (is.null(cn)) rep(0L, nrow(data)) else data[[cn]]
  }

  # Diabetes coercion with diagnostics (preserve prior behavior: as.integer)
  diab_ok <- is.logical(diabetes_x) || all(diabetes_x %in% c(0,1,NA))
  if (!diab_ok) {
    rlang::warn("liver_markers(): `diabetes` not in {0,1,TRUE,FALSE}; coercing with as.integer().")
  }
  diabetes <- as.integer(diabetes_x)

  # Denominator/transform diagnostics
  denom_info <- list(
    platelets = list(name = "platelets (APRI/FIB4 denominators)", vec = platelets)
  )
  .lm_warn_zero_denoms(denom_info)
  .lm_warn_nonpositive_for_log(list(
    TG = TG,
    GGT = GGT,
    bilirubin = bilirubin,
    creatinine = creatinine
  ))
  .lm_warn_nonpositive_for_sqrt(list(ALT = ALT))

  # Compute markers (unchanged formulas)
  # FLI
  L <- 0.953 * log(TG) + 0.139 * BMI + 0.718 * log(GGT) + 0.053 * waist - 15.745
  FLI <- exp(L) / (1 + exp(L)) * 100

  # NFS (albumin in g/L; published coefficient -0.66 was for g/dL, divided by 10)
  NFS <- -1.675 +
    0.037 * age +
    0.094 * BMI +
    1.13 * diabetes +
    0.99 * (AST / ALT) -
    0.013 * platelets -
    0.066 * albumin

  # APRI
  APRI <- (AST / 40) / platelets * 100

  # FIB-4
  FIB4 <- (age * AST) / (platelets * sqrt(ALT))

  # BARD (AST/ALT >= 0.8 scores 2 points per Harrison 2008; range 0-4)
  BARD <- as.integer((BMI >= 28) + 2L * (AST / ALT >= 0.8) + (diabetes == 1))

  # ALBI
  bili_umol <- bilirubin * 17.1
  ALBI <- log10(bili_umol) * 0.66 + albumin * -0.0852

  # MELD-XI
  MELD_XI <- 5.11 * log(bilirubin) + 11.76 * log(creatinine) + 9.44

  out <- tibble::tibble(
    FLI     = FLI,
    NFS     = NFS,
    APRI    = APRI,
    FIB4    = FIB4,
    BARD    = BARD,
    ALBI    = ALBI,
    MELD_XI = MELD_XI
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

  return(out)
}

# ---- internal helpers (not exported) -----------------------------------------

.lm_validate_args <- function(data, col_map, na_warn_prop, extreme_rules = NULL) {
  if (!is.data.frame(data)) {
    rlang::abort("liver_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_liver_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("liver_markers(): `col_map` must be a named list.",
                 class = "healthmarkers_liver_error_colmap_type")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L &&
        is.finite(na_warn_prop) && na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("liver_markers(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_liver_error_na_warn_prop")
  }
  invisible(TRUE)
}

.lm_default_extreme_rules <- function() {
  list(
    BMI           = c(10, 70),
    waist         = c(40, 200),
    TG            = c(10, 1500),   # mg/dL
    GGT           = c(1, 2000),    # U/L
    age           = c(18, 120),
    AST           = c(1, 5000),
    ALT           = c(1, 5000),
    platelets     = c(10, 1000),   # 10^9/L
    albumin       = c(15, 60),     # g/L
    bilirubin     = c(0.1, 40),    # mg/dL
    creatinine    = c(0.2, 20)     # mg/dL
  )
}

.lm_extreme_scan <- function(df, col_map, rules, required) {
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

.lm_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("liver_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    # Negative values quick check (vars expected >= 0)
    neg_n <- sum(is.finite(x) & x < 0)
    if (neg_n > 0L) {
      rlang::warn(sprintf("liver_markers(): column '%s' contains %d negative values; check units.", cn, neg_n))
    }
  }
  invisible(TRUE)
}

.lm_warn_zero_denoms <- function(denoms) {
  msgs <- character(0)
  for (nm in names(denoms)) {
    v <- denoms[[nm]]$vec
    lab <- denoms[[nm]]$name
    n0 <- sum(is.finite(v) & v == 0)
    if (n0 > 0L) msgs <- c(msgs, sprintf("%s==0: %d", lab, n0))
  }
  if (length(msgs)) {
    rlang::warn(sprintf("liver_markers(): zero denominators detected -> %s. Ratios may yield Inf/NaN.", paste(msgs, collapse = ", ")))
  }
  invisible(TRUE)
}

.lm_warn_nonpositive_for_log <- function(named_vecs) {
  for (nm in names(named_vecs)) {
    v <- named_vecs[[nm]]
    nbad <- sum(is.finite(v) & v <= 0)
    if (nbad > 0L) {
      rlang::warn(sprintf("liver_markers(): '%s' contains %d non-positive values; log() undefined.", nm, nbad))
    }
  }
  invisible(TRUE)
}

.lm_warn_nonpositive_for_sqrt <- function(named_vecs) {
  for (nm in names(named_vecs)) {
    v <- named_vecs[[nm]]
    nbad <- sum(is.finite(v) & v < 0)
    if (nbad > 0L) {
      rlang::warn(sprintf("liver_markers(): '%s' contains %d negative values; sqrt() undefined for negatives.", nm, nbad))
    }
  }
  invisible(TRUE)
}

