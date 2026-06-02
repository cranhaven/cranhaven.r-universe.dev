
#' Calculate lipid-panel markers, Visceral Adiposity Index (VAI),
#' Lipid Accumulation Product (LAP), and TyG-BMI index
#'
#' Given total cholesterol, HDL, TG (and optionally LDL, ApoB/ApoA1,
#' waist, BMI, glucose), computes:
#' - `non_HDL_c`, `remnant_c`
#' - `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`
#' - `ApoB_ApoA1`
#' - `VAI_Men`, `VAI_Women`
#' - `LAP_Men`, `LAP_Women`
#' - `TyG_BMI`
#'
#' Assumed units (no automatic conversion except where noted):
#' - TC, HDL_c, TG, LDL_c: mmol/L
#' - glucose: mmol/L (converted to mg/dL internally for TyG_BMI)
#' - waist: cm
#' - BMI: kg/m^2
#'
#' @param data A `data.frame` or `tibble` containing your lipid
#'   (and optional anthropometry/glucose) data.
#' @param col_map Named list mapping:
#'   - `TC`    -> total cholesterol
#'   - `HDL_c` -> HDL-C
#'   - `TG`    -> triglycerides
#'   - `LDL_c` -> (optional) LDL-C; if absent, estimated via Friedewald
#'   - `ApoB`, `ApoA1` -> (optional) apolipoproteins
#'   - `waist` -> (optional) waist circumference (cm)
#'   - `BMI`   -> (optional) body mass index (kg/m^2)
#'   - `glucose` -> (optional) fasting glucose (mmol/L); used for TyG_BMI
#' @param na_action One of `c("keep","omit","error","ignore","warn")`.
#'   - keep/ignore: compute and propagate NA in outputs
#'   - omit: drop rows with NA in required inputs (TC, HDL_c, TG)
#'   - error: abort if any required input contains NA
#'   - warn: like keep, but emit missingness warnings
#' @param na_warn_prop Proportion (0-1) threshold for high-missingness
#'   warnings when `na_action = "warn"`. Default 0.2.
#' @param verbose Logical; if `TRUE` (default), prints step-by-step progress
#'   including column mapping, optional input availability, pre-computation
#'   notes, physiological range information (informational only, values are
#'   not altered), the list of markers being computed with their inputs, and a
#'   per-column results summary.
#'
#' @return A tibble with computed lipid markers.
#'   Required outputs (always present):
#'   `non_HDL_c`, `remnant_c`, `ratio_TC_HDL`, `ratio_TG_HDL`,
#'   `ratio_LDL_HDL`, `ApoB_ApoA1`.
#'   Optional outputs (present when inputs available):
#'   `VAI_Men`, `VAI_Women` (waist + BMI required);
#'   `LAP_Men`, `LAP_Women` (waist required);
#'   `TyG_BMI` (BMI + glucose required).
#'   If an ID column is detected in `data` (e.g. `id`, `IID`,
#'   `participant_id`), it is prepended as the first output column.
#'
#' @details
#' Pre-computation (one level deep):
#' - If `BMI` is absent but `weight` (kg) and `height` (m or cm) are
#'   present, BMI is computed automatically.
#' - If `glucose` is absent but `G0` is present (or vice versa), the alias
#'   is derived automatically.
#' - If `LDL_c` is absent, it is always estimated via Friedewald
#'   (TC - HDL - TG/2.2, mmol/L form). An informational message is emitted when
#'   `verbose = TRUE`.
#'
#' @references
#' \insertRef{friedewald1972}{HealthMarkers}
#' \insertRef{amato2010vai}{HealthMarkers}
#' \insertRef{kahn2005lap}{HealthMarkers}
#' \insertRef{lee2016tyg}{HealthMarkers}
#' \insertRef{lee2020tygbmi}{HealthMarkers} (clinical application)
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#'
#' @examples
#' df <- data.frame(TC = c(5.2, 6.1), HDL_c = c(1.3, 1.1), TG = c(1.8, 2.3),
#'                  LDL_c = c(3.2, 4.1), waist = c(88, 95), BMI = c(26, 29))
#' # Full verbose output (default)
#' lipid_markers(df)
#' # Suppress messaging for batch use
#' lipid_markers(df, verbose = FALSE)
#' # Pre-compute BMI from weight and height
#' df2 <- data.frame(TC = 5.2, HDL_c = 1.3, TG = 1.8, weight = 70, height = 175)
#' lipid_markers(df2, verbose = FALSE)
#' @export
lipid_markers <- function(
  data,
  col_map      = NULL,
  na_action    = c("keep", "omit", "error", "ignore", "warn"),
  na_warn_prop = 0.2,
  verbose      = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "lipid_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data))
    stop(sprintf("%s(): `data` must be a data.frame or tibble.", fn_name), call. = FALSE)

  # --- Detect and preserve ID column -----------------------------------------
  id_col <- .hm_detect_id_col(data)

  # --- Build col_map: infer missing keys, materialize aliases ----------------
  all_lm_keys <- c("TC", "HDL_c", "TG", "LDL_c", "ApoB", "ApoA1", "waist", "BMI", "glucose")
  req_keys    <- c("TC", "HDL_c", "TG")
  opt_keys    <- c("LDL_c", "ApoB", "ApoA1", "waist", "BMI", "glucose")

  cm      <- .hm_build_col_map(data, col_map, all_lm_keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # --- Verbose: column mapping (user-provided vs inferred) -------------------
  .hm_log_cols(cm, col_map, fn_name, verbose, keys = all_lm_keys)

  # --- Pre-computation: fill still-missing required keys from raw inputs -----
  deps <- .lm_precompute_deps()
  is_missing_key <- function(k) is.null(col_map[[k]]) || !(col_map[[k]] %in% names(data))
  missing_req <- req_keys[vapply(req_keys, is_missing_key, logical(1))]
  if (length(missing_req) > 0L) {
    pc   <- .hm_precompute_from_deps(data, deps, missing_req, fn = fn_name, verbose = verbose)
    data <- pc$data
    for (k in missing_req) if (is.null(col_map[[k]]) && k %in% names(data)) col_map[[k]] <- k
    missing_req <- req_keys[vapply(req_keys, is_missing_key, logical(1))]
  }
  if (length(missing_req))
    stop(sprintf("%s(): missing required columns: %s",
                 fn_name, paste(missing_req, collapse = ", ")), call. = FALSE)

  req_cols <- unname(unlist(col_map[req_keys]))
  miss     <- setdiff(req_cols, names(data))
  if (length(miss))
    stop(sprintf("%s(): missing required columns: %s",
                 fn_name, paste(miss, collapse = ", ")), call. = FALSE)

  # --- Pre-computation: fill missing optional keys (BMI, glucose from G0) ----
  missing_opt <- opt_keys[vapply(opt_keys, is_missing_key, logical(1))]
  if (length(missing_opt) > 0L) {
    pc          <- .hm_precompute_from_deps(data, deps, missing_opt, fn = fn_name, verbose = verbose)
    newly_added <- setdiff(names(pc$data), names(data))
    if (length(newly_added)) {
      data <- pc$data
      for (k in newly_added) if (is.null(col_map[[k]]) && k %in% names(data)) col_map[[k]] <- k
    }
  }

  # --- Determine which optional keys are now present --------------------------
  present_opt   <- vapply(opt_keys, function(k) {
    nm <- col_map[[k]]; !is.null(nm) && nm %in% names(data)
  }, logical(1))
  used_opt_keys <- opt_keys[present_opt]
  missing_opt   <- setdiff(opt_keys, used_opt_keys)

  # --- Friedewald LDL if absent (always derivable: TC, HDL_c, TG required) ---
  if (!"LDL_c" %in% used_opt_keys) {
    data[["LDL_c_fried"]] <- data[[col_map$TC]] - data[[col_map$HDL_c]] - data[[col_map$TG]] / 2.2
    col_map[["LDL_c"]]    <- "LDL_c_fried"
    used_opt_keys         <- c(used_opt_keys, "LDL_c")
    missing_opt           <- setdiff(missing_opt, "LDL_c")
    if (isTRUE(verbose))
      hm_inform(
        sprintf("%s(): pre-computation: LDL_c estimated via Friedewald (TC - HDL - TG/2.2)", fn_name),
        level = "inform"
      )
  }

  used_keys <- c(req_keys, used_opt_keys)
  used_cols <- unname(unlist(col_map[used_keys]))

  # --- Verbose: optional inputs block -----------------------------------------
  if (isTRUE(verbose)) {
    idx_deps <- list(
      ApoB_ApoA1 = c("ApoB", "ApoA1"),
      VAI        = c("waist", "BMI"),
      LAP        = c("waist"),
      TyG_BMI    = c("BMI", "glucose")
    )
    na_indices <- character(0)
    for (idx in names(idx_deps)) {
      still_missing <- setdiff(idx_deps[[idx]], used_opt_keys)
      if (length(still_missing))
        na_indices <- c(na_indices,
          sprintf("  %s -> NA  [missing: %s]", idx, paste(still_missing, collapse = ", ")))
    }
    derivable_hints <- character(0)
    for (k in setdiff(missing_opt, "LDL_c")) {
      dep <- deps[[k]]
      if (!is.null(dep)) {
        prereqs_absent <- setdiff(dep$needs, names(data))
        if (length(prereqs_absent) == 0L) {
          derivable_hints <- c(derivable_hints,
            sprintf("  %s can be derived from: %s", k, paste(dep$needs, collapse = ", ")))
        } else {
          derivable_hints <- c(derivable_hints,
            sprintf("  %s: provide %s to enable", k, paste(dep$needs, collapse = ", ")))
        }
      }
    }
    shown_present <- setdiff(used_opt_keys, "LDL_c")
    shown_missing <- setdiff(missing_opt, "LDL_c")
    lines <- sprintf("%s(): optional inputs", fn_name)
    if (length(shown_present))
      lines <- c(lines, sprintf("  present:  %s", paste(shown_present, collapse = ", ")))
    if (length(shown_missing))
      lines <- c(lines, sprintf("  missing:  %s", paste(shown_missing, collapse = ", ")))
    if (length(na_indices))
      lines <- c(lines, "  indices -> NA:", na_indices)
    if (length(derivable_hints))
      lines <- c(lines, "  derivable:", derivable_hints)
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  # --- Coerce used columns to numeric -----------------------------------------
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old_vals <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old_vals))
      introduced_na <- sum(is.na(data[[cn]]) & !is.na(old_vals))
      if (introduced_na > 0L)
        warning(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na),
                call. = FALSE)
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # --- NA handling ------------------------------------------------------------
  nonfin_mask <- vapply(used_cols, function(cn) any(!is.finite(data[[cn]])), logical(1))
  if (na_action == "warn" && any(nonfin_mask))
    warning(sprintf("Non-finite values in: %s; treated as NA.",
                    paste(used_cols[nonfin_mask], collapse = ", ")), call. = FALSE)
  if (na_action == "error" && any(!stats::complete.cases(data[, req_cols, drop = FALSE])))
    stop(sprintf("%s(): missing or non-finite values in required inputs with na_action='error'.",
                 fn_name), call. = FALSE)

  keep_rows <- if (na_action == "omit")
    stats::complete.cases(data[, req_cols, drop = FALSE]) else rep(TRUE, nrow(data))
  d <- data[keep_rows, , drop = FALSE]

  # --- Helper: is key available in d? ----------------------------------------
  has <- function(k) !is.null(col_map[[k]]) && col_map[[k]] %in% names(d)

  # --- Verbose: physiological range check (informational, not altered) --------
  if (isTRUE(verbose)) {
    rules    <- .lm_default_extreme_rules()
    remapped <- list()
    for (nm in names(rules)) {
      cn <- if (!is.null(col_map[[nm]])) col_map[[nm]] else nm
      remapped[[cn]] <- rules[[nm]]
    }
    scan_vars <- intersect(used_cols, names(remapped))
    ex_flags  <- list(); total <- 0L
    for (v in scan_vars) {
      x <- d[[v]]; rng <- remapped[[v]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_flags[[v]] <- bad; total <- total + sum(bad)
    }
    if (total > 0L) {
      details <- vapply(names(ex_flags), function(v) {
        nb <- sum(ex_flags[[v]])
        if (nb > 0L) sprintf("  %s: %d value(s) outside plausible range", v, nb) else ""
      }, character(1))
      details <- details[nzchar(details)]
      hm_inform(
        paste(c(sprintf("%s(): range note (informational, values not altered):", fn_name),
                details), collapse = "\n"),
        level = "inform"
      )
    }
  }

  # --- Verbose: computing markers list ----------------------------------------
  if (isTRUE(verbose)) {
    status <- c(
      "  non_HDL_c     [TC, HDL_c]",
      "  remnant_c     [TC, HDL_c, LDL_c]",
      "  ratio_TC_HDL  [TC, HDL_c]",
      "  ratio_TG_HDL  [TG, HDL_c]",
      "  ratio_LDL_HDL [LDL_c, HDL_c]",
      sprintf("  ApoB_ApoA1    %s",
              if (has("ApoB") && has("ApoA1")) "[ApoB, ApoA1]"
              else "NA [ApoB/ApoA1 missing]"),
      sprintf("  VAI_Men/Women %s",
              if (has("waist") && has("BMI")) "[waist, BMI, TG, HDL_c]"
              else "NA [waist/BMI missing]"),
      sprintf("  LAP_Men/Women %s",
              if (has("waist")) "[waist, TG]"
              else "NA [waist missing]"),
      sprintf("  TyG_BMI       %s",
              if (has("BMI") && has("glucose")) "[TG, glucose, BMI]"
              else "NA [BMI/glucose missing]")
    )
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # --- Compute markers --------------------------------------------------------
  sdiv <- function(a, b) { z <- a / b; z[!is.finite(z)] <- NA_real_; z }
  lg   <- function(x) { y <- log(x); y[!is.finite(y)] <- NA_real_; y }

  TC  <- d[[col_map$TC]]
  HDL <- d[[col_map$HDL_c]]
  TG  <- d[[col_map$TG]]
  LDL <- d[[col_map$LDL_c]]

  ApoB_ApoA1 <- if (has("ApoB") && has("ApoA1")) {
    sdiv(d[[col_map$ApoB]], d[[col_map$ApoA1]])
  } else {
    rep(NA_real_, nrow(d))
  }

  out <- tibble::tibble(
    non_HDL_c     = TC - HDL,
    remnant_c     = TC - (HDL + LDL),
    ratio_TC_HDL  = sdiv(TC, HDL),
    ratio_TG_HDL  = sdiv(TG, HDL),
    ratio_LDL_HDL = sdiv(LDL, HDL),
    ApoB_ApoA1    = ApoB_ApoA1
  )

  if (has("waist") && has("BMI")) {
    W  <- d[[col_map$waist]]
    BM <- d[[col_map$BMI]]
    out$VAI_Men   <- (W / (39.68 + 1.88 * BM)) * sdiv(TG, 1.03) * sdiv(1.31, HDL)
    out$VAI_Women <- (W / (36.58 + 1.89 * BM)) * sdiv(TG, 0.81) * sdiv(1.52, HDL)
  }

  if (has("waist")) {
    W <- d[[col_map$waist]]
    out$LAP_Men   <- (W - 65) * TG
    out$LAP_Women <- (W - 58) * TG
  }

  if (has("BMI") && has("glucose")) {
    TG_mgdl  <- TG * 88.57
    Glu_mgdl <- d[[col_map$glucose]] * 18
    BM       <- d[[col_map$BMI]]
    out$TyG_BMI <- lg(TG_mgdl * Glu_mgdl / 2) * BM
  }

  # --- Prepend ID column if detected ------------------------------------------
  if (!is.null(id_col)) {
    id_vec        <- d[[id_col]][seq_len(nrow(out))]
    out[[id_col]] <- id_vec
    out           <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out           <- tibble::as_tibble(out)
  }

  # --- Verbose: results summary -----------------------------------------------
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}

# ---- internal helpers --------------------------------------------------------

.lm_precompute_deps <- function() {
  list(
    BMI = list(
      needs    = c("weight", "height"),
      describe = "weight_kg / height_m^2",
      compute  = function(data) {
        w   <- as.numeric(data[["weight"]])
        h   <- as.numeric(data[["height"]])
        h_m <- ifelse(is.finite(h) & h > 3, h / 100, h)
        w / (h_m ^ 2)
      }
    ),
    glucose = list(
      needs    = "G0",
      describe = "alias: fasting glucose (G0)",
      compute  = function(data) as.numeric(data[["G0"]])
    ),
    G0 = list(
      needs    = "glucose",
      describe = "alias: fasting glucose (glucose)",
      compute  = function(data) as.numeric(data[["glucose"]])
    )
  )
}

.lm_default_extreme_rules <- function() {
  list(
    TC      = c(0, 50),
    HDL_c   = c(0, 10),
    TG      = c(0, 50),
    LDL_c   = c(0, 50),
    ApoB    = c(0, 10),
    ApoA1   = c(0, 10),
    waist   = c(30, 250),
    BMI     = c(10, 80),
    glucose = c(0, 50)
  )
}

