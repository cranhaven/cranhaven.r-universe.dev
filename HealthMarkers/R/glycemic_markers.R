
#' Calculate glycemic-, C-peptide-, and additional metabolic markers
#'
#' Given fasting labs and anthropometry, computes:
#'  - SPISE (Single-Point Insulin Sensitivity Estimator)
#'  - METS_IR (Metabolic Score for Insulin Resistance)
#'  - prediabetes flag (HbA1c >= 42 mmol/mol)
#'  - diabetes flag (HbA1c >= 48 mmol/mol)
#'  - HOMA_CP (C-peptide-based HOMA-IR variant; operational formula, see notes)
#'  - LAR (Leptin/Adiponectin Ratio)
#'  - ASI (Adiponectin Sensitivity Index; adiponectin/insulin)
#'  - TyG_index (Triglyceride-Glucose Index)
#'
#' Assumed units (no automatic conversion of inputs except where noted):
#' - HDL_c, TG: mmol/L (TyG internally converts TG to mg/dL via 88.57)
#' - BMI: kg/m^2
#' - glucose, G0: mmol/L (TyG internally converts glucose to mg/dL via 18)
#' - HbA1c: mmol/mol
#' - C_peptide, I0: pmol/L (HOMA_CP uses I-like conversion factor 6 as in insulin muU/mL; see notes)
#' - leptin, adiponectin: ng/mL
#'
#' These indices are intended for research and feature-engineering applications.
#' The prediabetes and diabetes flags apply standard HbA1c cut-offs (WHO/IDF criteria)
#' but this function is not validated as a clinical diagnostic tool.
#'
#' Quality controls and options:
#' - Input validation ensures required variables exist and are numeric-coercible.
#' - Non-numeric inputs are coerced to numeric with a warning (NAs introduced reported).
#' - Missing or non-finite inputs are handled via `na_action`.
#' - Logs and divisions are computed safely (non-positive arguments yield NA).
#' - Physiological range notes are printed when `verbose = TRUE` (values are not altered).
#' - Verbose mode prints step-by-step progress and a completion summary.
#'
#' @param data A data.frame or tibble containing at least:
#'   - HDL_c (mmol/L), TG (mmol/L), BMI (kg/m^2)
#'   Optional if present: glucose (mmol/L), HbA1c (mmol/mol), C_peptide (pmol/L),
#'   G0 (mmol/L), I0 (pmol/L), leptin (ng/mL), adiponectin (ng/mL).
#' @param col_map Optional named list mapping keys to column names in `data`.
#'   Required keys: `HDL_c`, `TG`, `BMI`. Optional keys (if present will be used):
#'   `glucose`, `HbA1c`, `C_peptide`, `G0`, `I0`, `leptin`, `adiponectin`.
#'   If `NULL` (default), the function uses columns named exactly as the keys.
#' @param na_action One of `c("ignore","warn","error","keep","omit")`.
#'   HM-CS aliases: `keep` == `ignore`; `omit` drops rows with any NA/non-finite
#'   in used inputs before computing markers. Default "ignore".
#' @param na_warn_prop Proportion (0-1) threshold for high-missingness warnings
#'   among used columns when `na_action = "warn"`. Default 0.2.
#' @param verbose Logical; if `TRUE` (default), prints step-by-step progress
#'   including column mapping, optional input availability, pre-computation
#'   notes, physiological range information (informational only, values are not
#'   altered), the list of markers being computed with their inputs, and a
#'   per-column results summary.
#'
#' @return A tibble with columns:
#'   - SPISE, METS_IR, prediabetes, diabetes, HOMA_CP, LAR, ASI, TyG_index
#'   If an ID column is detected in `data` (e.g. `id`, `IID`, `participant_id`),
#'   it is prepended as the first output column.
#'
#' @details
#' Optional marker detection and pre-computation (one level deep):
#' When `col_map = NULL` (default), column names are inferred automatically.
#' The seven optional keys (glucose, HbA1c, C_peptide, G0, I0, leptin,
#' adiponectin) are computed only when present in `data`. If `BMI` is absent
#' but `weight` (kg) and `height` (m or cm) are present, BMI is computed
#' automatically. If `glucose` is absent but `G0` column exists (or vice
#' versa), the missing key is derived via alias. With `verbose = TRUE`
#' (default), the function reports: column mapping, what is missing and which
#' raw inputs could provide it, which indices will be NA and why, any
#' physiological range notes, and a per-column results summary.
#'
#' Notes on HOMA_CP:
#' - This function retains the package's existing operational formula:
#'   HOMA_CP = (G0 (mmol/L) * (C_peptide (pmol/L) / 6)) / 22.5
#'   which mirrors HOMA-IR's structure using a 6 pmol/muU scaling used for insulin.
#'   Users should verify unit conventions for their datasets; alternative C-peptide
#'   HOMA implementations exist (e.g., HOMA2-CP).
#' @references
#' \insertRef{paulmichl2016spise}{HealthMarkers}
#' \insertRef{bellogaytan2018metsir}{HealthMarkers}
#' \insertRef{fruhbeck2018adiponectinleptin}{HealthMarkers}
#' \insertRef{matthews1985homa}{HealthMarkers}
#' \insertRef{simentalmendia2008tyg}{HealthMarkers}
#' \insertRef{yang2006adiponectininsulin}{HealthMarkers}
#' 
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' # Quick smoke-test
#' df <- data.frame(glucose = 5.6, HbA1c = 44, G0 = 5.5, I0 = 60,
#'                  HDL_c = 1.2, TG = 1.5, BMI = 24)
#' glycemic_markers(df, verbose = FALSE)
#'
#' \donttest{
#' df <- tibble::tibble(
#'   HDL_c       = c(1.0, 1.3),
#'   TG          = c(1.3, 2.0),
#'   BMI         = c(24, 30),
#'   glucose     = c(5.6, 7.1),
#'   HbA1c       = c(44, 38),
#'   C_peptide   = c(300, 500),
#'   G0          = c(5.5, 6.2),
#'   I0          = c(60, 120),
#'   leptin      = c(10, 20),
#'   adiponectin = c(8, 5)
#' )
#' glycemic_markers(df)
#' glycemic_markers(df, verbose = FALSE)
#' glycemic_markers(df, na_action = "omit", verbose = FALSE)
#' }
glycemic_markers <- function(
  data,
  col_map      = NULL,
  na_action    = c("ignore", "warn", "error", "keep", "omit"),
  na_warn_prop = 0.2,
  verbose      = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "glycemic_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    stop(sprintf("%s(): `data` must be a data.frame or tibble.", fn_name), call. = FALSE)
  }

  # --- Detect and preserve ID column -----------------------------------------
  id_col <- .hm_detect_id_col(data)

  # --- Build col_map: infer missing keys, materialize aliases ----------------
  all_gm_keys <- c("HDL_c", "TG", "BMI", "HbA1c", "C_peptide", "G0", "I0",
                   "leptin", "adiponectin", "glucose")
  req_keys <- c("HDL_c", "TG", "BMI")
  opt_keys <- c("glucose", "HbA1c", "C_peptide", "G0", "I0", "leptin", "adiponectin")

  cm      <- .hm_build_col_map(data, col_map, all_gm_keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # --- Verbose: column mapping (user-provided vs inferred) -------------------
  .hm_log_cols(cm, col_map, fn_name, verbose, keys = all_gm_keys)

  # --- Pre-computation: fill still-missing required keys from raw inputs -----
  is_missing_key <- function(k) is.null(col_map[[k]]) || !(col_map[[k]] %in% names(data))
  missing_req <- req_keys[vapply(req_keys, is_missing_key, logical(1))]
  if (length(missing_req) > 0L) {
    pc   <- .hm_precompute_from_deps(data, .gm_precompute_deps(),
                                     missing_req, fn = fn_name, verbose = verbose)
    data <- pc$data
    for (k in missing_req) {
      if (is.null(col_map[[k]]) && k %in% names(data)) col_map[[k]] <- k
    }
    missing_req <- req_keys[vapply(req_keys, is_missing_key, logical(1))]
  }
  if (length(missing_req)) {
    stop(sprintf("%s(): missing required columns: %s",
                 fn_name, paste(missing_req, collapse = ", ")), call. = FALSE)
  }

  req_cols <- unname(unlist(col_map[req_keys]))
  miss     <- setdiff(req_cols, names(data))
  if (length(miss)) {
    stop(sprintf("%s(): missing required columns: %s",
                 fn_name, paste(miss, collapse = ", ")), call. = FALSE)
  }

  # --- Determine which optional keys are present ------------------------------
  present_opt   <- vapply(opt_keys, function(k) {
    nm <- col_map[[k]]
    !is.null(nm) && nm %in% names(data)
  }, logical(1))
  used_opt_keys <- opt_keys[present_opt]

  # --- Pre-computation: fill derivable optional keys (one-level) -------------
  missing_opt <- setdiff(opt_keys, used_opt_keys)
  if (length(missing_opt) > 0L) {
    pc          <- .hm_precompute_from_deps(data, .gm_precompute_deps(),
                                            missing_opt, fn = fn_name, verbose = verbose)
    newly_added <- setdiff(names(pc$data), names(data))
    if (length(newly_added) > 0L) {
      data <- pc$data
      for (k in newly_added) {
        if (is.null(col_map[[k]]) && k %in% names(data)) col_map[[k]] <- k
      }
      present_opt <- vapply(opt_keys, function(k) {
        nm <- col_map[[k]]
        !is.null(nm) && nm %in% names(data)
      }, logical(1))
      used_opt_keys <- opt_keys[present_opt]
      missing_opt   <- setdiff(opt_keys, used_opt_keys)
    }
  }

  used_keys <- c(req_keys, used_opt_keys)
  used_cols <- unname(unlist(col_map[used_keys]))

  # --- Verbose: optional inputs + what each missing key affects ---------------
  if (isTRUE(verbose)) {
    idx_deps <- list(
      METS_IR     = c("glucose"),
      prediabetes = c("HbA1c"),
      diabetes    = c("HbA1c"),
      HOMA_CP     = c("C_peptide", "G0"),
      LAR         = c("leptin", "adiponectin"),
      ASI         = c("adiponectin", "I0"),
      TyG_index   = c("glucose")
    )
    na_indices <- character(0)
    for (idx in names(idx_deps)) {
      still_missing <- setdiff(idx_deps[[idx]], used_opt_keys)
      if (length(still_missing)) {
        na_indices <- c(na_indices,
          sprintf("  %s -> NA  [missing: %s]", idx, paste(still_missing, collapse = ", ")))
      }
    }
    deps_all        <- .gm_precompute_deps()
    derivable_hints <- character(0)
    for (k in missing_opt) {
      dep <- deps_all[[k]]
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
    lines <- sprintf("%s(): optional inputs", fn_name)
    if (length(used_opt_keys))
      lines <- c(lines, sprintf("  present:  %s", paste(used_opt_keys, collapse = ", ")))
    if (length(missing_opt))
      lines <- c(lines, sprintf("  missing:  %s", paste(missing_opt, collapse = ", ")))
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
      if (introduced_na > 0) {
        warning(sprintf("Column '%s' coerced to numeric; NAs introduced: %d",
                        cn, introduced_na), call. = FALSE)
      }
    }
  }

  # --- Extract vectors --------------------------------------------------------
  getv <- function(key) if (key %in% used_keys) data[[col_map[[key]]]] else NULL

  HDL_c       <- data[[col_map$HDL_c]]
  TG          <- data[[col_map$TG]]
  BMI         <- data[[col_map$BMI]]
  glucose     <- getv("glucose")
  HbA1c       <- getv("HbA1c")
  C_peptide   <- getv("C_peptide")
  G0          <- getv("G0")
  I0          <- getv("I0")
  leptin      <- getv("leptin")
  adiponectin <- getv("adiponectin")

  # --- NA handling ------------------------------------------------------------
  used_df     <- data[, used_cols, drop = FALSE]
  nonfin_mask <- vapply(used_cols, function(cn) any(!is.finite(data[[cn]])), logical(1))
  if (na_action == "warn" && any(nonfin_mask)) {
    warning(sprintf("Non-finite values detected in: %s; treated as NA.",
                    paste(used_cols[nonfin_mask], collapse = ", ")), call. = FALSE)
  }
  if (na_action == "error" && any(!stats::complete.cases(used_df))) {
    stop(sprintf("%s(): missing or non-finite values in required inputs with na_action='error'.",
                 fn_name), call. = FALSE)
  } else if (na_action == "omit") {
    keep  <- stats::complete.cases(used_df)
    data  <- data[keep, , drop = FALSE]
    HDL_c <- HDL_c[keep]; TG <- TG[keep]; BMI <- BMI[keep]
    if (!is.null(glucose))     glucose     <- glucose[keep]
    if (!is.null(HbA1c))       HbA1c       <- HbA1c[keep]
    if (!is.null(C_peptide))   C_peptide   <- C_peptide[keep]
    if (!is.null(G0))          G0          <- G0[keep]
    if (!is.null(I0))          I0          <- I0[keep]
    if (!is.null(leptin))      leptin      <- leptin[keep]
    if (!is.null(adiponectin)) adiponectin <- adiponectin[keep]
  }

  # --- Verbose: physiological range check (informational, values not altered) --
  if (isTRUE(verbose)) {
    rules         <- .gm_default_extreme_rules()
    remapped_rules <- list()
    for (nm in names(rules)) {
      cn <- if (!is.null(col_map[[nm]])) col_map[[nm]] else nm
      remapped_rules[[cn]] <- rules[[nm]]
    }
    scan_vars <- intersect(used_cols, names(remapped_rules))
    ex        <- .gm_extreme_scan(data, scan_vars, remapped_rules)
    if (ex$count > 0L) {
      details <- vapply(names(ex$flags), function(v) {
        nb <- sum(ex$flags[[v]])
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

  # --- Verbose: which markers will be computed and from what ------------------
  if (isTRUE(verbose)) {
    status <- c(
      "  SPISE           [HDL_c, TG, BMI]",
      sprintf("  METS_IR         %s",
              if (!is.null(glucose)) "[glucose, TG, BMI, HDL_c]" else "NA [glucose missing]"),
      sprintf("  prediabetes     %s",
              if (!is.null(HbA1c))  "[HbA1c]"                    else "NA [HbA1c missing]"),
      sprintf("  diabetes        %s",
              if (!is.null(HbA1c))  "[HbA1c]"                    else "NA [HbA1c missing]"),
      sprintf("  HOMA_CP         %s",
              if (!is.null(C_peptide) && !is.null(G0)) "[G0, C_peptide]"
              else "NA [C_peptide/G0 missing]"),
      sprintf("  LAR             %s",
              if (!is.null(leptin) && !is.null(adiponectin)) "[leptin, adiponectin]"
              else "NA [leptin/adiponectin missing]"),
      sprintf("  ASI             %s",
              if (!is.null(adiponectin) && !is.null(I0)) "[adiponectin, I0]"
              else "NA [adiponectin/I0 missing]"),
      sprintf("  TyG_index       %s",
              if (!is.null(glucose)) "[TG, glucose]" else "NA [glucose missing]")
    )
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # --- Compute markers --------------------------------------------------------
  lg <- function(x) .gm_log(x)
  dv <- function(a, b) .gm_safe_div(a, b)

  TG_mgdl <- TG * 88.57

  SPISE <- dv(600 * (HDL_c ^ 0.185), ((TG ^ 0.2) * (BMI ^ 1.338)))

  METS_IR <- if (!is.null(glucose)) {
    dv(lg(2 * glucose + TG) * BMI, lg(HDL_c))
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  prediabetes <- if (!is.null(HbA1c)) as.integer(HbA1c >= 42) else rep(NA_integer_, NROW(HDL_c))
  diabetes    <- if (!is.null(HbA1c)) as.integer(HbA1c >= 48) else rep(NA_integer_, NROW(HDL_c))

  HOMA_CP <- if (!is.null(C_peptide) && !is.null(G0)) {
    dv(G0 * (C_peptide / 6), 22.5)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  LAR <- if (!is.null(leptin) && !is.null(adiponectin)) {
    dv(leptin, adiponectin)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  ASI <- if (!is.null(adiponectin) && !is.null(I0)) {
    dv(adiponectin, I0)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  TyG_index <- if (!is.null(glucose)) {
    lg((TG_mgdl * (glucose * 18)) / 2)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  out <- tibble::tibble(
    SPISE       = SPISE,
    METS_IR     = METS_IR,
    prediabetes = prediabetes,
    diabetes    = diabetes,
    HOMA_CP     = HOMA_CP,
    LAR         = LAR,
    ASI         = ASI,
    TyG_index   = TyG_index
  )

  # --- Prepend ID column if detected ------------------------------------------
  if (!is.null(id_col)) {
    id_vec         <- data[[id_col]][seq_len(nrow(out))]
    out[[id_col]]  <- id_vec
    out            <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out            <- tibble::as_tibble(out)
  }

  # --- Verbose: results summary -----------------------------------------------
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}

# ---- internal helpers (not exported) ----------------------------------------

# Dependency map for one-level pre-computation inside glycemic_markers().
# Each entry: list(needs, describe, compute). `needs` are literal column names
# that must exist in data; `compute(data)` returns a numeric vector.
.gm_precompute_deps <- function() {
  list(
    BMI = list(
      needs    = c("weight", "height"),
      describe = "weight_kg / height_m^2",
      compute  = function(data) {
        w   <- as.numeric(data[["weight"]])
        h   <- as.numeric(data[["height"]])
        h_m <- ifelse(is.finite(h) & h > 3, h / 100, h)  # cm -> m if needed
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

.gm_default_extreme_rules <- function() {
  list(
    HDL_c = c(0.1, 5),
    TG = c(0.1, 20),
    BMI = c(10, 80),
    glucose = c(2, 30),
    HbA1c = c(20, 200),
    C_peptide = c(0, 5000),
    G0 = c(2, 30),
    I0 = c(0, 3000),
    leptin = c(0, 200),
    adiponectin = c(0, 300)
  )
}

.gm_extreme_scan <- function(df, vars, rules) {
  flags <- lapply(vars, function(v) {
    if (is.null(rules[[v]])) return(rep(FALSE, NROW(df)))
    rng <- rules[[v]]
    x <- df[[v]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    bad
  })
  names(flags) <- vars
  list(count = sum(vapply(flags, sum, integer(1))), flags = flags)
}

.gm_safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

.gm_log <- function(x) {
  y <- x
  y[!(is.finite(y) & y > 0)] <- NA_real_
  out <- suppressWarnings(log(y))
  out[!is.finite(out)] <- NA_real_
  out
}

