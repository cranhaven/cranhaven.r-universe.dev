#' Compute a Suite of Nutrient-Based Health Markers
#'
#' Given a data frame or tibble of routine biochemical labs,
#' `nutrient_markers()` returns a set of widely used ratios, products,
#' and simple percentages that summarize iron metabolism, protein status,
#' omega-3 balance, renal excretion, mineral homeostasis, and aromatic
#' amino-acid patterns.
#'
#' Recognized markers (returned as columns):
#' - FerritinTS: Ferritin / Transferrin saturation
#' - AGR: Albumin / Globulin, where Globulin = Total protein - Albumin
#' - Omega3Index: EPA + DHA (percentage points)
#' - Mg_Cr_Ratio: Magnesium / Creatinine
#' - GlycatedAlbuminPct: (Glycated albumin / Albumin) x 100
#' - UA_Cr_Ratio: Uric acid / Creatinine
#' - BUN_Cr_Ratio: BUN / Creatinine
#' - Ca_x_Phosphate: Calcium x Phosphate
#' - AnionGap: (Na + K) - (Cl + HCO3)
#' - Tyr_Phe_Ratio: Tyrosine / Phenylalanine
#'
#' @param data A data frame or tibble containing subject-level data.
#' @param col_map Optional named list mapping variable keys (see Details) to
#'   column names in `data`. You only need to supply the keys you have; any
#'   markers with missing inputs return `NA`. If NULL, defaults to identity
#'   mapping for all known keys.
#' @param na_action One of c("keep","omit","error") controlling missing-data policy
#'   across the columns referenced by `col_map`.
#'   - "keep" (default): keep NA; outputs become NA where inputs are NA.
#'   - "omit": drop rows with NA in any used input column.
#'   - "error": abort if any used input contains NA.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for high-missingness
#'   diagnostics on used input columns. Default 0.2.
#' @param verbose Logical; if TRUE, prints stepwise messages and a final summary via hm_inform. Default FALSE.
#'
#' @details
#' Recognized `col_map` keys and expected units (no automatic conversion):
#' - ferritin: Serum ferritin (ng/mL)
#' - transferrin_sat: Transferrin saturation (%)
#' - albumin: Serum albumin (g/L)
#' - total_protein: Total serum protein (g/L)
#' - EPA: Red-cell EPA as % of total fatty acids
#' - DHA: Red-cell DHA as % of total fatty acids
#' - Mg: Serum magnesium (mmol/L)
#' - creatinine: Serum creatinine (umol/L)
#' - glycated_albumin: Glycated albumin (g/L)
#' - uric_acid: Serum uric acid (umol/L)
#' - BUN: Blood urea nitrogen (mg/dL)
#' - phosphate: Serum phosphate (mmol/L)
#' - calcium: Serum calcium (mmol/L)
#' - Na: Serum sodium (mmol/L)
#' - K: Serum potassium (mmol/L)
#' - Cl: Serum chloride (mmol/L)
#' - HCO3: Serum bicarbonate (mmol/L)
#' - Tyr: Serum tyrosine (umol/L)
#' - Phe: Serum phenylalanine (umol/L)
#'
#' **Unit-mixing notes:**
#' - `BUN_Cr_Ratio` divides BUN (mg/dL) by creatinine (umol/L). This is NOT
#'   numerically equivalent to the standard clinical BUN:Creatinine ratio
#'   (reference range ~10-20), which requires both in mg/dL. The result here
#'   is approximately 88.4x smaller than the standard ratio. Provide creatinine
#'   in mg/dL and adjust `col_map` if you require the standard clinical ratio.
#' - `Mg_Cr_Ratio` divides Mg (mmol/L) by creatinine (umol/L). The result is
#'   1/1000 of the standard Mg/Cr ratio in mmol/mmol. Typically applied to urine;
#'   serum Mg/Cr is not a standard clinical metric.
#'
#' Default `extreme_rules` (inputs) are broad and intended for unit/entry checks:
#' ferritin (0, 2000), transferrin_sat (0, 100), albumin (10, 60), total_protein (40, 100),
#' EPA (0, 20), DHA (0, 20), Mg (0.2, 3), creatinine (20, 2000), glycated_albumin (0, 60),
#' uric_acid (50, 1000), BUN (1, 150), phosphate (0.1, 5), calcium (0.5, 4),
#' Na (100, 200), K (2, 8), Cl (70, 130), HCO3 (5, 45), Tyr (10, 300), Phe (20, 300).
#'
#' @return A tibble with one row per input row and these columns:
#' FerritinTS, AGR, Omega3Index, Mg_Cr_Ratio, GlycatedAlbuminPct,
#' UA_Cr_Ratio, BUN_Cr_Ratio, Ca_x_Phosphate, AnionGap, Tyr_Phe_Ratio.
#'
#' @references
#' \insertRef{harris2004omega3}{HealthMarkers}
#' \insertRef{koga2010glyalb}{HealthMarkers}
#' \insertRef{block1998calcp}{HealthMarkers}
#' \insertRef{waikar2009creak}{HealthMarkers} (creatinine kinetics context)
#' @examples
#' # Quick smoke-test
#' df <- data.frame(ferritin = 50, albumin = 45, uric_acid = 300, Na = 140)
#' nutrient_markers(df, verbose = FALSE)
#'
#' \donttest{
#' df <- tibble::tibble(
#'   ferritin         = c(50, 100),
#'   transferrin_sat  = c(30, 50),
#'   albumin          = c(45, 40),
#'   total_protein    = c(70, 75),
#'   EPA              = c(2.0, 2.5),
#'   DHA              = c(4.0, 4.5),
#'   Mg               = c(0.85, 0.90),
#'   creatinine       = c(80, 90),
#'   glycated_albumin = c(12, 14),
#'   uric_acid        = c(300, 400),
#'   BUN              = c(14, 16),
#'   phosphate        = c(1.0, 1.2),
#'   calcium          = c(2.3, 2.4),
#'   Na               = c(140, 138),
#'   K                = c(4.2, 4.0),
#'   Cl               = c(100, 102),
#'   HCO3             = c(24, 26),
#'   Tyr              = c(60, 70),
#'   Phe              = c(50, 55)
#' )
#' nutrient_markers(df, verbose = TRUE)
#' }
#'
#' @importFrom rlang abort warn inform
#' @importFrom tibble tibble
#' @export
nutrient_markers <- function(
  data,
  col_map      = NULL,
  na_action    = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  verbose      = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "nutrient_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)

  # HM-CS v2 validation hook; no strictly required keys for this summarizer
  hm_validate_inputs(data, col_map, required_keys = character(0), fn = fn_name)

  keys <- c(
    "ferritin","transferrin_sat","albumin","total_protein",
    "EPA","DHA","Mg","creatinine","glycated_albumin","uric_acid",
    "BUN","phosphate","calcium","Na","K","Cl","HCO3","Tyr","Phe"
  )

  if (is.list(col_map) && length(col_map) > 0L) {
    extra <- setdiff(names(col_map), keys)
    if (length(extra)) {
      rlang::warn(
        sprintf("nutrient_markers(): ignoring unrecognized keys in col_map: %s", paste(extra, collapse = ", ")),
        class = "healthmarkers_nutrient_warn_unrecognized_keys"
      )
      col_map[extra] <- NULL
    }
  }
  cm      <- .hm_build_col_map(data, col_map, keys = keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  mapped    <- unlist(col_map, use.names = TRUE)
  used_cols <- intersect(unname(mapped), names(data))

  # --- Verbose: column mapping
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: optional inputs
  if (isTRUE(verbose)) {
    avail_keys <- names(col_map)[vapply(names(col_map), function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    absent_keys <- setdiff(keys, avail_keys)
    lines <- sprintf("%s(): optional inputs", fn_name)
    if (length(avail_keys))
      lines <- c(lines, sprintf("  present:  %s", paste(avail_keys, collapse = ", ")))
    if (length(absent_keys))
      lines <- c(lines, sprintf("  missing:  %s", paste(absent_keys, collapse = ", ")))
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  # Coerce used inputs to numeric; NA on non-finite
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("nutrient_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug verbosity)
  .nm_high_missing_diag(data, used_cols, na_warn_prop = na_warn_prop)

  # NA policy over used inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("nutrient_markers(): used input columns contain missing values (na_action='error').",
                   class = "healthmarkers_nm_error_missing_values")
    }
  } else if (na_action == "omit" && length(used_cols)) {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in used inputs", fn_name, sum(!keep)),
                level = "inform")
    data <- data[keep, , drop = FALSE]
  }

  # --- Verbose: physiological range check (informational, values not altered)
  if (isTRUE(verbose)) {
    rules_info <- .nm_default_extreme_rules()
    ex_info    <- .nm_extreme_scan(data, col_map, rules_info, keys)
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
    marker_deps <- list(
      FerritinTS         = c("ferritin", "transferrin_sat"),
      AGR                = c("albumin", "total_protein"),
      Omega3Index        = c("EPA", "DHA"),
      Mg_Cr_Ratio        = c("Mg", "creatinine"),
      GlycatedAlbuminPct = c("glycated_albumin", "albumin"),
      UA_Cr_Ratio        = c("uric_acid", "creatinine"),
      BUN_Cr_Ratio       = c("BUN", "creatinine"),
      Ca_x_Phosphate     = c("calcium", "phosphate"),
      AnionGap           = c("Na", "K", "Cl", "HCO3"),
      Tyr_Phe_Ratio      = c("Tyr", "Phe")
    )
    avail_keys2 <- names(col_map)[vapply(names(col_map), function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    status <- vapply(names(marker_deps), function(m) {
      miss_k <- setdiff(marker_deps[[m]], avail_keys2)
      if (length(miss_k) == 0L)
        sprintf("  %-20s [%s]", m, paste(marker_deps[[m]], collapse = ", "))
      else
        sprintf("  %-20s NA [missing: %s]", m, paste(miss_k, collapse = ", "))
    }, character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  n <- nrow(data)
  getcol <- function(key) {
    nm <- col_map[[key]]
    if (!is.null(nm) && nm %in% names(data)) data[[nm]] else NULL
  }

  denom_zero <- list()

  safe_div <- function(num, den, label) {
    if (is.null(num) || is.null(den)) return(rep(NA_real_, n))
    z <- (!is.na(den)) & (den == 0)
    denom_zero[[label]] <<- sum(z, na.rm = TRUE)
    out <- num / den
    out[!is.finite(out)] <- NA_real_
    out
  }

  ferr <- getcol("ferritin")
  tsat <- getcol("transferrin_sat")
  alb  <- getcol("albumin")
  tprot <- getcol("total_protein")
  epa  <- getcol("EPA")
  dha  <- getcol("DHA")
  mg   <- getcol("Mg")
  cr   <- getcol("creatinine")
  galb <- getcol("glycated_albumin")
  ua   <- getcol("uric_acid")
  bun  <- getcol("BUN")
  phos <- getcol("phosphate")
  ca   <- getcol("calcium")
  na_  <- getcol("Na")
  k_   <- getcol("K")
  cl_  <- getcol("Cl")
  hco3 <- getcol("HCO3")
  tyr  <- getcol("Tyr")
  phe  <- getcol("Phe")

  FerritinTS <- safe_div(ferr, tsat, "FerritinTS")

  AGR <- if (!is.null(alb) && !is.null(tprot)) {
    glob <- tprot - alb
    safe_div(alb, glob, "AGR")
  } else rep(NA_real_, n)

  Omega3Index <- if (!is.null(epa) && !is.null(dha)) epa + dha else rep(NA_real_, n)

  Mg_Cr_Ratio <- safe_div(mg, cr, "Mg_Cr_Ratio")
  GlycatedAlbuminPct <- {
    x <- safe_div(galb, alb, "GlycatedAlbuminPct")
    if (all(is.na(x))) x else x * 100
  }
  UA_Cr_Ratio <- safe_div(ua, cr, "UA_Cr_Ratio")
  BUN_Cr_Ratio <- safe_div(bun, cr, "BUN_Cr_Ratio")

  Ca_x_Phosphate <- if (!is.null(ca) && !is.null(phos)) ca * phos else rep(NA_real_, n)

  AnionGap <- if (!is.null(na_) && !is.null(k_) && !is.null(cl_) && !is.null(hco3)) {
    (na_ + k_) - (cl_ + hco3)
  } else rep(NA_real_, n)

  Tyr_Phe_Ratio <- safe_div(tyr, phe, "Tyr_Phe_Ratio")

  out <- tibble::tibble(
    FerritinTS = FerritinTS,
    AGR = AGR,
    Omega3Index = Omega3Index,
    Mg_Cr_Ratio = Mg_Cr_Ratio,
    GlycatedAlbuminPct = GlycatedAlbuminPct,
    UA_Cr_Ratio = UA_Cr_Ratio,
    BUN_Cr_Ratio = BUN_Cr_Ratio,
    Ca_x_Phosphate = Ca_x_Phosphate,
    AnionGap = AnionGap,
    Tyr_Phe_Ratio = Tyr_Phe_Ratio
  )

  dz_total <- sum(unlist(denom_zero), na.rm = TRUE)
  if (dz_total > 0L) {
    nz <- unlist(denom_zero) > 0
    which_str <- paste(sprintf("%s=%d", names(denom_zero)[nz], unlist(denom_zero)[nz]), collapse = ", ")
    rlang::warn(sprintf("nutrient_markers(): zero denominators detected in %d cases (%s).", dz_total, which_str))
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

# ---- internal helpers ---------------------------------------------------------

.nm_high_missing_diag <- function(df, cols, na_warn_prop = 0.2) {
  if (!length(cols)) return(invisible(TRUE))
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("nutrient_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.nm_default_extreme_rules <- function() {
  list(
    ferritin = c(0, 2000),
    transferrin_sat = c(0, 100),
    albumin = c(10, 60),
    total_protein = c(40, 100),
    EPA = c(0, 20),
    DHA = c(0, 20),
    Mg = c(0.2, 3),
    creatinine = c(20, 2000),
    glycated_albumin = c(0, 60),
    uric_acid = c(50, 1000),
    BUN = c(1, 150),
    phosphate = c(0.1, 5),
    calcium = c(0.5, 4),
    Na = c(100, 200),
    K = c(2, 8),
    Cl = c(70, 130),
    HCO3 = c(5, 45),
    Tyr = c(10, 300),
    Phe = c(20, 300)
  )
}

.nm_extreme_scan <- function(df, col_map, rules, keys) {
  count <- 0L
  flags <- list()
  for (key in intersect(names(col_map), names(rules))) {
    cn <- col_map[[key]]
    if (is.null(cn) || !cn %in% names(df)) next
    x <- df[[cn]]
    rng <- rules[[key]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

