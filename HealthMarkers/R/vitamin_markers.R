
#' Compute composite vitamin and endocrine marker ratios and z-scores
#'
#' Given serum/plasma vitamins and related analytes, `vitamin_markers()` computes:
#' - VitD_Z: z-score of 25(OH)D using provided reference mean/sd
#' - B12_Fol_Ratio: vitamin B12 / folate
#' - Ferr_TSat_R: ferritin / transferrin saturation (TSat)
#' - Cort_DHEA_R: cortisol / DHEA-S
#' - T_E2_Ratio: testosterone / estradiol
#' - TSH_fT4_R: TSH / free T4
#' - Retinol_Z: z-score of retinol using provided reference mean/sd
#' - Toco_Lip_R: alpha-tocopherol / total lipids
#' - Mg_Zn_R: magnesium / zinc
#' - Cu_Zn_R: copper / zinc
#' Plus pass-through: PIVKA_II, VitC, Homocysteine, MMA
#'
#' HM-CS v2:
#' - Validation via `hm_validate_inputs(data, col_map, required_keys, fn)`
#' - User errors via `rlang::abort(..., class=...)`
#' - Verbosity via `hm_inform(level)` controlled by `options(healthmarkers.verbose)`
#' - High-missingness diagnostics at debug level only
#'
#' @param data A data.frame or tibble with vitamin/analyte columns.
#' @param col_map Named list mapping required keys to column names:
#'   VitD, VitD_ref_mean, VitD_ref_sd, B12, Folate, Ferritin, TSat,
#'   Cortisol, DHEAS, Testosterone, Estradiol, TSH, free_T4,
#'   Retinol, Retinol_ref_mean, Retinol_ref_sd, Tocopherol, Total_lipids,
#'   PIVKA_II, VitC, Homocysteine, MMA, Magnesium, Zinc, Copper.
#' @param na_action One of c("keep","omit","error") for required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness debug notices. Default 0.2.
#' @param verbose Logical; if `TRUE` (default), prints column mapping, input
#'   availability, physiological range information (informational only, values
#'   not altered), the list of markers being computed with their inputs, and a
#'   per-column results summary.
#'
#' @return A tibble with columns:
#'   VitD_Z, B12_Fol_Ratio, Ferr_TSat_R, Cort_DHEA_R, T_E2_Ratio, TSH_fT4_R,
#'   Retinol_Z, Toco_Lip_R, PIVKA_II, VitC, Homocysteine, MMA, Mg_Zn_R, Cu_Zn_R.
#'   If an ID column is detected in `data` (e.g. `id`, `IID`, `participant_id`),
#'   it is prepended as the first output column.
#'
#' @examples
#' \donttest{
#' # All 25 required columns must be supplied
#' df <- data.frame(
#'   VitD = 50, VitD_ref_mean = 40, VitD_ref_sd = 5,
#'   B12 = 300, Folate = 15, Ferritin = 80, TSat = 0.25,
#'   Cortisol = 200, DHEAS = 100, Testosterone = 12, Estradiol = 120,
#'   TSH = 2, free_T4 = 14, Retinol = 0.8, Retinol_ref_mean = 0.9,
#'   Retinol_ref_sd = 0.2, Tocopherol = 30, Total_lipids = 3,
#'   PIVKA_II = 5, VitC = 60, Homocysteine = 10, MMA = 0.3,
#'   Magnesium = 0.8, Zinc = 15, Copper = 15
#' )
#' vitamin_markers(df, verbose = FALSE)
#' }
#'
#' @note
#' `VitD_Z` and `Retinol_Z` are z-scores using **user-supplied** reference
#' mean and SD; no population reference equations are applied. All ratio markers
#' (`B12_Fol_Ratio`, `Ferr_TSat_R`, `Cort_DHEA_R`, `T_E2_Ratio`, `TSH_fT4_R`,
#' `Toco_Lip_R`, `Mg_Zn_R`, `Cu_Zn_R`) are simple numerator/denominator
#' divisions. `PIVKA_II`, `VitC`, `Homocysteine`, and `MMA` are
#' **pass-through** columns; no formula is applied.
#'
#' @references
#' \insertRef{holick2007vitddeficiency}{HealthMarkers} (vitamin D deficiency review; background)
#' \insertRef{oleary2010vitb12}{HealthMarkers} (vitamin B12 in health and disease; background)
#' \insertRef{ganz2015iron}{HealthMarkers} (iron homeostasis and ferritin; background)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
vitamin_markers <- function(data,
                            col_map      = NULL,
                            na_action    = c("keep", "omit", "error"),
                            na_warn_prop = 0.2,
                            verbose      = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "vitamin_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    rlang::abort("vitamin_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_vitamin_error_data_type")
  }
  if (!is.null(col_map) && !is.list(col_map)) {
    rlang::abort("vitamin_markers(): `col_map` must be a named list.",
                 class = "healthmarkers_vitamin_error_colmap_type")
  }

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)
  required_keys <- c(
    "VitD","VitD_ref_mean","VitD_ref_sd","B12","Folate","Ferritin","TSat",
    "Cortisol","DHEAS","Testosterone","Estradiol","TSH","free_T4",
    "Retinol","Retinol_ref_mean","Retinol_ref_sd","Tocopherol","Total_lipids",
    "PIVKA_II","VitC","Homocysteine","MMA","Magnesium","Zinc","Copper"
  )

  cm      <- .hm_build_col_map(data, col_map, required_keys, fn = "vitamin_markers")
  data    <- cm$data
  col_map <- cm$col_map

  # HM-CS v2: standardized validation
  hm_validate_inputs(data, col_map, required_keys = required_keys, fn = fn_name)

  # --- Verbose: col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: optional inputs
  if (isTRUE(verbose)) {
    avail_keys  <- required_keys[vapply(required_keys, function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    absent_keys <- setdiff(required_keys, avail_keys)
    lines <- sprintf("%s(): optional inputs", fn_name)
    if (length(avail_keys))
      lines <- c(lines, sprintf("  present:  %s", paste(avail_keys, collapse = ", ")))
    if (length(absent_keys))
      lines <- c(lines, sprintf("  missing:  %s", paste(absent_keys, collapse = ", ")))
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  # Ensure mapped columns exist
  req_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("vitamin_markers(): missing required columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_vitamin_error_missing_columns"
    )
  }

  # Coerce mapped numerics; warn if NAs introduced; non-finite -> NA
  for (key in required_keys) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("vitamin_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug)
  for (key in required_keys) {
    cn <- col_map[[key]]
    x <- data[[cn]]
    n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("vitamin_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
            msg   = hm_fmt_col_map(col_map[required_keys], "vitamin_markers"))

  # NA policy
  used_cols <- req_cols
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("vitamin_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_vitamin_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep)),
                level = "inform")
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      VitD_Z = numeric(),
      B12_Fol_Ratio = numeric(),
      Ferr_TSat_R = numeric(),
      Cort_DHEA_R = numeric(),
      T_E2_Ratio = numeric(),
      TSH_fT4_R = numeric(),
      Retinol_Z = numeric(),
      Toco_Lip_R = numeric(),
      PIVKA_II = numeric(),
      VitC = numeric(),
      Homocysteine = numeric(),
      MMA = numeric(),
      Mg_Zn_R = numeric(),
      Cu_Zn_R = numeric()
    ))
  }

  # Optional extremes scan (informational range check via verbose)
  if (isTRUE(verbose)) {
    default_rules_info <- list(
      VitD = c(0, 250), VitD_ref_mean = c(-Inf, Inf), VitD_ref_sd = c(0.01, Inf),
      B12 = c(0, 2000), Folate = c(0, 100),
      Ferritin = c(0, 3000), TSat = c(0, 1),
      Cortisol = c(0, 2000), DHEAS = c(0, 2000),
      Testosterone = c(0, 200), Estradiol = c(0, 5000),
      TSH = c(0, 200), free_T4 = c(0, 100),
      Retinol = c(0, 10), Retinol_ref_mean = c(-Inf, Inf), Retinol_ref_sd = c(0.001, Inf),
      Tocopherol = c(0, 200), Total_lipids = c(0.001, 100),
      PIVKA_II = c(0, 10000), VitC = c(0, 1000), Homocysteine = c(0, 200), MMA = c(0, 20),
      Magnesium = c(0, 10), Zinc = c(0, 1000), Copper = c(0, 1000)
    )
    flagged_details <- character(0)
    for (key in required_keys) {
      rng <- default_rules_info[[key]]
      if (is.null(rng) || !is.finite(rng[1]) || !is.finite(rng[2])) next
      cn <- col_map[[key]]
      if (is.null(cn) || !(cn %in% names(data))) next
      x <- data[[cn]]
      bad <- sum(is.finite(x) & (x < rng[1] | x > rng[2]), na.rm = TRUE)
      if (bad > 0L)
        flagged_details <- c(flagged_details,
          sprintf("  %s: %d value(s) outside plausible range", key, bad))
    }
    if (length(flagged_details))
      hm_inform(
        paste(c(sprintf("%s(): range note (informational, values not altered):", fn_name),
                flagged_details), collapse = "\n"),
        level = "inform"
      )
  }

  # --- Verbose: computing markers
  if (isTRUE(verbose)) {
    marker_deps <- list(
      VitD_Z        = c("VitD", "VitD_ref_mean", "VitD_ref_sd"),
      B12_Fol_Ratio = c("B12", "Folate"),
      Ferr_TSat_R   = c("Ferritin", "TSat"),
      Cort_DHEA_R   = c("Cortisol", "DHEAS"),
      T_E2_Ratio    = c("Testosterone", "Estradiol"),
      TSH_fT4_R     = c("TSH", "free_T4"),
      Retinol_Z     = c("Retinol", "Retinol_ref_mean", "Retinol_ref_sd"),
      Toco_Lip_R    = c("Tocopherol", "Total_lipids"),
      PIVKA_II      = c("PIVKA_II"),
      VitC          = c("VitC"),
      Homocysteine  = c("Homocysteine"),
      MMA           = c("MMA"),
      Mg_Zn_R       = c("Magnesium", "Zinc"),
      Cu_Zn_R       = c("Copper", "Zinc")
    )
    avail_keys2 <- required_keys[vapply(required_keys, function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    status <- vapply(names(marker_deps), function(m) {
      miss_k <- setdiff(marker_deps[[m]], avail_keys2)
      if (length(miss_k) == 0L)
        sprintf("  %-16s [%s]", m, paste(marker_deps[[m]], collapse = ", "))
      else
        sprintf("  %-16s NA [missing: %s]", m, paste(miss_k, collapse = ", "))
    }, character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # Safe division with consolidated zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  g <- function(key) data[[col_map[[key]]]]

  VitD_Z        <- (g("VitD")     - g("VitD_ref_mean"))    / g("VitD_ref_sd")
  B12_Fol_Ratio <-  safe_div(g("B12"), g("Folate"), "B12_Folate_den")
  Ferr_TSat_R   <-  safe_div(g("Ferritin"), g("TSat"), "Ferr_TSat_den")
  Cort_DHEA_R   <-  safe_div(g("Cortisol"), g("DHEAS"), "Cort_DHEAS_den")
  T_E2_Ratio    <-  safe_div(g("Testosterone"), g("Estradiol"), "T_E2_den")
  TSH_fT4_R     <-  safe_div(g("TSH"), g("free_T4"), "TSH_fT4_den")
  Retinol_Z     <- (g("Retinol") - g("Retinol_ref_mean")) / g("Retinol_ref_sd")
  Toco_Lip_R    <-  safe_div(g("Tocopherol"), g("Total_lipids"), "Toco_Lip_den")
  Mg_Zn_R       <-  safe_div(g("Magnesium"), g("Zinc"), "Mg_Zn_den")
  Cu_Zn_R       <-  safe_div(g("Copper"), g("Zinc"), "Cu_Zn_den")

  out <- tibble::tibble(
    VitD_Z        = as.numeric(VitD_Z),
    B12_Fol_Ratio = as.numeric(B12_Fol_Ratio),
    Ferr_TSat_R   = as.numeric(Ferr_TSat_R),
    Cort_DHEA_R   = as.numeric(Cort_DHEA_R),
    T_E2_Ratio    = as.numeric(T_E2_Ratio),
    TSH_fT4_R     = as.numeric(TSH_fT4_R),
    Retinol_Z     = as.numeric(Retinol_Z),
    Toco_Lip_R    = as.numeric(Toco_Lip_R),
    PIVKA_II      = as.numeric(g("PIVKA_II")),
    VitC          = as.numeric(g("VitC")),
    Homocysteine  = as.numeric(g("Homocysteine")),
    MMA           = as.numeric(g("MMA")),
    Mg_Zn_R       = as.numeric(Mg_Zn_R),
    Cu_Zn_R       = as.numeric(Cu_Zn_R)
  )

  # Consolidated zero-denominator warning
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("vitamin_markers(): zero denominators detected in %d cases (%s).", dz_total, lbl))
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

