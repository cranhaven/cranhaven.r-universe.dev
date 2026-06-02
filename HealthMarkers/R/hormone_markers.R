
#' Compute a suite of hormone ratio markers with QA and verbose summaries
#'
#' Ratios computed:
#' - FAI = (total_testosterone / SHBG) * 100
#' - LH_FSH = LH / FSH
#' - E2_P = estradiol / progesterone
#' - T3_T4 = free_T3 / free_T4
#' - ARR = aldosterone / renin
#' - Ins_Glu = insulin / glucagon
#' - GH_IGF1 = GH / IGF1
#' - PRL_T = prolactin / total_testosterone
#' - CAR_slope = (cortisol_30 - cortisol_0) / 30
#'
#' @param data Data frame or tibble with mapped hormone inputs.
#' @param col_map Named list mapping the required keys to column names:
#'   total_testosterone, SHBG, LH, FSH, estradiol, progesterone, free_T3, free_T4,
#'   aldosterone, renin, insulin, glucagon, GH, IGF1, prolactin, cortisol_0, cortisol_30.
#' @param na_action One of `c("keep","omit","error","warn","ignore")`. `"keep"`/`"ignore"` leave NAs;
#'   `"omit"` drops rows with any NA in used inputs; `"error"` aborts;
#'   `"warn"` also warns about high missingness.
#' @param na_warn_prop Proportion in \eqn{[0,1]} for high-missingness warnings
#'   when `na_action = "warn"`. Default 0.2.
#' @param verbose Logical; if `TRUE` (default), prints column mapping, input
#'   availability, inference notes, physiological range information
#'   (informational only, values not altered), computing markers, and a
#'   per-column results summary.
#' @details
#' Some inputs may be inferred when missing (for example, `free_T3` from
#' `TSH` + `free_T4`, or `GH` from `IGF1`) using internal heuristics. These
#' inferred values are intended for exploratory feature engineering only and
#' must not be treated as clinical substitutes for directly measured assays.
#' Ratios such as FAI, ARR, and CAR_slope have established literature usage;
#' several other outputs are simple arithmetic composites included for
#' feature-engineering convenience and may not have a single canonical
#' derivation paper.
#' @references
#' \insertRef{sowers2009fai}{HealthMarkers}
#' \insertRef{funder2016primaryaldosteronism}{HealthMarkers}
#' \insertRef{clow2004car}{HealthMarkers}
#' @return Tibble with one column per computable ratio. If an ID column is
#'   detected in `data` (e.g. `id`, `IID`, `participant_id`), it is prepended
#'   as the first output column.
#'
#' @examples
#' df <- data.frame(
#'   TT = c(15, 12), SHBG = c(40, 35), LH = c(5, 6), FSH = c(4, 5),
#'   E2 = c(100, 120), Prog = c(0.5, 0.6), fT3 = c(4.5, 4.2),
#'   fT4 = c(15, 14), Aldo = c(200, 180), Renin = c(10, 12),
#'   Ins = c(60, 70), Gluc = c(8, 9), GH = c(1.2, 1.0),
#'   IGF1 = c(180, 160), Prl = c(10, 12), Cort0 = c(400, 380),
#'   Cort30 = c(600, 580)
#' )
#' col_map <- list(
#'   total_testosterone = "TT", SHBG = "SHBG", LH = "LH", FSH = "FSH",
#'   estradiol = "E2", progesterone = "Prog", free_T3 = "fT3",
#'   free_T4 = "fT4", aldosterone = "Aldo", renin = "Renin",
#'   insulin = "Ins", glucagon = "Gluc", GH = "GH", IGF1 = "IGF1",
#'   prolactin = "Prl", cortisol_0 = "Cort0", cortisol_30 = "Cort30"
#' )
#' hormone_markers(df, col_map = col_map)
#' @export
hormone_markers <- function(
  data,
  col_map      = NULL,
  na_action    = c("keep", "omit", "error", "warn", "ignore"),
  na_warn_prop = 0.2,
  verbose      = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "hormone_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)
  if (na_action == "keep") na_action <- "ignore"

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)

  # Per-ratio input dependencies
  ratio_defs <- list(
    FAI       = c("total_testosterone", "SHBG"),
    LH_FSH    = c("LH", "FSH"),
    E2_P      = c("estradiol", "progesterone"),
    E2_T      = c("estradiol", "total_testosterone"),
    T3_T4     = c("free_T3", "free_T4"),
    TSH_fT4   = c("TSH", "free_T4"),
    ARR       = c("aldosterone", "renin"),
    Ins_Glu   = c("insulin", "glucagon"),
    GH_IGF1   = c("GH", "IGF1"),
    PRL_T     = c("prolactin", "total_testosterone"),
    CAR_slope = c("cortisol_0", "cortisol_30")
  )

  # Inference rules: derive a missing key from available mapped keys.
  # Each entry: list(target_key  = name to inject into col_map,
  #                   needs       = mapped keys required,
  #                   compute     = function(data, col_map) -> vector,
  #                   label       = short description for messaging)
  infer_rules <- list(
    list(
      target  = "free_T3",
      needs   = c("TSH", "free_T4"),
      compute = function(d, cm) {
        # Internal heuristic estimate: fT3 ~ fT4 * 0.33 * TSH^(-0.20)
        # Source DOI not yet verified; keep as heuristic estimate.
        fT4 <- d[[cm[["free_T4"]]]]
        tsh <- d[[cm[["TSH"]]]]
        ifelse(is.finite(tsh) & tsh > 0 & is.finite(fT4),
               fT4 * 0.33 * tsh^(-0.20), NA_real_)
      },
      label   = "free_T3 estimated from TSH + free_T4 (heuristic)"
    ),
    list(
      target  = "GH",
      needs   = c("IGF1"),
      compute = function(d, cm) {
        # Rough equivalence: median GH ~ IGF1 / 22.5 (nmol-based scale)
        # Use only as a last resort; flag as estimated
        d[[cm[["IGF1"]]]] / 22.5
      },
      label   = "GH estimated from IGF1 (IGF1 / 22.5, approximate)"
    )
  )

  # Validate inputs (HM-CS v3)
  if (!is.data.frame(data)) {
    rlang::abort("hormone_markers(): `data` must be a data.frame or tibble.", class = "healthmarkers_horm_error_data_type")
  }

  all_hm_keys <- c("total_testosterone","SHBG","LH","FSH","estradiol","progesterone",
    "free_T3","free_T4","TSH","aldosterone","renin",
    "insulin","IGF1","prolactin","cortisol_0","cortisol_30")
  cm_hm   <- .hm_build_col_map(data, col_map, all_hm_keys, fn = fn_name)
  data    <- cm_hm$data
  col_map <- cm_hm$col_map

  if (is.null(col_map) || !is.list(col_map)) {
    rlang::abort("hormone_markers(): `col_map` must be a named list.", class = "healthmarkers_horm_error_colmap_type")
  }

  # --- Inference pass: derive missing keys from available mapped data --------
  inferred_keys <- character(0)
  for (rule in infer_rules) {
    if (rule$target %in% names(col_map)) next          # already explicitly mapped
    if (!all(rule$needs %in% names(col_map))) next      # prerequisites not available
    tmp_col <- paste0(".hm_inferred_", rule$target)
    data[[tmp_col]] <- rule$compute(data, col_map)
    col_map[[rule$target]] <- tmp_col
    inferred_keys <- c(inferred_keys, rule$target)
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg   = sprintf("hormone_markers(): inferred '%s' \u2014 %s", rule$target, rule$label))
  }
  # --------------------------------------------------------------------------

  # Determine which ratios can be computed from the supplied col_map
  avail_ratios <- vapply(ratio_defs, function(keys) all(keys %in% names(col_map)), logical(1))
  skipped_ratios <- names(ratio_defs)[!avail_ratios]
  if (length(skipped_ratios) > 0L && isTRUE(verbose))
    hm_inform(sprintf("%s(): skipping %d ratio(s) with unmapped inputs: %s",
                      fn_name, length(skipped_ratios), paste(skipped_ratios, collapse = ", ")),
              level = "inform")
  if (!any(avail_ratios))
    rlang::abort("hormone_markers(): no computable ratios; supply at least one pair of mapped inputs.",
                 class = "healthmarkers_horm_error_no_ratios")

  used_keys <- unique(unlist(ratio_defs[avail_ratios], use.names = FALSE))

  # Ensure each used mapping is a single non-empty character string
  bad_keys <- vapply(used_keys, function(k) {
    v <- col_map[[k]]
    !is.character(v) || length(v) != 1L || !nzchar(v)
  }, logical(1))
  if (any(bad_keys))
    rlang::abort(paste0("missing required columns: ", paste(used_keys[bad_keys], collapse = ", ")),
                 class = "healthmarkers_horm_error_bad_map_values")

  # Mapped columns must exist in data
  used_cols <- unname(vapply(used_keys, function(k) col_map[[k]], character(1)))
  missing_cols <- setdiff(used_cols, names(data))
  if (length(missing_cols))
    rlang::abort(paste0("missing required columns: ", paste(missing_cols, collapse = ", ")),
                 class = "healthmarkers_horm_error_missing_columns")
  # na_warn_prop sanity
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) && na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("hormone_markers(): `na_warn_prop` must be a single number in [0,1].",
                 class = "healthmarkers_horm_error_na_warn_prop")
  }

  # --- Verbose: col_map
  .hm_log_cols(cm_hm, col_map, fn_name, verbose)

  # --- Verbose: optional inputs (ratio availability)
  if (isTRUE(verbose)) {
    all_needed <- unique(unlist(ratio_defs, use.names = FALSE))
    avail_k    <- names(col_map)[vapply(names(col_map), function(k) {
      !is.null(col_map[[k]]) && col_map[[k]] %in% names(data)
    }, logical(1))]
    absent_k   <- setdiff(all_needed, avail_k)
    lines      <- sprintf("%s(): optional inputs", fn_name)
    if (length(avail_k))
      lines <- c(lines, sprintf("  present:  %s", paste(intersect(all_needed, avail_k), collapse = ", ")))
    if (length(absent_k))
      lines <- c(lines, sprintf("  missing:  %s", paste(absent_k, collapse = ", ")))
    if (length(skipped_ratios))
      lines <- c(lines, sprintf("  ratios skipped (missing inputs): %s",
                                paste(skipped_ratios, collapse = ", ")))
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  # Coerce to numeric; warn if NAs introduced; sanitize non-finite to NA
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced_na <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced_na > 0L) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na),
                    class = "healthmarkers_horm_warn_na_coercion")
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # HM-CS: omit rows with any NA in used inputs
  if (na_action == "omit") {
    keep_rows <- stats::complete.cases(data[, used_cols, drop = FALSE])
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep_rows)),
                level = "inform")
    data <- data[keep_rows, , drop = FALSE]
  }

  # Missingness QA
  qa <- .hor_quality_scan(data, used_cols, na_warn_prop)
  if (na_action == "warn") {
    if (length(qa$any_na)) {
      rlang::warn(sprintf("Missing values in: %s.", paste(qa$any_na, collapse = ", ")),
                  class = "healthmarkers_horm_warn_any_na")
    }
    if (length(qa$high_na)) {
      rlang::warn(sprintf("High missingness (>= %.0f%%): %s.", 100 * na_warn_prop, paste(qa$high_na, collapse = ", ")),
                  class = "healthmarkers_horm_warn_high_na")
    }
  }
  if (na_action == "error" && length(qa$any_na)) {
    rlang::abort("hormone_markers(): missing or non-finite values in required inputs with na_action='error'.",
                 class = "healthmarkers_horm_error_missing_values")
  }

  # --- Verbose: physiological range check (informational, values not altered)
  if (isTRUE(verbose)) {
    rules_info   <- .hor_default_extreme_rules()
    flagged_info <- .hor_extreme_scan(data, col_map, rules_info)
    total_flagged_info <- sum(vapply(flagged_info, function(x) sum(x, na.rm = TRUE), integer(1)))
    if (total_flagged_info > 0L) {
      details <- vapply(names(flagged_info), function(nm) {
        nb <- sum(flagged_info[[nm]], na.rm = TRUE)
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
    status <- vapply(names(ratio_defs), function(idx) {
      keys_for_idx <- ratio_defs[[idx]]
      if (avail_ratios[idx])
        sprintf("  %-12s [%s]", idx, paste(keys_for_idx, collapse = ", "))
      else {
        miss_k <- setdiff(keys_for_idx, names(col_map))
        sprintf("  %-12s NA [missing: %s]", idx, paste(miss_k, collapse = ", "))
      }
    }, character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # Extract only the vectors actually needed
  v <- function(k) data[[col_map[[k]]]]

  sdiv <- function(a, b) { z <- a / b; z[!is.finite(z)] <- NA_real_; z }

  # Compute only the available ratios
  out_list <- list()
  if (avail_ratios["FAI"])       out_list$FAI       <- sdiv(v("total_testosterone"), v("SHBG")) * 100
  if (avail_ratios["LH_FSH"])    out_list$LH_FSH    <- sdiv(v("LH"), v("FSH"))
  if (avail_ratios["E2_P"])      out_list$E2_P      <- sdiv(v("estradiol"), v("progesterone"))
  if (avail_ratios["E2_T"])      out_list$E2_T      <- sdiv(v("estradiol"), v("total_testosterone"))
  if (avail_ratios["T3_T4"])     out_list$T3_T4     <- sdiv(v("free_T3"), v("free_T4"))
  if (avail_ratios["TSH_fT4"])   out_list$TSH_fT4   <- sdiv(v("TSH"), v("free_T4"))
  if (avail_ratios["ARR"])       out_list$ARR       <- sdiv(v("aldosterone"), v("renin"))
  if (avail_ratios["Ins_Glu"])   out_list$Ins_Glu   <- sdiv(v("insulin"), v("glucagon"))
  if (avail_ratios["GH_IGF1"])   out_list$GH_IGF1   <- sdiv(v("GH"), v("IGF1"))
  if (avail_ratios["PRL_T"])     out_list$PRL_T     <- sdiv(v("prolactin"), v("total_testosterone"))
  if (avail_ratios["CAR_slope"]) out_list$CAR_slope <- sdiv(v("cortisol_30") - v("cortisol_0"), 30)

  # Tag inferred columns so downstream callers know
  out <- tibble::as_tibble(out_list)
  if (length(inferred_keys) > 0L) {
    inferred_ratios <- names(ratio_defs)[vapply(ratio_defs, function(keys) any(keys %in% inferred_keys), logical(1))]
    inferred_ratios <- intersect(inferred_ratios, names(out))
    attr(out, "inferred_inputs") <- inferred_keys
    attr(out, "inferred_ratios") <- inferred_ratios
  }

  # --- Prepend ID column if detected
  if (!is.null(id_col)) {
    id_vec        <- data[[id_col]][seq_len(nrow(out))]
    out[[id_col]] <- id_vec
    keep_cols     <- c(id_col, setdiff(names(out), id_col))
    out           <- out[, keep_cols, drop = FALSE]
    out           <- tibble::as_tibble(out)
  }

  # --- Verbose: results summary
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}

# --- internal helpers ---------------------------------------------------------

.hor_quality_scan <- function(df, cols, na_warn_prop = 0.2) {
  any_na <- character(0); high_na <- character(0)
  for (cn in cols) {
    x <- df[[cn]]
    if (any(is.na(x))) any_na <- c(any_na, cn)
    prop <- mean(is.na(x))
    if (is.finite(prop) && prop >= na_warn_prop) high_na <- c(high_na, cn)
  }
  list(any_na = unique(any_na), high_na = unique(high_na))
}

.hor_default_extreme_rules <- function() {
  # Plausible broad ranges for lab units; tuned to tests where needed
  list(
    total_testosterone = c(0, 100),
    SHBG = c(0, 200),
    LH = c(0, 200),
    FSH = c(0, 200),
    estradiol = c(0, 10000),
    progesterone = c(0, 1000),
    TSH = c(0, 100),
    free_T3 = c(0, 20),
    free_T4 = c(0, 50),
    aldosterone = c(0, 1000),
    renin = c(0, 50),
    insulin = c(0, 1000),
    glucagon = c(0, 500),
    GH = c(0, 200),
    IGF1 = c(0, 2000),
    prolactin = c(0, 500),
    cortisol_0 = c(0, 2000),
    cortisol_30 = c(0, 2000)
  )
}

.hor_extreme_scan <- function(df, col_map, rules) {
  flags <- list()
  for (nm in names(rules)) {
    cn <- col_map[[nm]]
    if (is.null(cn) || !(cn %in% names(df))) next
    rng <- rules[[nm]]
    x <- df[[cn]]
    flags[[nm]] <- is.finite(x) & (x < rng[1] | x > rng[2])
  }
  flags
}

