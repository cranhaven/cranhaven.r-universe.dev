# HM-CS v2 utilities
# Note: hm_inform() and hm_get_verbosity() are defined in zzz-options.R (canonical).

# Verbose stage 1 -- input logger ---------------------------------------------
# Emits: "fn(): reading input 'expr' -- N rows x P variables"
# Capture data_name via deparse(substitute(data)) as the very first line of
# the calling function, then call here right after fn_name is assigned.
#' @keywords internal
.hm_log_input <- function(data, data_name, fn_name, verbose) {
  if (!isTRUE(verbose)) return(invisible(NULL))
  if (length(data_name) != 1L || nchar(data_name) > 200L) data_name <- "data"
  dims <- if (is.data.frame(data)) {
    sprintf("%d rows \u00d7 %d variables", nrow(data), ncol(data))
  } else {
    sprintf("class: %s", paste(class(data), collapse = "/"))
  }
  hm_inform(
    sprintf("%s(): reading input '%s' \u2014 %s", fn_name, data_name, dims),
    level = "inform"
  )
  invisible(NULL)
}

# Verbose stage 2 -- column resolution logger ----------------------------------
# Replaces the repeated inline col_map verbose blocks in every function.
# Emits, e.g.:
#   fn(): resolving 5 columns -- 3 specified, 2 inferred from data
#     TC                ->  'TC'
#     HDL_c             ->  'HDL_c'
#     TG                ->  'TG'
#     LDL_c             ->  'LDL_c'    (inferred)
#     ApoB              ->  'ApoB'     (inferred)
#
# @param cm      result of .hm_build_col_map() -- needs $user_keys, $inferred_keys
# @param col_map the resolved named list key -> column name
# @param fn_name function name string
# @param verbose logical
# @param keys    optional character vector to restrict which keys are shown
#' @keywords internal
.hm_log_cols <- function(cm, col_map, fn_name, verbose, keys = NULL) {
  if (!isTRUE(verbose)) return(invisible(NULL))

  u_keys <- if (!is.null(keys)) intersect(cm$user_keys,     keys) else cm$user_keys
  i_keys <- if (!is.null(keys)) intersect(cm$inferred_keys, keys) else cm$inferred_keys

  n_user <- length(u_keys)
  n_inf  <- length(i_keys)
  total  <- n_user + n_inf

  if (total == 0L) {
    hm_inform(sprintf("%s(): no columns resolved", fn_name), level = "inform")
    return(invisible(NULL))
  }

  parts_desc <- character(0)
  if (n_user > 0L) parts_desc <- c(parts_desc, sprintf("%d specified",          n_user))
  if (n_inf  > 0L) parts_desc <- c(parts_desc, sprintf("%d inferred from data", n_inf))

  header <- sprintf("%s(): col_map (%d column%s \u2014 %s)",
                    fn_name, total,
                    if (total == 1L) "" else "s",
                    paste(parts_desc, collapse = ", "))

  fmt_key <- function(k, map, tag) {
    col <- map[[k]]
    if (is.null(col)) col <- "(unresolved)"
    sprintf("  %-18s->  '%s'%s", k, col, tag)
  }

  lines <- c(
    if (n_user > 0L) vapply(u_keys, fmt_key, character(1), map = col_map, tag = ""),
    if (n_inf  > 0L) vapply(i_keys, fmt_key, character(1), map = col_map, tag = "    (inferred)")
  )

  hm_inform(paste(c(header, lines), collapse = "\n"), level = "inform")
  invisible(NULL)
}

# Canonical sex normalizer ----------------------------------------------------
# Maps any sex encoding to a consistent output:
#   "MF"  -> "M" or "F"
#   "12"  -> 1L (male) or 2L (female)   [ogtt_is, metss, kidney_failure_risk]
#   "10"  -> 1L (male) or 0L (female)   [renal_markers, cvd_risk]
#   "01"  -> 0L (male) or 1L (female)   [obesity_indices RFM]
#
# Accepted inputs (case-insensitive):
#   Male   : "m", "male", "1"
#   Female : "f", "female", "2", "0"
#   (Convention: when "0" or "2" are mixed, "1" always = male per package default)
#
# @keywords internal
.hm_normalize_sex <- function(x, to = c("MF", "12", "10", "01"), fn = NULL) {
  to <- match.arg(to)
  s  <- trimws(tolower(as.character(x)))
  mf <- ifelse(s %in% c("m", "male", "1"),          "M",
        ifelse(s %in% c("f", "female", "2", "0"),   "F", NA_character_))
  bad <- sum(is.na(mf) & !is.na(x) & nchar(trimws(as.character(x))) > 0)
  if (bad > 0L && !is.null(fn))
    hm_inform(level = "debug",
              msg   = sprintf("%s(): 'sex' has %d unrecognized value(s); treated as NA.", fn, bad))
  switch(to,
    "MF" = mf,
    "12" = ifelse(mf == "M", 1L, ifelse(mf == "F", 2L, NA_integer_)),
    "10" = ifelse(mf == "M", 1L, ifelse(mf == "F", 0L, NA_integer_)),
    "01" = ifelse(mf == "M", 0L, ifelse(mf == "F", 1L, NA_integer_))
  )
}

#' Normalise na_action aliases to canonical values
#'
#' Converts the backward-compat aliases "ignore" and "warn" to "keep" so that
#' the rest of each function only needs to handle c("keep","omit","error").
#' Returns a list with:
#'   - na_action_eff: the canonical value ("keep", "omit", or "error")
#'   - na_action_raw: the original matched value (for warn-diagnostic checks)
#'
#' @param na_action character(1) already matched via match.arg()
#' @keywords internal
.hm_normalize_na_action <- function(na_action) {
  na_action_eff <- if (na_action %in% c("ignore", "warn")) "keep" else na_action
  list(na_action_raw = na_action, na_action_eff = na_action_eff)
}

#' HM-CS v2 input validation hook
#' @param data data.frame
#' @param col_map named list or NULL
#' @param required_keys character vector of required keys
#' @param fn function name (string)
#' @keywords internal
hm_validate_inputs <- function(data, col_map, required_keys, fn) {
  if (!is.data.frame(data)) {
    rlang::abort(sprintf("%s(): `data` must be a data.frame or tibble.", fn),
                 class = sprintf("healthmarkers_%s_error_data_type", fn))
  }

  # If no keys required (e.g., summarizers), allow NULL or empty col_map
  if (length(required_keys) == 0L) {
    if (!is.null(col_map) && !is.list(col_map)) {
      rlang::abort(sprintf("%s(): `col_map` must be a named list or NULL.", fn),
                   class = sprintf("healthmarkers_%s_error_colmap_type", fn))
    }
    return(invisible(TRUE))
  }

  # Validate col_map structure
  if (is.null(col_map) || !is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort(sprintf("%s(): `col_map` must be a named list of required keys -> column names.", fn),
                 class = sprintf("healthmarkers_%s_error_colmap_type", fn))
  }

  # Ensure all required keys present and mapped to non-empty names
  have_names <- rlang::`%||%`(names(col_map), character(0))
  missing_keys <- setdiff(required_keys, have_names)
  if (length(missing_keys)) {
    rlang::abort(
      sprintf("%s(): `col_map` missing entries for: %s", fn, paste(missing_keys, collapse = ", ")),
      class = sprintf("healthmarkers_%s_error_missing_map", fn)
    )
  }
  mapped <- vapply(required_keys, function(k) {
    val <- col_map[[k]]
    if (is.null(val) || is.na(val)) "" else as.character(val)
  }, character(1))
  if (any(!nzchar(mapped))) {
    bad <- required_keys[!nzchar(mapped)]
    rlang::abort(
      sprintf("%s(): `col_map` has empty mapping for: %s", fn, paste(bad, collapse = ", ")),
      class = sprintf("healthmarkers_%s_error_missing_map", fn)
    )
  }

  invisible(TRUE)
}

# Utility: coerce selected columns to numeric, warn when NAs introduced, non-finite -> NA
hm_coerce_numeric <- function(data, cols, fn = "healthmarkers") {
  for (cn in intersect(cols, names(data))) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("%s(): column '%s' coerced to numeric; NAs introduced: %d", fn, cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }
  data
}

#' Quiet name repair binder (if you need it elsewhere)
#' @importFrom vctrs vec_as_names
#' @keywords internal
hm_bind_cols_quiet <- function(...) {
  dplyr::bind_cols(...,
    .name_repair = ~ vctrs::vec_as_names(., repair = "unique", quiet = TRUE)
  )
}

#' Format a column-map resolution line for verbose output
#'
#' Produces a single string like:
#' "fn(): column map: G0 -> 'col_a', I0 -> 'col_b', ..."
#'
#' @param col_map Named list of key -> column-name mappings (only the keys
#'   actually used by the function need to be supplied).
#' @param fn Optional function name prefixed to the message.
#' @keywords internal
hm_fmt_col_map <- function(col_map, fn = NULL) {
  nms   <- names(col_map)
  parts <- vapply(nms, function(k) paste0(k, " -> '", col_map[[k]], "'"), character(1))
  msg   <- paste(parts, collapse = ", ")
  if (!is.null(fn)) msg <- paste0(fn, "(): column map: ", msg)
  msg
}

# ---------------------------------------------------------------------------
# Package-level constant: short internal key -> hm_col_report() dictionary key
# Used by both .hm_autofill_col_map() and .hm_build_col_map().
# Edit ONLY this one definition to add/rename mappings.
# ---------------------------------------------------------------------------
.hm_short_to_dict <- c(
  G0    = "fasting_glucose",  I0    = "fasting_insulin",
  G30   = "glucose_30m",      I30   = "insulin_30m",
  G120  = "glucose_120m",     I120  = "insulin_120m",
  TC    = "total_cholesterol",
  HDL_c = "HDL_c",            LDL_c = "LDL_c",
  TG    = "TG",
  total_chol = "total_cholesterol", sbp  = "sbp",
  smoker = "smoking",         bp_treated = "hypertension",
  diabetes = "diabetes",
  ApoA1 = "apoA1",            ApoB  = "apoB",
  ALT   = "ALT",              AST   = "AST",
  GGT   = "GGT",
  bilirubin  = "bilirubin",
  BMI   = "BMI",              bmi   = "BMI",
  waist = "waist",            weight = "weight",
  age   = "age",              sex   = "sex",
  eGFR  = "eGFR",             UACR  = "UACR",
  FFA   = "FFA",
  fat_mass     = "fat_mass",
  rate_palmitate = "rate_palmitate",
  rate_glycerol  = "rate_glycerol",
  creatinine   = "creatinine",
  albumin      = "albumin",   alb   = "albumin",
  calcium      = "calcium",   ca    = "calcium",
  platelets    = "platelets",
  height       = "height",
  ALM          = "ALM",       alm   = "ALM",
  bmd_t        = "BMD",       BMD   = "BMD",
  FEV1         = "FEV1",      FVC   = "FVC",
  fev1         = "FEV1",      fvc   = "FVC",
  FEV1pct      = "FEV1pct",
  kynurenine   = "kynurenine", tryptophan = "tryptophan",
  vitd         = "vitaminD",  vitaminD = "vitaminD",
  vitamin_d    = "vitaminD",
  # Inflammatory
  CRP          = "CRP",       IL6   = "IL6",        TNFa  = "TNFa",
  WBC          = "WBC",       neutrophils = "neutrophils",
  lymphocytes  = "lymphocytes", monocytes = "monocytes",
  eosinophils  = "eosinophils",
  ESR          = "ESR",
  # Renal
  BUN          = "BUN",       race  = "ethnicity",
  cystatin_C   = "cystatin_C", urea_serum = "urea_serum",
  creatinine_urine = "creatinine_urine", urea_urine = "urea_urine",
  # Vitamins / micronutrients
  VitD         = "vitaminD",  B12   = "vitaminB12",
  Folate       = "folate",    Ferritin = "ferritin",
  TSat         = "transferrin_sat",
  Cortisol     = "Cortisol",  DHEAS = "DHEAS",
  Testosterone = "testosterone", Estradiol = "estradiol",
  TSH          = "TSH",       free_T4 = "FT4",
  Retinol      = "Retinol",   Tocopherol = "Tocopherol",
  Total_lipids = "Total_lipids",
  VitC         = "VitC",      Homocysteine = "Homocysteine",
  MMA          = "MMA",       Magnesium = "magnesium",
  Zinc         = "zinc",      Copper = "copper",
  # Hormones
  total_testosterone = "testosterone",
  SHBG         = "SHBG",      LH    = "LH",         FSH   = "FSH",
  progesterone = "progesterone", free_T3 = "free_T3",
  aldosterone  = "aldosterone", renin = "renin",
  IGF1         = "IGF1",      prolactin = "prolactin",
  cortisol_0   = "cortisol_0",  cortisol_30 = "cortisol_30",
  insulin      = "fasting_insulin",
  # Sarcopenia
  strength     = "strength",  walking = "walking",
  chair        = "chair",     stairs  = "stairs",
  falls        = "falls",
  # BODE / pulmonary
  fev1_pct     = "FEV1pct",   sixmwd = "sixmwd",    mmrc  = "mmrc",
  fev1_pp      = "FEV1pct",
  # Saliva
  cort1        = "saliva_cort1", cort2 = "saliva_cort2",
  cort3        = "saliva_cort3", amylase = "saliva_amylase",
  glucose      = "saliva_glucose",
  # Endocrine / neuro
  nfl          = "nfl",
  GH           = "GH",        glucagon = "glucagon",
  PIVKA_II     = "PIVKA_II",
  # Metabolic risk features
  chol_total   = "total_cholesterol",
  chol_ldl     = "LDL_c",
  chol_hdl     = "HDL_c",
  triglycerides = "TG",
  age_year     = "age",
  z_HOMA       = "fasting_insulin",
  bp_sys_z     = "sbp",
  bp_dia_z     = "dbp",
  # Glycaemic / metabolic extras
  HbA1c        = "HbA1c",
  C_peptide    = "C_peptide",
  leptin       = "leptin",
  adiponectin  = "adiponectin"
)

# Auto-fill a col_map from hm_col_report() when the caller didn't supply one.
# Only fills keys present in the synonym dictionary; missing keys remain absent.
# Translates internal short keys (G0, I0, TG, ...) to their hm_col_report()
# dictionary key names, then back again so the returned list uses short keys.
# @param col_map  User-supplied col_map (may be NULL or missing).
# @param data     The data frame being analysed.
# @param keys     Character vector of short internal keys to fill.
# @param fn       Function name for informational message.
# @keywords internal
.hm_autofill_col_map <- function(col_map, data, keys, fn = "") {
  if (!is.null(col_map)) return(col_map)

  # Mapping: short internal key -> dictionary key used by hm_col_report()
  short_to_dict <- .hm_short_to_dict

  inferred <- tryCatch(
    hm_col_report(data, col_map = NULL, verbose = FALSE,
                  fuzzy = FALSE, show_unmatched = FALSE),
    error = function(e) list()
  )

  out <- list()
  for (k in keys) {
    dict_key <- if (!is.na(short_to_dict[k])) short_to_dict[[k]] else k
    if (!is.null(inferred[[dict_key]])) {
      out[[k]] <- inferred[[dict_key]]
    } else if (!is.null(inferred[[k]])) {
      # fallback: key already matches dictionary directly
      out[[k]] <- inferred[[k]]
    }
  }

  if (length(out) > 0L) {
    hm_inform(level = "debug",
              msg = sprintf("%s(): col_map not supplied -- auto-inferred %d/%d keys: %s",
                            fn, length(out), length(keys),
                            paste(names(out), unlist(out), sep = "->", collapse = ", ")))
  }
  out
}

#' Detect a participant/sample ID column in a data frame
#'
#' Checks column names against common patterns (case-insensitive exact match):
#' id, iid, participant_id, subject_id, sample_id, pid, sid, record_id.
#' Returns the first matching column name, or NULL if none found.
#' @keywords internal
.hm_detect_id_col <- function(data) {
  nms       <- names(data)
  nms_lower <- tolower(nms)
  candidates <- c("id", "iid", "participant_id", "participantid",
                  "subject_id",  "subjectid",  "sample_id",  "sampleid",
                  "pid",         "sid",         "record_id")
  for (cand in candidates) {
    m <- which(nms_lower == cand)
    if (length(m)) return(nms[m[1L]])
  }
  NULL
}

#' One-level pre-computation of missing keys from existing raw columns
#'
#' Given a dependency map (\code{deps}), for each key in \code{missing_keys}
#' checks whether its prerequisite columns are present in \code{data}. If yes,
#' the key is computed and added as a new column; otherwise an informational
#' message is emitted (when \code{verbose = TRUE}) indicating what to provide.
#'
#' @param data         data.frame to augment.
#' @param deps         Named list of dependency definitions, each with:
#'   \describe{
#'     \item{needs}{character vector of prerequisite column names}
#'     \item{describe}{short human-readable formula string}
#'     \item{compute}{function(data) returning a numeric vector}
#'   }
#' @param missing_keys character vector of key names to attempt to compute.
#' @param fn           calling function name (for messages).
#' @param verbose      logical.
#' @return A list with \code{$data} (augmented data.frame) and
#'   \code{$log} (character vector of per-key messages).
#' @keywords internal
.hm_precompute_from_deps <- function(data, deps, missing_keys,
                                     fn = "", verbose = FALSE) {
  if (length(missing_keys) == 0L) return(list(data = data, log = character(0)))
  log_msgs <- character(0)

  for (key in missing_keys) {
    dep <- deps[[key]]
    if (is.null(dep)) next

    prereqs_absent <- setdiff(dep$needs, names(data))

    if (length(prereqs_absent) == 0L) {
      computed <- tryCatch(dep$compute(data), error = function(e) NULL)
      if (!is.null(computed) && length(computed) == nrow(data)) {
        data[[key]] <- computed
        n_ok <- sum(!is.na(computed))
        msg  <- sprintf("  -> %s computed from %s [%s]: %d/%d valid",
                        key, paste(dep$needs, collapse = ", "),
                        dep$describe, n_ok, nrow(data))
        log_msgs <- c(log_msgs, msg)
        if (isTRUE(verbose)) {
          hm_inform(
            paste0(sprintf("%s(): pre-computation:\n", fn), msg),
            level = "inform"
          )
        }
      }
    } else {
      if (isTRUE(verbose)) {
        hm_inform(
          sprintf("%s(): pre-computation: %s cannot be derived -- provide: %s",
                  fn, key, paste(prereqs_absent, collapse = ", ")),
          level = "inform"
        )
      }
    }
  }

  list(data = data, log = log_msgs)
}

#' Build a complete col_map with dictionary inference and alias materialization
#'
#' Combines three previously separate steps into one call:
#' \enumerate{
#'   \item User-supplied entries in \code{col_map} are treated as authoritative
#'         Layer 0 seeds -- they are never overwritten.
#'   \item \code{hm_col_report()} fills any remaining unmatched keys via the
#'         full synonym dictionary (exact -> case-insensitive -> contains layers).
#'   \item Literal-name fallback: if a key in \code{keys} still has no match,
#'         but the key name exists verbatim as a column in \code{data}, it is
#'         used directly.
#'   \item Materialization: for every mapped key \code{k} where
#'         \code{col_map[[k]] != k} and \code{k} is \emph{not} already a column
#'         in \code{data}, a copy \code{data[[k]] <- data[[col_map[[k]]]]} is
#'         added. This ensures that downstream pre-computation helpers (which
#'         check \code{names(data)} for dependency names) and their
#'         \code{compute()} closures (which use literal key names) both resolve
#'         correctly even when the physical column has a non-standard name.
#' }
#'
#' @param data    The data.frame being analysed (modified locally; original
#'   is not altered outside the function call).
#' @param col_map Named list of user-supplied key -> column-name mappings, or
#'   \code{NULL} for full auto-inference.
#' @param keys    Character vector of short internal keys to resolve.
#' @param fn      Calling function name string (for messages).
#'
#' @return A list with four elements:
#' \describe{
#'   \item{data}{The (possibly augmented) data.frame.}
#'   \item{col_map}{Complete named list of key -> resolved column name.}
#'   \item{user_keys}{Keys that were explicitly provided in the original
#'     \code{col_map} argument.}
#'   \item{inferred_keys}{Keys resolved by dictionary inference or literal
#'     fallback (i.e., not in \code{user_keys}).}
#' }
#' @keywords internal
.hm_build_col_map <- function(data, col_map, keys, fn = "") {

  user_keys <- if (is.list(col_map) && length(col_map) > 0L)
    intersect(names(col_map), keys) else character(0)

  # Step 1+2: run hm_col_report with user seeds as Layer 0 ------------------
  inferred_full <- tryCatch(
    hm_col_report(data, col_map = col_map, verbose = FALSE,
                  fuzzy = FALSE, show_unmatched = FALSE),
    error = function(e) list()
  )

  # short_to_dict translation table ----------------------------------------
  short_to_dict <- .hm_short_to_dict

  # Step 2: assemble resolved col_map from inferred_full + literal fallback --
  out_map <- if (is.list(col_map) && length(col_map) > 0L) col_map else list()

  for (k in keys) {
    if (!is.null(out_map[[k]])) next  # user-supplied: keep
    dict_key <- if (!is.na(short_to_dict[k])) short_to_dict[[k]] else k
    if (!is.null(inferred_full[[dict_key]])) {
      out_map[[k]] <- inferred_full[[dict_key]]
    } else if (!is.null(inferred_full[[k]])) {
      out_map[[k]] <- inferred_full[[k]]
    } else if (k %in% names(data)) {
      # Step 3: literal fallback
      out_map[[k]] <- k
    }
  }

  inferred_keys <- setdiff(names(out_map)[names(out_map) %in% keys], user_keys)

  # Step 4: materialize aliases (col_map[[k]] != k) into data ----------------
  for (k in names(out_map)) {
    mapped_col <- out_map[[k]]
    if (!is.null(mapped_col) && mapped_col != k &&
        mapped_col %in% names(data) && !k %in% names(data)) {
      data[[k]] <- data[[mapped_col]]
    }
  }

  list(
    data          = data,
    col_map       = out_map,
    user_keys     = user_keys,
    inferred_keys = inferred_keys
  )
}

#' Summarise a result tibble: count non-NA rows per output column
#'
#' Produces a single string like:
#' "fn(): results: col_a 28/30, col_b 30/30, col_c 25/30"
#'
#' @param result A tibble / data.frame returned by a HealthMarkers function.
#' @param fn Optional function name prefixed to the message.
#' @keywords internal
hm_result_summary <- function(result, fn = NULL) {
  n     <- nrow(result)
  parts <- vapply(names(result), function(cn) {
    ok <- sum(!is.na(result[[cn]]))
    paste0(cn, " ", ok, "/", n)
  }, character(1))
  msg <- paste(parts, collapse = ", ")
  if (!is.null(fn)) msg <- paste0(fn, "(): results: ", msg)
  msg
}

# ---------------------------------------------------------------------------
# Global pre-computation -- Tier 0 of the DAG pipeline
# ---------------------------------------------------------------------------
#' Derive "Tier 0" variables before any marker function runs.
#'
#' Computes the following variables **only when they are absent** from `data`:
#' \describe{
#'   \item{BMI}{from `weight` (kg) and `height` (m or cm) via `weight / height_m^2`}
#'   \item{glucose / G0}{bidirectional alias: whichever is absent is filled from the other}
#'   \item{insulin / I0}{bidirectional alias (pmol/L <-> muU/mL unchanged -- same unit assumed)}
#'   \item{eGFR}{CKD-EPI 2009 creatinine equation from `creatinine`, `age`, `sex`
#'               (and optionally `race`). Written as `eGFR` only; downstream
#'               functions that need `eGFR_cr` receive it via col_map.}
#'   \item{UACR}{`urine_albumin` / `urine_creatinine` (both in mg/mmol or the
#'               ratio already in mg/g -- no unit conversion; caller's responsibility)}
#'   \item{LDL_c}{Friedewald: `TC - HDL_c - TG/2.2` (mmol/L); skipped if TG > 4.5}
#' }
#'
#' col_map keys are respected: if the user has mapped e.g.\ `creatinine -> "Cr_serum"`,
#' the materialized `data[["creatinine"]]` column (placed there by `.hm_build_col_map()`)
#' is used transparently.
#'
#' @param data    data.frame after col_map materialization.
#' @param col_map Named list of key -> column mappings (used only to resolve
#'   the physical column names when materialization has NOT been run yet).
#' @param verbose Logical; emit one-line summary per derived variable.
#'
#' @return A list:
#' \describe{
#'   \item{data}{data.frame with new columns appended.}
#'   \item{precomputed}{Character vector of variable names that were derived.}
#' }
#' @keywords internal
.hm_global_precompute <- function(data, col_map = NULL, verbose = FALSE) {
  precomputed <- character(0)
  fn <- "global_precompute"

  # Helper: resolve a key to actual column name in data (col_map then literal)
  .resolve <- function(key) {
    if (!is.null(col_map[[key]]) && col_map[[key]] %in% names(data))
      return(col_map[[key]])
    if (key %in% names(data)) return(key)
    NULL
  }
  .get <- function(key) {
    col <- .resolve(key)
    if (is.null(col)) return(NULL)
    as.numeric(data[[col]])
  }
  .absent <- function(key) is.null(.resolve(key))
  .log <- function(nm, src) {
    precomputed <<- c(precomputed, nm)
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): derived '%s' from %s", fn, nm,
                        paste(src, collapse = " + ")), level = "inform")
  }

  # --- 1. BMI -----------------------------------------------------------
  if (.absent("BMI")) {
    w <- .get("weight"); h <- .get("height")
    if (!is.null(w) && !is.null(h)) {
      h_m <- ifelse(is.finite(h) & h > 3, h / 100, h)
      bmi  <- w / (h_m ^ 2)
      data[["BMI"]] <- ifelse(is.finite(bmi) & bmi > 5 & bmi < 150, bmi, NA_real_)
      .log("BMI", c("weight", "height"))
    }
  }

  # --- 2. glucose <-> G0 alias ------------------------------------------
  if (.absent("glucose") && !.absent("G0")) {
    data[["glucose"]] <- .get("G0")
    .log("glucose", "G0")
  } else if (.absent("G0") && !.absent("glucose")) {
    data[["G0"]] <- .get("glucose")
    .log("G0", "glucose")
  }

  # --- 3. insulin <-> I0 alias ------------------------------------------
  if (.absent("insulin") && !.absent("I0")) {
    data[["insulin"]] <- .get("I0")
    .log("insulin", "I0")
  } else if (.absent("I0") && !.absent("insulin")) {
    data[["I0"]] <- .get("insulin")
    .log("I0", "insulin")
  }

  # --- 4. eGFR (CKD-EPI 2009 creatinine) --------------------------------
  if (.absent("eGFR")) {
    Cr  <- .get("creatinine")
    age <- .get("age")
    sex_col <- .resolve("sex")
    sex_raw <- if (!is.null(sex_col)) data[[sex_col]] else NULL
    if (!is.null(Cr) && !is.null(age) && !is.null(sex_raw)) {
      s_up <- toupper(as.character(sex_raw))
      sexi <- ifelse(startsWith(s_up, "M") | sex_raw %in% c(1, "1"), 1L, 0L)
      race_col <- .resolve("race")
      race_raw <- if (!is.null(race_col)) toupper(as.character(data[[race_col]])) else "OTHER"
      factor_race <- ifelse(grepl("BLACK|NHB|AFRIC", race_raw), 1.159, 1)
      kappa <- ifelse(sexi == 1L, 0.9, 0.7)
      alpha <- ifelse(sexi == 1L, -0.411, -0.329)
      # Female sex multiplier (CKD-EPI 2009)
      factor_sex  <- ifelse(sexi == 0L, 1.018, 1)
      ratio <- Cr / kappa
      min_r <- pmin(ratio, 1); max_r <- pmax(ratio, 1)
      egfr  <- 141 * (min_r ^ alpha) * (max_r ^ -1.209) *
        (0.993 ^ age) * factor_race * factor_sex
      egfr[!is.finite(egfr)] <- NA_real_
      data[["eGFR"]] <- egfr
      .log("eGFR", c("creatinine", "age", "sex"))
    }
  }

  # --- 5. UACR ----------------------------------------------------------
  if (.absent("UACR")) {
    ualb <- .get("urine_albumin")
    ucr  <- .get("urine_creatinine")
    if (!is.null(ualb) && !is.null(ucr)) {
      ratio <- ualb / ucr
      ratio[!is.finite(ratio)] <- NA_real_
      data[["UACR"]] <- ratio
      .log("UACR", c("urine_albumin", "urine_creatinine"))
    }
  }

  # --- 6. LDL_c (Friedewald, mmol/L) -----------------------------------
  if (.absent("LDL_c")) {
    tc    <- .get("TC")
    hdl   <- .get("HDL_c")
    tg    <- .get("TG")
    if (!is.null(tc) && !is.null(hdl) && !is.null(tg)) {
      ldl <- tc - hdl - tg / 2.2
      # Friedewald invalid when TG > 4.5 mmol/L
      ldl[is.finite(tg) & tg > 4.5] <- NA_real_
      ldl[!is.finite(ldl)] <- NA_real_
      data[["LDL_c"]] <- ldl
      .log("LDL_c", c("TC", "HDL_c", "TG"))
    }
  }

  # --- 7. WHR (waist-to-hip ratio) ------------------------------------
  if (.absent("whr") && .absent("WHR")) {
    wst <- .get("waist"); hip <- .get("hip")
    if (!is.null(wst) && !is.null(hip)) {
      r <- wst / hip
      r[!is.finite(r) | r <= 0] <- NA_real_
      data[["whr"]] <- r
      .log("whr", c("waist", "hip"))
    }
  }

  # --- 8. MAP (mean arterial pressure) ---------------------------------
  if (.absent("MAP")) {
    sbp <- .get("sbp"); dbp <- .get("dbp")
    if (!is.null(sbp) && !is.null(dbp)) {
      map <- (sbp + 2 * dbp) / 3
      map[!is.finite(map) | map <= 0] <- NA_real_
      data[["MAP"]] <- map
      .log("MAP", c("sbp", "dbp"))
    }
  }

  # --- 9. BUN-to-creatinine ratio -------------------------------------
  if (.absent("BUN_Cr_ratio")) {
    bun <- .get("BUN"); cr <- .get("creatinine")
    if (!is.null(bun) && !is.null(cr)) {
      r <- bun / cr
      r[!is.finite(r)] <- NA_real_
      data[["BUN_Cr_ratio"]] <- r
      .log("BUN_Cr_ratio", c("BUN", "creatinine"))
    }
  }

  # --- 10. VLDL (TG / 2.2, mmol/L) ------------------------------------
  if (.absent("VLDL")) {
    tg <- .get("TG")
    if (!is.null(tg)) {
      data[["VLDL"]] <- tg / 2.2
      .log("VLDL", "TG")
    }
  }

  # --- 11. non_HDL cholesterol ----------------------------------------
  if (.absent("non_HDL")) {
    tc <- .get("TC"); hdl <- .get("HDL_c")
    if (!is.null(tc) && !is.null(hdl)) {
      r <- tc - hdl
      r[!is.finite(r)] <- NA_real_
      data[["non_HDL"]] <- r
      .log("non_HDL", c("TC", "HDL_c"))
    }
  }

  # --- 12. remnant_c ---------------------------------------------------
  if (.absent("remnant_c")) {
    tc <- .get("TC"); hdl <- .get("HDL_c"); ldl <- .get("LDL_c")
    if (!is.null(tc) && !is.null(hdl) && !is.null(ldl)) {
      r <- tc - hdl - ldl
      r[!is.finite(r)] <- NA_real_
      data[["remnant_c"]] <- r
      .log("remnant_c", c("TC", "HDL_c", "LDL_c"))
    }
  }

  # --- 13. Lipid ratios ------------------------------------------------
  if (.absent("AIP")) {
    tg <- .get("TG"); hdl <- .get("HDL_c")
    if (!is.null(tg) && !is.null(hdl)) {
      r <- tg / hdl
      aip <- ifelse(is.finite(r) & r > 0, log10(r), NA_real_)
      data[["AIP"]] <- aip
      .log("AIP", c("TG", "HDL_c"))
    }
  }
  if (.absent("CRI_I")) {
    tc <- .get("TC"); hdl <- .get("HDL_c")
    if (!is.null(tc) && !is.null(hdl)) {
      r <- tc / hdl; r[!is.finite(r)] <- NA_real_
      data[["CRI_I"]] <- r
      .log("CRI_I", c("TC", "HDL_c"))
    }
  }
  if (.absent("CRI_II")) {
    ldl <- .get("LDL_c"); hdl <- .get("HDL_c")
    if (!is.null(ldl) && !is.null(hdl)) {
      r <- ldl / hdl; r[!is.finite(r)] <- NA_real_
      data[["CRI_II"]] <- r
      .log("CRI_II", c("LDL_c", "HDL_c"))
    }
  }
  if (.absent("HDL_TG_ratio")) {
    hdl <- .get("HDL_c"); tg <- .get("TG")
    if (!is.null(hdl) && !is.null(tg)) {
      r <- hdl / tg; r[!is.finite(r)] <- NA_real_
      data[["HDL_TG_ratio"]] <- r
      .log("HDL_TG_ratio", c("HDL_c", "TG"))
    }
  }
  if (.absent("LDL_HDL_ratio")) {
    ldl <- .get("LDL_c"); hdl <- .get("HDL_c")
    if (!is.null(ldl) && !is.null(hdl)) {
      r <- ldl / hdl; r[!is.finite(r)] <- NA_real_
      data[["LDL_HDL_ratio"]] <- r
      .log("LDL_HDL_ratio", c("LDL_c", "HDL_c"))
    }
  }

  # --- 14. Inflammatory cell ratios ------------------------------------
  if (.absent("NLR")) {
    n <- .get("neutrophils"); l <- .get("lymphocytes")
    if (!is.null(n) && !is.null(l)) {
      r <- n / l; r[!is.finite(r)] <- NA_real_
      data[["NLR"]] <- r
      .log("NLR", c("neutrophils", "lymphocytes"))
    }
  }
  if (.absent("dNLR")) {
    n <- .get("neutrophils"); w <- .get("WBC")
    if (!is.null(n) && !is.null(w)) {
      denom <- w - n
      r <- n / denom
      r[!is.finite(r) | r < 0] <- NA_real_
      data[["dNLR"]] <- r
      .log("dNLR", c("neutrophils", "WBC"))
    }
  }
  if (.absent("PLR")) {
    p <- .get("platelets"); l <- .get("lymphocytes")
    if (!is.null(p) && !is.null(l)) {
      r <- p / l; r[!is.finite(r)] <- NA_real_
      data[["PLR"]] <- r
      .log("PLR", c("platelets", "lymphocytes"))
    }
  }
  if (.absent("SII")) {
    n <- .get("neutrophils"); p <- .get("platelets"); l <- .get("lymphocytes")
    if (!is.null(n) && !is.null(p) && !is.null(l)) {
      r <- n * p / l; r[!is.finite(r)] <- NA_real_
      data[["SII"]] <- r
      .log("SII", c("neutrophils", "platelets", "lymphocytes"))
    }
  }
  if (.absent("SIRI")) {
    n <- .get("neutrophils"); m <- .get("monocytes"); l <- .get("lymphocytes")
    if (!is.null(n) && !is.null(m) && !is.null(l)) {
      r <- n * m / l; r[!is.finite(r)] <- NA_real_
      data[["SIRI"]] <- r
      .log("SIRI", c("neutrophils", "monocytes", "lymphocytes"))
    }
  }

  # --- 15. Electrolyte ratios ------------------------------------------
  if (.absent("sodium_potassium_ratio")) {
    na_v <- .get("sodium"); k_v <- .get("potassium")
    if (!is.null(na_v) && !is.null(k_v)) {
      r <- na_v / k_v; r[!is.finite(r)] <- NA_real_
      data[["sodium_potassium_ratio"]] <- r
      .log("sodium_potassium_ratio", c("sodium", "potassium"))
    }
  }
  if (.absent("Mg_Zn_den")) {
    mg <- .get("magnesium"); zn <- .get("zinc")
    if (!is.null(mg) && !is.null(zn)) {
      r <- mg / zn; r[!is.finite(r)] <- NA_real_
      data[["Mg_Zn_den"]] <- r
      .log("Mg_Zn_den", c("magnesium", "zinc"))
    }
  }
  if (.absent("Cu_Zn_den")) {
    cu <- .get("copper"); zn <- .get("zinc")
    if (!is.null(cu) && !is.null(zn)) {
      r <- cu / zn; r[!is.finite(r)] <- NA_real_
      data[["Cu_Zn_den"]] <- r
      .log("Cu_Zn_den", c("copper", "zinc"))
    }
  }

  # --- 16. Hormone ratios ----------------------------------------------
  if (.absent("Cort_DHEAS_den")) {
    cort <- .get("Cortisol"); dheas <- .get("DHEAS")
    if (!is.null(cort) && !is.null(dheas)) {
      r <- cort / dheas; r[!is.finite(r)] <- NA_real_
      data[["Cort_DHEAS_den"]] <- r
      .log("Cort_DHEAS_den", c("Cortisol", "DHEAS"))
    }
  }
  if (.absent("T_E2_den")) {
    t_v <- .get("testosterone"); e2 <- .get("estradiol")
    if (!is.null(t_v) && !is.null(e2)) {
      r <- t_v / e2; r[!is.finite(r)] <- NA_real_
      data[["T_E2_den"]] <- r
      .log("T_E2_den", c("testosterone", "estradiol"))
    }
  }
  if (.absent("TSH_fT4_den")) {
    tsh <- .get("TSH"); ft4 <- .get("FT4")
    if (!is.null(tsh) && !is.null(ft4)) {
      r <- tsh / ft4; r[!is.finite(r)] <- NA_real_
      data[["TSH_fT4_den"]] <- r
      .log("TSH_fT4_den", c("TSH", "FT4"))
    }
  }

  # --- 17. Amino-acid ratio --------------------------------------------
  if (.absent("Tyr_Phe_Ratio")) {
    tyr <- .get("Tyr"); phe <- .get("Phe")
    if (!is.null(tyr) && !is.null(phe)) {
      r <- tyr / phe; r[!is.finite(r)] <- NA_real_
      data[["Tyr_Phe_Ratio"]] <- r
      .log("Tyr_Phe_Ratio", c("Tyr", "Phe"))
    }
  }

  # --- 18. Urinary biomarker-to-creatinine ratios ----------------------
  ucr <- .get("u_creatinine")
  if (!is.null(ucr)) {
    for (pair in list(
      c("KIM1_gCr",  "KIM1"),
      c("NGAL_gCr",  "NGAL"),
      c("NAG_gCr",   "NAG"),
      c("L_FABP_gCr","L_FABP"),
      c("IL18_gCr",  "IL18")
    )) {
      nm <- pair[1]; src <- pair[2]
      if (.absent(nm)) {
        val <- .get(src)
        if (!is.null(val)) {
          r <- val / ucr; r[!is.finite(r)] <- NA_real_
          data[[nm]] <- r
          .log(nm, c(src, "u_creatinine"))
        }
      }
    }
  }

  derived_map <- setNames(as.list(precomputed), precomputed)
  list(data = data, precomputed = precomputed, derived_map = derived_map)
}