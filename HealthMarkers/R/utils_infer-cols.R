
#' Infer column names from user data based on flexible patterns, with logging
#'
#' Given a data.frame and a named mapping spec (e.g., list(G0 = NULL, I0 = NULL)),
#' infer the source column names for each key using a set of regex patterns.
#' You can supply your own patterns and "preferred" names to deterministically
#' resolve ambiguous matches. A structured log is kept and can be written to disk.
#'
#' This helper produces a col_map you can pass to HealthMarkers functions
#' (e.g., fasting_is(), lipid-derived indices).
#'
#' Backward compatibility:
#' - By default, strict = TRUE and strategy = "error" keep prior behavior:
#'   - Error if no match found.
#'   - Error if multiple candidates found.
#' - You can opt into smarter resolution via strategy = "prefer" or "first".
#'
#' @param data A data.frame or tibble whose column names are scanned.
#' @param map Named list where names are target keys (e.g., "G0","I0","TG") and
#'   values are NULL (to infer) or a user-supplied column name (to keep as-is).
#' @param verbose Logical; if TRUE, messages are printed for each mapping decision.
#'   Default TRUE.
#' @param log_file Optional file path; if supplied, a human-readable mapping log is written there.
#' @param patterns Optional named character vector of regex patterns keyed by the
#'   same names as `map`. If NULL, a built-in dictionary is used.
#' @param prefer Optional named list of character vectors with preferred column
#'   names for each key, used to resolve multiple matches deterministically.
#'   Matching is case-insensitive and exact against the provided names.
#' @param strategy One of c("error","prefer","first","stable") controlling resolution when
#'   there are multiple candidates. Default "error" (backward compatible).
#'   - "prefer": use `prefer` names first; else fall back to "stable" tie-break.
#'   - "first": take the first match in data's column order.
#'   - "stable": choose shortest name, then alphabetical.
#' @param strict Logical; if TRUE (default), missing matches error. If FALSE, missing
#'   matches leave \code{map[[key]]} as NULL and issue a warning.
#' @param ignore_case Logical; pass to grep(ignore.case = ...). Default TRUE.
#' @param fuzzy Logical; if TRUE and no regex matches are found, attempt a fuzzy match
#'   with agrep using `max_distance`. Default FALSE.
#' @param max_distance Numeric in \eqn{[0,1]} passed to agrep when fuzzy = TRUE. Default 0.1.
#' @param return One of c("map","list"). "map" (default) invisibly returns the
#'   filled mapping list. "list" returns a list(map = ..., log = tibble) for auditing.
#'
#' @return By default, invisibly returns the filled `map`. If return = "list",
#'   returns a list(map = \code{named list}, log = \code{tibble}).
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   fasting_glucose = c(5.5, 6.1),
#'   fasting_insulin = c(60, 88),
#'   TG = c(120, 150),
#'   `HDL-c` = c(50, 45),
#'   age = c(55, 60)
#' )
#' spec <- list(G0 = NULL, I0 = NULL, TG = NULL, HDL_c = NULL)
#' # Backward-compatible: strict and "error" strategy
#' res1 <- infer_cols(df, spec, verbose = FALSE)
#' # Prefer/resolve ties deterministically
#' res2 <- infer_cols(df, spec, strategy = "prefer", verbose = TRUE)
#' # Get structured log
#' res3 <- infer_cols(df, spec, return = "list")
infer_cols <- function(data,
                       map,
                       verbose = TRUE,
                       log_file = NULL,
                       patterns = NULL,
                       prefer = NULL,
                       strategy = c("error", "prefer", "first", "stable"),
                       strict = TRUE,
                       ignore_case = TRUE,
                       fuzzy = FALSE,
                       max_distance = 0.1,
                       return = c("map", "list")) {
  strategy <- match.arg(strategy)
  return <- match.arg(return)

  # ---- validations ----
  if (!is.data.frame(data)) stop("HealthMarkers::infer_cols: `data` must be a data.frame or tibble.")
  if (!is.list(map) || is.null(names(map)) || any(names(map) == "")) {
    stop("HealthMarkers::infer_cols: `map` must be a named list (e.g., list(G0=NULL, I0=NULL)).")
  }
  keys <- names(map)
  cn <- names(data)

  # Built-in regex dictionary (separator-tolerant: space/_/-/.)
  if (is.null(patterns)) {
    sep <- "[-_. ]"             # flexible separators
    opt <- function(x) paste0("(?:", x, ")?")  # optional group (non-capturing)
    # Tokens for common fields
    patterns <- c(
      # ---- Insulin / OGTT / Fasting ----
      G0   = paste0("^(?:G", opt(sep), "0(?:\\b|$)|glu(?:cose)?", opt(sep), "0(?:\\b|$)|",
                    "fast(?:ing)?", opt(sep), opt("plasma"), opt(sep), "glu(?:cose)?(?:\\b|$)|",
                    "FPG(?:\\b|$))"),
      I0   = paste0("^(?:I", opt(sep), "0(?:\\b|$)|ins(?:ulin)?", opt(sep), "0(?:\\b|$)|",
                    "fast(?:ing)?", opt(sep), "ins(?:ulin)?(?:\\b|$))"),
      G30  = paste0("^(?:G", opt(sep), "30(?:\\b|$)|glu(?:cose)?", opt(sep), "30(?:\\b|$))"),
      I30  = paste0("^(?:I", opt(sep), "30(?:\\b|$)|ins(?:ulin)?", opt(sep), "30(?:\\b|$))"),
      G120 = paste0("^(?:G", opt(sep), "120(?:\\b|$)|glu(?:cose)?", opt(sep), "120(?:\\b|$))"),
      I120 = paste0("^(?:I", opt(sep), "120(?:\\b|$)|ins(?:ulin)?", opt(sep), "120(?:\\b|$))"),

      # ---- Anthropometry ----
      height    = paste0("^(?:height|Height|height_m|body_height|stature|ht_cm)(?:\\b|$)"),
      weight    = paste0("^(?:weight|body", sep, "?weight)(?:\\b|$)"),
      bmi       = paste0("^(?:BMI|body", sep, "?mass", sep, "?index)(?:\\b|$)"),
      BMI       = paste0("^(?:BMI|body", sep, "?mass", sep, "?index)(?:\\b|$)"),
      waist     = paste0("^(?:WC|waist|waist", sep, "?circumference)(?:\\b|$)"),
      WC        = paste0("^(?:WC|waist|waist", sep, "?circumference)(?:\\b|$)"),
      age       = "^(?:age|years?)(?:\\b|$)",
      sex       = "^(?:sex|gender)(?:\\b|$)",
      SBP       = paste0("^(?:SBP|sys(?:tolic)?", sep, "?(?:blood", sep, "?)?pres(?:sure)?)(?:\\b|$)"),
      DBP       = paste0("^(?:DBP|dia(?:stolic)?", sep, "?(?:blood", sep, "?)?pres(?:sure)?)(?:\\b|$)"),
      FFA       = paste0("^(?:FFA|NEFA|free", sep, "?fatty", sep, "?acids?)(?:\\b|$)"),
      fat_mass  = paste0("^(?:fat", sep, "?mass|FM)(?:\\b|$)"),
      lean_mass = paste0("^(?:lean", sep, "?mass|LM|lean", sep, "?body", sep, "?mass)(?:\\b|$)"),
      ALM       = paste0("^(?:ALM|alm|ALM_kg|appendicular", sep, "?lean", sep, "?mass)(?:\\b|$)"),

      # ---- Lipids ----
      TG     = "^(?:TG|tri(?:acyl)?glyceri?des?)(?:\\b|$)",
      HDL_c  = paste0("^(?:HDL(?:", sep, "?c)?|HDL", sep, "?chol(?:esterol)?|",
                      "high", sep, "?density", sep, "?lipoprotein)(?:\\b|$)"),
      LDL_c  = paste0("^(?:LDL(?:", sep, "?c)?|LDL", sep, "?chol(?:esterol)?|",
                      "low", sep, "?density", sep, "?lipoprotein)(?:\\b|$)"),
      TC     = paste0("^(?:TC|total", sep, "?chol(?:esterol)?)(?:\\b|$)"),
      ApoB   = paste0("^(?:Apo", sep, "?B|apoB|apolipoprotein", sep, "?B|ApoB100)(?:\\b|$)"),
      ApoA1  = paste0("^(?:Apo", sep, "?A1|apoA1|apolipoprotein", sep, "?A1)(?:\\b|$)"),

      # ---- Cardiometabolic aliases ----
      chol_total    = paste0("^(?:chol", sep, "?total|total", sep, "?chol(?:esterol)?)(?:\\b|$)"),
      chol_ldl      = paste0("^(?:chol", sep, "?ldl|ldl)(?:\\b|$)"),
      chol_hdl      = paste0("^(?:chol", sep, "?hdl|hdl)(?:\\b|$)"),
      triglycerides = "^(?:TG|tri(?:acyl)?glyceri?des?)(?:\\b|$)",

      # ---- Liver ----
      AST        = paste0("^(?:AST|aspartate", sep, "?aminotransferase)(?:\\b|$)"),
      ALT        = paste0("^(?:ALT|alanine", sep, "?aminotransferase)(?:\\b|$)"),
      GGT        = paste0("^(?:GGT|gamma", sep, "?glutamyl", sep, "?transferase)(?:\\b|$)"),
      platelets  = paste0("^(?:platelets?|platelet", sep, "?count)(?:\\b|$)"),
      albumin    = paste0("^(?:albumin|serum", sep, "?albumin)(?:\\b|$)"),
      bilirubin  = "^(?:bilirubin|bili)(?:\\b|$)",
      creatinine = "^(?:creatinine|creat)(?:\\b|$)",

      # ---- Electrolytes & minerals ----
      calcium   = paste0("^(?:calcium|ca|Ca|ca_mgdl)(?:\\b|$)"),
      phosphate = paste0("^(?:phosphate|phos)(?:\\b|$)"),
      magnesium = paste0("^(?:magnesium|Mg|mg)(?:\\b|$)"),

      # ---- Renal / Urine ----
      eGFR             = paste0("^(?:eGFR|estimated", sep, "?GFR|CKD", sep, "?EPI)(?:\\b|$)"),
      UACR             = paste0("^(?:UACR|ACR|urine", sep, "?albumin", sep, "?creatinine", sep, "?ratio)(?:\\b|$)"),
      urine_albumin    = paste0("^(?:urine", sep, "?albumin)(?:\\b|$)"),
      urine_creatinine = paste0("^(?:urine", sep, "?creatinine)(?:\\b|$)"),
      plasma_Na        = paste0("^(?:plasma", sep, "?Na|serum", sep, "?Na)(?:\\b|$)"),
      urine_Na         = paste0("^(?:urine", sep, "?Na)(?:\\b|$)"),
      serum_creatinine = paste0("^(?:serum", sep, "?creatinine)(?:\\b|$)"),

      # ---- Sweat ----
      sweat_chloride   = paste0("^(?:sweat", sep, "?chloride)(?:\\b|$)"),
      sweat_Na         = paste0("^(?:sweat", sep, "?Na)(?:\\b|$)"),
      sweat_K          = paste0("^(?:sweat", sep, "?K)(?:\\b|$)"),
      sweat_lactate    = paste0("^(?:sweat", sep, "?lactate)(?:\\b|$)"),
      weight_before    = paste0("^(?:weight", sep, "?before)(?:\\b|$)"),
      weight_after     = paste0("^(?:weight", sep, "?after)(?:\\b|$)"),
      duration         = paste0("^(?:duration|time", sep, "?h)(?:\\b|$)"),
      body_surface_area = paste0("^(?:body", sep, "?surface", sep, "?area|BSA)(?:\\b|$)"),

      # ---- Tracer / metabolic flux ----
      rate_glycerol  = paste0("^(?:rate", sep, "?glycerol|glycerol", sep, "?fm)(?:\\b|$)"),
      rate_palmitate = paste0("^(?:rate", sep, "?palmitate|palmitate", sep, "?fm)(?:\\b|$)"),

      # ---- Tryptophan-kynurenine pathway ----
      tryptophan  = paste0("^(?:tryptophan|Trp", sep, "?uM|tryptophan", sep, "?umolL)(?:\\b|$)"),
      kynurenine  = paste0("^(?:kynurenine|Kyn", sep, "?nM|kynurenine", sep, "?nmolL)(?:\\b|$)"),

      # ---- Saliva ----
      saliva_cort1   = paste0("^(?:saliva", sep, "?cort1|cortisol", sep, "?wake)(?:\\b|$)"),
      saliva_cort2   = paste0("^(?:saliva", sep, "?cort2|cortisol", sep, "?30)(?:\\b|$)"),
      saliva_cort3   = paste0("^(?:saliva", sep, "?cort3|cortisol", sep, "?60)(?:\\b|$)"),
      saliva_amylase = paste0("^(?:saliva", sep, "?amylase)(?:\\b|$)"),
      saliva_glucose = paste0("^(?:saliva", sep, "?glucose)(?:\\b|$)")
    )
  }

  # default preferences (exact names to pick first if multiple matches)
  if (is.null(prefer)) {
    prefer <- list(
      G0 = c("G0", "glucose_0", "glucose0", "fasting_glucose", "fpg"),
      I0 = c("I0", "insulin_0", "insulin0", "fasting_insulin"),
      TG = c("TG", "triglycerides"),
      HDL_c = c("HDL_c", "HDL-c", "HDLc", "HDL", "hdl_chol"),
      LDL_c = c("LDL_c", "LDL-c", "LDLc", "LDL", "ldl_chol"),
      TC = c("TC", "total_cholesterol"),
      ApoB = c("ApoB", "apoB", "apolipoprotein_B", "ApoB100"),
      ApoA1 = c("ApoA1", "apoA1", "apolipoprotein_A1")
    )
  }

  logs <- list()
  add_log <- function(key, selected, candidates, reason) {
    logs[[length(logs) + 1L]] <<- list(
      key = key,
      selected = if (is.null(selected)) NA_character_ else selected,
      candidates = paste(candidates, collapse = ", "),
      reason = reason
    )
    if (isTRUE(verbose)) {
      msg <- sprintf("HealthMarkers::infer_cols - %s -> %s (%s)",
                     key, ifelse(is.null(selected), "<none>", selected), reason)
      message(msg)
    }
  }

  `%||%` <- function(a, b) if (is.null(a)) b else a

  pick_preferred <- function(cands, prefs) {
    if (length(cands) == 0L || length(prefs) == 0L) return(NULL)
    lc <- tolower(cands); lp <- tolower(prefs)
    for (p in lp) {
      hit <- which(lc == p)[1]
      if (length(hit) == 1L && !is.na(hit)) return(cands[hit])
    }
    NULL
  }

  pick_stable <- function(cands) {
    if (length(cands) <= 1L) return(cands)
    lens <- nchar(cands)
    ord <- order(lens, cands)
    cands[ord][1L]
  }

  # iterate over keys
  for (nm in keys) {
    # keep user-specified mapping
    if (!is.null(map[[nm]])) {
      if (map[[nm]] %in% cn) {
        add_log(nm, map[[nm]], map[[nm]], "user-supplied")
        next
      } else {
        msg <- sprintf("HealthMarkers::infer_cols: user-supplied column '%s' for key '%s' not found in data.", map[[nm]], nm)
        if (isTRUE(strict)) stop(msg) else { warning(msg, call. = FALSE); map[[nm]] <- NULL }
      }
    }

    pat <- if (nm %in% names(patterns)) patterns[[nm]] else NULL
    if (is.null(pat) || is.na(pat)) {
      msg <- sprintf("HealthMarkers::infer_cols: no pattern defined for '%s'.", nm)
      if (isTRUE(strict)) stop(msg) else { warning(msg, call. = FALSE); add_log(nm, NULL, character(0), "no pattern"); next }
    }

    hits <- grep(pat, cn, ignore.case = isTRUE(ignore_case), value = TRUE)
    # Fuzzy fallback if enabled and no hits
    if (length(hits) == 0L && isTRUE(fuzzy)) {
      idx <- tryCatch(agrep(nm, cn, max.distance = max_distance, ignore.case = isTRUE(ignore_case)), error = function(e) integer(0))
      if (length(idx)) {
        hits <- cn[idx]
      }
    }

    if (length(hits) == 0L) {
      msg <- sprintf("HealthMarkers::infer_cols: no match for '%s'.", nm)
      if (isTRUE(strict)) stop(msg) else { warning(msg, call. = FALSE); add_log(nm, NULL, hits, "no match"); next }
    }

    if (length(hits) == 1L) {
      map[[nm]] <- hits
      add_log(nm, hits, hits, "unique match")
      next
    }

    # resolve multiple candidates
    sel <- NULL
    reason <- NULL
    if (strategy == "prefer" && !is.null(prefer[[nm]])) {
      sel <- pick_preferred(hits, prefer[[nm]])
      if (!is.null(sel)) reason <- "preferred match"
    }
    if (is.null(sel)) {
      if (strategy == "first") {
        sel <- hits[1L]
        reason <- "first in data order"
      } else if (strategy == "stable" || (strategy == "prefer" && is.null(reason))) {
        sel <- pick_stable(hits)
        reason <- "stable tie-break (shortest, then alphabetical)"
      } else if (strategy == "error") {
        stop(sprintf("HealthMarkers::infer_cols: multiple candidates for '%s': %s",
                     nm, paste(hits, collapse = ", ")))
      }
    }

    map[[nm]] <- sel
    add_log(nm, sel, hits, reason %||% "resolved")
  }

  # write log if requested
  if (!is.null(log_file)) {
    lines <- vapply(logs, function(x) {
      sprintf("%s -> %s | candidates: [%s] | %s", x$key, x$selected, x$candidates, x$reason)
    }, character(1))
    writeLines(lines, con = log_file)
    if (isTRUE(verbose)) message("HealthMarkers::infer_cols - wrote inference log to ", log_file)
  }

  if (return == "list") {
    log_tbl <- if (length(logs)) {
      tibble::tibble(
        key = vapply(logs, `[[`, character(1), "key"),
        selected = vapply(logs, `[[`, character(1), "selected"),
        candidates = vapply(logs, `[[`, character(1), "candidates"),
        reason = vapply(logs, `[[`, character(1), "reason")
      )
    } else {
      tibble::tibble(key = character(0), selected = character(0), candidates = character(0), reason = character(0))
    }
    return(list(map = map, log = log_tbl))
  }

  invisible(map)
}

#' Simplified column inference for HealthMarkers aggregators
#'
#' Exact-name matching helper used by `all_health_markers()` and related wrappers.
#' It picks the first matching candidate for each key, logs decisions via
#' `hm_inform()` when `verbose = TRUE`, and errors if required keys cannot be
#' resolved.
#'
#' @param data Data frame whose column names are scanned.
#' @param patterns Named list of character vectors, each giving candidate column
#'   names for a key (first match wins).
#' @param required_keys Character vector of keys that must resolve; otherwise an
#'   error is raised.
#' @param verbose Logical; if TRUE, emits hm_inform() messages for matches and
#'   unresolved keys.
#' @return Named list mapping keys to column names; unresolved non-required keys
#'   become `NA_character_`.
#' @rdname infer_cols
#' @keywords internal
hm_infer_cols <- function(data, patterns, required_keys = names(patterns), verbose = FALSE) {
  if (!is.data.frame(data)) {
    rlang::abort("hm_infer_cols(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_infer_error_data_type")
  }
  if (!is.list(patterns) || is.null(names(patterns)) || any(names(patterns) == "")) {
    rlang::abort("hm_infer_cols(): `patterns` must be a named list of candidate names per key.",
                 class = "healthmarkers_infer_error_patterns_type")
  }

  hm_inform(level = if (isTRUE(verbose)) "inform" else "debug", msg = "hm_infer_cols(): inferring column map")

  resolved <- list()
  for (key in names(patterns)) {
    cands <- unique(na.omit(as.character(patterns[[key]])))
    hit <- intersect(cands, names(data))
    if (length(hit) >= 1L) {
      resolved[[key]] <- hit[[1L]]
      if (isTRUE(verbose)) hm_inform(level = "debug", msg = sprintf("hm_infer_cols(): key '%s' -> '%s'", key, hit[[1L]]))
    } else {
      resolved[[key]] <- NA_character_
      hm_inform(level = "debug", msg = sprintf("hm_infer_cols(): key '%s' unresolved", key))
    }
  }

  # Ensure required keys are found
  missing_req <- required_keys[is.na(rlang::`%||%`(unlist(resolved[required_keys]), NA_character_))]
  if (length(missing_req)) {
    rlang::abort(
      sprintf("hm_infer_cols(): could not infer columns for required keys: %s",
              paste(missing_req, collapse = ", ")),
      class = "healthmarkers_infer_error_missing_required"
    )
  }

  resolved
}

#' Internal: default exact-name patterns for hm_infer_cols()
#' Not exported; used by all_health_markers() when col_map is missing.
#' Each vector lists every column-name spelling observed in the package's
#' simulated data, the Inter99/ADDITION real phenotype dataset, and common
#' biobank / cohort study conventions.  The first element is the canonical
#' internal key name.
#' @keywords internal
.hm_default_col_patterns_exact <- function() {
  list(

  ## =========================================================
  ## Demographics & basic
  ## =========================================================
  age = c("age","Age","AGE","age_year","age_years","age_0","baseline_age",
          "participant_age","age_at_visit","age_at_recruitment","age_enrol",
          ## UKB
          "age_when_attended_assessment_centre_0_0",
          "age_when_attended_assessment_centre_1_0",
          "age_at_recruitment_0_0",
          ## NHANES
          "RIDAGEYR","ridageyr",
          ## Danish (Statistics Denmark, SUSY, GESUS, Inter99)
          "alder","ALDER","alderen","pt_alder","f_alder",
          ## Norwegian (HUNT, Tromso) -- same word as Danish
          ## Swedish (SCAPIS, TwinGene) -- same word as Danish
          ## Finnish (FinnGen, THL)
          "ika","ik\u00e4","syntymavuosi",
          ## Estonian (EstBB)
          "vanus",
          ## Dutch (LifeLines, Rotterdam Study)
          "leeftijd",
          ## LOINC (All of Us / OMOP flat exports)
          "LOINC_30525_0","loinc_30525_0"),
  sex = c("sex","Sex","SEX","gender","Gender","gender_qrisk",
          "biological_sex","sex_at_birth","male","Male","female","Female",
          "woman","women",
          ## UKB
          "sex_0_0","sex_f31_0_0","genetic_sex_0_0","genetic_sex_f22001_0_0",
          ## NHANES
          "RIAGENDR","riagendr",
          ## Danish (CPR register: koen 1=male 2=female)
          "koen","KOEN","kon","KON","koensvar","pt_koen",
          ## Norwegian (HUNT, Tromso)
          "kjonn","kj\u00f8nn","Kjonn",
          ## Swedish (SCAPIS, TwinGene)
          ## "kon" already above; also:
          "k\u00f6n",
          ## Finnish (FinnGen)
          "sukupuoli","sp",
          ## Estonian (EstBB)
          "sugu",
          ## Dutch (LifeLines, Rotterdam)
          "geslacht",
          ## Generation Scotland (GS)
          "genetic_sex"),
  ethnicity = c("ethnicity","Ethnicity","ethiniciy","race","Race",
                "ethnicity_qrisk","ASIAN","asian","BLACK","black",
                "HISPANIC","hispanic","WHITE","white","Caucasian","caucasian",
                "NHW","NHW_M","NHB","HISP","latino",
                ## UKB
                "ethnic_background_0_0","ethnic_background_1_0",
                "ethnic_background_f21000_0_0",
                ## NHANES
                "RIDRETH1","ridreth1","RIDRETH3","ridreth3",
                ## Generation Scotland
                "ethnic_group",
                ## Dutch (LifeLines)
                "etniciteit"),

  ## =========================================================
  ## Anthropometry & obesity
  ## =========================================================
  height = c("height","Height","HEIGHT","height_m","height_5",
             "body_height","stature","standing_height","ht_cm","cm",
             ## UKB
             "standing_height_0_0","standing_height_1_0",
             ## NHANES
             "BMXHT","bmxht",
             ## Danish (SUSY, DANHES, Inter99, GESUS)
             "hoejde","HOEJDE","hojde","hoejde_cm","kropshoejde",
             ## Norwegian (HUNT, Tromso) -- hoyde / hoyde
             "hoyde","hoyden","hoyde_cm",
             ## Swedish (SCAPIS) -- langd / langd
             "langd","langd_cm",
             ## Finnish (FinnGen)
             "pituus","pituus_cm",
             ## Estonian (EstBB)
             "pikkus",
             ## Dutch (LifeLines, Rotterdam)
             "lengte","lengte_cm",
             ## LOINC
             "LOINC_8302_2","loinc_8302_2"),
  weight = c("weight","Weight","WEIGHT","weight_kg","weight_0","weight_1",
             "weight_3","weight_5","weight_before","weight_after","wt","wt_kg","kg",
             ## UKB
             "weight_0_0","weight_1_0",
             ## NHANES
             "BMXWT","bmxwt",
             ## Danish
             "vaegt","VAEGT","kropsv\u00e6gt","kropsvaegt","vaegt_kg",
             ## Norwegian (HUNT, Tromso)
             "vekt","vekt_kg",
             ## Swedish (SCAPIS)
             "vikt","vikt_kg",
             ## Finnish (FinnGen)
             "paino","kehonpaino","paino_kg",
             ## Estonian (EstBB)
             "kaal","kaal_kg",
             ## Dutch (LifeLines, Rotterdam)
             "gewicht","gewicht_kg",
             ## LOINC
             "LOINC_29463_7","loinc_29463_7"),
  BMI    = c("BMI","bmi","bmi_0","bmi_1","bmi_3","bmi_5",
             "BMI_value","bmi_calc","BMIkgm2","bmi_kg_m2","body_mass_index",
             "BMI3025",
             ## UKB (also caught by Layer 3 via "body_mass_index")
             "body_mass_index_bmi_0_0","body_mass_index_bmi_1_0",
             ## NHANES
             "BMXBMI","bmxbmi",
             ## Finnish (FinnGen)
             "painoindeksi",
             ## Dutch (LifeLines)
             "lichaamsgewichtsindex",
             ## LOINC
             "LOINC_39156_5","loinc_39156_5"),
  waist  = c("waist","Waist","waist_cm","waist_0","waist_1","waist_3","waist_5",
             "waist_circ","waist_circumference","waist_measure","WC","wc",
             ## UKB
             "waist_circumference_0_0","waist_circumference_1_0",
             ## NHANES
             "BMXWAIST","bmxwaist",
             ## Danish
             "talje","TALJE","taljemaal","taljeomkreds","talje_cm",
             "taille","midje",
             ## Norwegian (HUNT, Tromso)
             "midjeomkrets","midjemaal","livvidde",
             ## Swedish (SCAPIS)
             "midjeomkrets","midjematt","midja",
             ## Finnish (FinnGen)
             "vyotaronymp\u00e4rys","vyotaronymparys","vyotaro",
             ## Estonian (EstBB)
             "vootkond",
             ## Dutch (LifeLines, Rotterdam)
             "tailleomtrek","buikomvang","middel_omtrek"),
  hip    = c("hip","Hip","hip_cm","hip_0","hip_1","hip_3","hip_5",
             "hip_circ","hip_circumference",
             ## UKB
             "hip_circumference_0_0","hip_circumference_1_0",
             ## NHANES
             "BMXHIP","bmxhip",
             ## Danish
             "hofte","HOFTE","hofteomkreds","hofte_cm",
             ## Norwegian (HUNT, Tromso)
             "hofteomkrets","hoftemaal",
             ## Swedish (SCAPIS)
             "hoftomkrets","hoftomkrets_cm",
             ## Finnish (FinnGen)
             "lantionymp\u00e4rys","lantionymp\u00e4rys_cm",
             ## Dutch (LifeLines, Rotterdam)
             "heupomtrek","heup_omtrek"),
  whr    = c("whr","WHR","whratio","whratio_5","waist_hip_ratio",
             "waist_to_BMI_ratio","waist_to_height_ratio","WHRadjBMI"),
  ABSI   = c("ABSI","absi"),
  BAI    = c("BAI","bai"),
  BRI    = c("BRI","bri"),
  RFM    = c("RFM","rfm"),
  WHRadjBMI   = c("WHRadjBMI"),
  obesity_metrics = c("obesity_metrics","metabolic_risk_features"),

  ## =========================================================
  ## Body composition / DXA
  ## =========================================================
  fat_mass = c("fat_mass","fatmass","FM","fm","fm_kg","fm_wt","fat_kg","body_fat_mass",
               "fatpct","fat_percent","fat_percentage","fatfreemass",
               "fatfreemassindex","fatmassindex","body_fat_perc"),
  lean_mass = c("lean_mass","leanmass","LM","lm","lm_kg","fat_free_mass",
                "fatfreemass","FFM","ffm"),
  ALM   = c("ALM","alm","ALM_kg","appendicular_lean_mass"),
  VAT   = c("VAT","vat","visceral_fat","visceral_adipose_tissue"),
  SAT   = c("SAT","SAT_VAT_ratio","subcutaneous_adipose_tissue",
            "subcutaneous_fat","SAT_kg","SAT_cm2"),
  BMD   = c("BMD","bmd","bmd_t","bone_mineral_density"),
  BMD_ref_mean = c("BMD_ref_mean"),
  BMD_ref_sd   = c("BMD_ref_sd"),
  liver_fat    = c("liver_fat","liver_fat_pct"),

  ## =========================================================
  ## Glycemic markers (glucose / HbA1c)
  ## =========================================================
  fasting_glucose = c(
    "G0","pglu0","pglu0_0","glu0","fasting_glucose","glucose_fasting",
    "glucose0","glucose_0","fpg","FPG","fasting_plasma_glucose",
    "fasting_bg","fbg","FBG","fast_glu","bg0","BG0",
    "plasma_glucose","serum_glucose","glucose_f","GLUC","glu","Glu",
    "GLUCOSE","glucose","nglu0","p_glucose0",
    ## UKB
    "glucose_0_0","glucose_1_0",
    ## NHANES
    "LBXGLU","lbxglu","LBXSGL","lbxsgl","LBDGLUSI","lbdglusi",
    ## Danish (NPU01370; glukose)
    "NPU01370","NPU21572",
    "glukose","p_glukose","s_glukose","fastende_glukose","faste_glukose",
    "blodsukker","fasteblodsukker","fP_glukose",
    ## Norwegian (HUNT, Tromso) -- same as Danish
    ## Swedish (SCAPIS) -- note: glukos (no final e)
    "glukos","p_glukos","s_glukos","fasteglukos",
    ## Finnish (FinnGen)
    "glukoosi","p_glukoosi","paastoglukoosi",
    ## Estonian (EstBB)
    "glukoos","p_glukoos",
    ## Dutch (LifeLines, Rotterdam)
    "glucose_nuchter","nuchtere_glucose",
    ## LOINC
    "LOINC_2345_7","loinc_2345_7","LOINC_14749_6","loinc_14749_6"),
  glucose_30m  = c("G30","pglu30","pglu30_0","glu30","glucose_30m",
                   "glucose_30","bg30","G30_0"),
  glucose_120m = c("G120","pglu120","pglu120_0","glu120","glucose_120",
                   "glucose_2h","glucose_120m","bg120","glu120m","G120_0"),
  glucose_generic = c("glucose"),
  HbA1c = c("hba1c","HbA1c","hba1c_0","hba1c_1","hba1c_3","hba1c_5",
             "A1c","a1c","HBA1C","HbA1C","glycated_hba1c","hba1c_mmol",
             "hemoglobin_a1c","HBAIC","ghb","GHB","hgba1c",
             "glycohemoglobin","HbA1c_ifcc",
             ## UKB
             "glycated_haemoglobin_hba1c_0_0","glycated_haemoglobin_hba1c_1_0",
             ## NHANES
             "LBXGH","lbxgh","LBDGH","lbdgh",
             ## Danish (NPU22089)
             "NPU22089","NPU27300",
             "hba1c_ifcc","hba1c_mmolmol","glykeret_hb","glykeret_haemoglobin",
             ## Finnish (FinnGen)
             "hemoglobiini_a1c","hba1c_hemoglobiini",
             ## Dutch (LifeLines, Rotterdam)
             "geglycosyleerd_hemoglobine",
             ## LOINC
             "LOINC_4548_4","loinc_4548_4","LOINC_59261_8","loinc_59261_8"),
  glycated_albumin = c("glycated_albumin","GlycatedAlbuminPct"),

  ## =========================================================
  ## Insulin / OGTT / C-peptide
  ## =========================================================
  fasting_insulin = c(
    "I0","insu0","insu0_0","ins0","insulin0","insulin_0","fasting_insulin",
    "insulin_fasting","ins_f","basal_insulin","FI","fi","fast_ins",
    "Insulin","INS","ins","plasma_insulin","serum_insulin","Xinsulin",
    "insulin_f","ins_fasting","insulin_bas","homair"),
  insulin_30m  = c("I30","insu30","insu30_0","ins30","insulin_30",
                   "ins_30","insulin_30m"),
  insulin_120m = c("I120","insu120","insu120_0","ins120","insulin_120",
                   "ins_2h","insulin_120m"),
  insulin_ogtt    = c("insulin_ogtt","insulin_panel","insulin_adipose",
                      "insulin_tracer_dxa","insulin"),
  c_peptide_0     = c("cp0","cpep0","C_peptide","c_peptide","cp0_0"),
  c_peptide_30    = c("cp30","cpep30"),
  c_peptide_120   = c("cp120","cpep120"),
  fast_is_index   = c("fasting_is","fasting"),
  glucose_markers = c("glycemic","glycemic_markers"),

  ## Derived IR / IS indices stored as columns
  HOMA_IR   = c("HOMA_IR","homair","z_HOMA","HOMA","homa_ir","homa","IR","IR_"),
  ISI       = c("ISI_matsuda","ISIstum0_120","isi","IS","insulin_sensitivity_index",
                "BIG_SI","BIG_AIR"),
  AISI      = c("AISI"),
  Avignon_Si0   = c("Avignon_Si0"),
  Avignon_Si120 = c("Avignon_Si120"),
  OGTT_ISI      = c("ogtt_is","OGTT","ISI_matsuda","ISIstum0_120"),

  ## =========================================================
  ## Lipids
  ## =========================================================
  total_cholesterol = c(
    "TC","tc","chol","Chol","CHOL","chol_0","chol_1","chol_3","chol_5",
    "total_cholesterol","total_chol","chol_total","chol_t","cholesterol",
    "Cholesterol","TCHOL","tot_chol","totalchol","TotalChol","total.chol",
    "s_chol","serum_cholesterol","plasma_cholesterol","TC_mmol","CHOLE",
    "tcholesterol","cholesterol_total",
    ## UKB
    "cholesterol_0_0","cholesterol_1_0",
    ## NHANES
    "LBXTC","lbxtc","LBDTCSI","lbdtcsi",
    ## Danish (NPU01567; kolesterol)
    "NPU01567",
    "kolesterol","p_kolesterol","s_kolesterol","total_kolesterol",
    "p_total_kolesterol","totalkolesterol",
    ## Finnish (FinnGen) -- note -i ending
    "kolesteroli","p_kolesteroli","s_kolesteroli","kokonaiskolesteroli",
    ## Estonian (EstBB) -- note double-o
    "kolesterool","p_kolesterool","kogukolesterool",
    ## German (NAKO, KORA, Gutenberg) -- Cholesterin (not cholesterol!)
    "Cholesterin","cholesterin","Gesamtcholesterin","gesamtcholesterin",
    ## Dutch (LifeLines, Rotterdam)
    "totaal_cholesterol","cholesterol_totaal",
    ## LOINC
    "LOINC_2093_3","loinc_2093_3"),
  HDL_c = c(
    "HDL_c","HDL","hdlc","hdlc_0","hdlc_1","hdlc_3","hdlc_5",
    "HDLc","hdl_chol","chol_hdl","hdl_c","HDL_cholesterol",
    "hdl_cholesterol","hdlchol","HDLC","s_HDL","p_hdl","HDL_mmol",
    "hdl_mmol","HDLCHOLEST","chol_hdl_c","high_density_lipoprotein",
    "total.hdl","cholesterol_HDL_ratio",
    ## UKB
    "hdl_cholesterol_0_0","hdl_cholesterol_1_0",
    ## NHANES
    "LBDHDD","lbdhdd","LBXHDD","lbxhdd","LBDHDSI","lbdhdsi",
    ## Danish (NPU01637)
    "NPU01637",
    "hdl_kolesterol","p_hdl_kolesterol","s_hdl_kolesterol",
    ## Finnish (FinnGen)
    "hdl_kolesteroli","p_hdl_kolesteroli",
    ## Estonian (EstBB)
    "hdl_kolesterool",
    ## German (NAKO, KORA)
    "HDL_Cholesterin","hdl_cholesterin",
    ## Dutch (LifeLines, Rotterdam)
    "hdl_cholesterol_nuchter",
    ## LOINC
    "LOINC_2085_9","loinc_2085_9"),
  LDL_c = c(
    "LDL_c","LDL","ldl","ldl_5","LDLc","ldl_c","ldl_chol",
    "ldlcalc","LDLcalc","LDL_PN","ldl_cholesterol","LDL_cholesterol",
    "LDLC","ldlchol","ldl_calc","s_LDL","p_ldl","LDL_mmol","ldl_mmol",
    "LDLCHOLEST","ldl_direct","LDL_direct","low_density_lipoprotein",
    ## UKB
    "ldl_direct_0_0","ldl_direct_1_0",
    ## NHANES
    "LBDLDL","lbdldl","LBXLDL","lbxldl","LBDLDLSI","lbdldlsi",
    ## Danish (NPU01568)
    "NPU01568",
    "ldl_kolesterol","p_ldl_kolesterol","s_ldl_kolesterol",
    ## Finnish (FinnGen)
    "ldl_kolesteroli","p_ldl_kolesteroli",
    ## Estonian (EstBB)
    "ldl_kolesterool",
    ## German (NAKO, KORA)
    "LDL_Cholesterin","ldl_cholesterin",
    ## LOINC
    "LOINC_13457_7","loinc_13457_7","LOINC_18262_6","loinc_18262_6"),
  TG    = c(
    "TG","tg","trig","trig_0","trig_1","trig_3","trig_5",
    "triglycerides","Triglycerides","TRIGLYCERIDES","trigs","tgs",
    "TRIG","TryG","tryg","triacylglycerol","triacylglycerols",
    "TAG","TAGs","tag","fasting_tg","fasting_triglycerides",
    "plasma_tg","serum_tg","tg_mmol","tg_mg","tg_0","TG_fasting",
    "s_TG","p_TG","lipid_tg","tg_baseline","triglycerid",
    "TGL","tgl","non_fasting_tg","TG_mgdl","uNMR_TRIG",
    ## UKB
    "triglycerides_0_0","triglycerides_1_0",
    ## NHANES
    "LBXTR","lbxtr","LBDTRSI","lbdtrsi",
    ## Danish (NPU01566; triglycerider)
    "NPU01566",
    "triglycerider","p_triglycerider","s_triglycerider",
    "p_triglycerid","triglyceridkonc",
    ## Norwegian (HUNT, Tromso) -- triglyserider (different ending)
    "triglyserider","p_triglyserider",
    ## Finnish (FinnGen)
    "triglyseridit","p_triglyseridit",
    ## Estonian (EstBB)
    "triglutseriiidid","triglutseriidid",
    ## German (NAKO, KORA) -- Triglyzeride
    "Triglyzeride","triglyzeride",
    ## Dutch (LifeLines, Rotterdam) -- triglyceriden
    "triglyceriden","p_triglyceriden",
    ## LOINC
    "LOINC_2571_8","loinc_2571_8"),
  VLDL  = c("VLDL","vldl","vldl_c","vldl_chol","vldlcalc","VLDLC",
            "vldl_cholesterol"),
  remnant_c = c("remnant_c","remCHOL","remnant_cholesterol"),
  non_HDL   = c("non_HDL_c","nonHDL","non_hdl","nonhdl","non_HDL_cholesterol",
               "nonHDL_cholesterol","non_hdl_chol"),
  apoA1 = c("apoA1","ApoA1","APOA1","apolipoprotein_A1","apo_A1","apo_a1",
             ## UKB (ukbtools uses "apolipoprotein_a" not "apolipoprotein_a1")
             "apolipoprotein_a","apolipoprotein_a_0_0","apolipoprotein_a_1_0"),
  apoB  = c("apoB","ApoB","APOB","apolipoprotein_B","ApoB100","apo_B","apo_b",
             ## UKB
             "apolipoprotein_b","apolipoprotein_b_0_0","apolipoprotein_b_1_0"),

  ## Atherogenic / lipid-derived indices
  AIP          = c("AIP","AIP_denHDL","ratio_TG_HDL","atherogenic","atherogenic_indices"),
  CRI_I        = c("CRI_I_denHDL"),
  CRI_II       = c("CRI_II_denHDL"),
  HDL_TG_ratio = c("ratio_TG_HDL"),
  TC_HDL_ratio = c("ratio_TC_HDL","cholesterol_HDL_ratio"),
  LDL_HDL_ratio = c("ratio_LDL_HDL"),

  ## =========================================================
  ## Blood pressure & heart rate
  ## =========================================================
  sbp = c("sbp","SBP","sysbp","sysbp_0","sysbp_1","sysbp_3","sysbp_5",
          "systolic","systolic_bp","bp_sys","bp_sys_z","systolic_blood_pressure",
          "blood_pressure_systolic","std_systolic_blood_pressure",
          ## UKB
          "systolic_blood_pressure_automated_reading_0_0",
          "systolic_blood_pressure_automated_reading_0_1",
          ## NHANES
          "BPXOSY1","bpxosy1",
          ## Danish (SUSY, DANHES, clinical EHR)
          "systolisk","systolisk_bt","systolisk_blodtryk",
          "sbt","SBT","sys_bt","sbt1","sbt2",
          ## Norwegian (HUNT, Tromso) -- note double k: blodtrykk
          "systolisk_blodtrykk",
          ## Swedish (SCAPIS)
          "systoliskt_blodtryck",
          ## Dutch (LifeLines)
          "systolische_bloeddruk","sbd",
          ## Generation Scotland (GS)
          "SBP_mean","sbp_mean",
          ## LOINC
          "LOINC_8480_6","loinc_8480_6"),
  dbp = c("dbp","DBP","diabp","diabp_0","diabp_1","diabp_3","diabp_5",
          "diastolic","diastolic_bp","bp_dia","bp_dia_z",
          "blood_pressure_diastolic",
          ## UKB
          "diastolic_blood_pressure_automated_reading_0_0",
          ## NHANES
          "BPXODI1","bpxodi1",
          ## Danish
          "diastolisk","diastolisk_bt","diastolisk_blodtryk",
          "dbt","DBT","dia_bt","dbt1","dbt2",
          ## Norwegian (HUNT, Tromso)
          "diastolisk_blodtrykk",
          ## Swedish (SCAPIS)
          "diastoliskt_blodtryck",
          ## Dutch (LifeLines)
          "diastolische_bloeddruk","dbd",
          ## Generation Scotland (GS)
          "DBP_mean","dbp_mean",
          ## LOINC
          "LOINC_8462_4","loinc_8462_4"),
  pulse = c("pulse","Pulse","bpm","heart_rate","hr","heart.rate",
            "AvgRRInterval","pulse_rate","pulse_0",
            ## UKB
            "pulse_rate_automated_reading_0_0",
            ## NHANES
            "BPXOPLS1","bpxopls1",
            ## Danish
            "puls","PULS","hjertefrekvens","pulsfrekvens",
            ## Swedish (SCAPIS)
            "hj\u00e4rtfrekvens","hjartfrekvens",
            ## Finnish (FinnGen)
            "syke","pulssi",
            ## Dutch (LifeLines)
            "hartslag","polsslag",
            ## LOINC
            "LOINC_8867_4","loinc_8867_4"),
  MAP   = c("MAP","map","mean_arterial_pressure","PP","pp"),

  ## =========================================================
  ## Liver function
  ## =========================================================
  ALT = c("ALT","alt","alat","ALAT","GPT","gpt","GPT",
          "alanine_aminotransferase","alanine_transaminase",
          "SGPT","sgpt","liver_alt","alt_ul","ALT_UL","alt_iu","ALT_IU",
          "alt_u","alanine_aminotransferasa",
          ## UKB
          "alanine_aminotransferase_0_0","alanine_aminotransferase_1_0",
          ## NHANES
          "LBXSATSI","lbxsatsi","LBDSATSI","lbdsatsi",
          ## Danish (NPU03429)
          "NPU03429","NPU19651",
          "p_alat","s_alat","alanin_aminotransferase",
          ## Finnish (FinnGen)
          "alaniiniaminotransferaasi",
          ## German (NAKO, KORA)
          "GPT_ALAT","Alanin_Aminotransferase",
          ## Dutch (LifeLines)
          "alanine_transaminase_alat",
          ## LOINC
          "LOINC_1742_6","loinc_1742_6"),
  AST = c("AST","ast","asat","ASAT","GOT","got","SGOT","sgot",
          "aspartate_aminotransferase","aspartate_transaminase",
          "liver_ast","ast_ul","AST_UL","ast_iu","AST_IU",
          ## UKB
          "aspartate_aminotransferase_0_0","aspartate_aminotransferase_1_0",
          ## NHANES
          "LBXSASSI","lbxsassi","LBDSASSI","lbdsassi",
          ## Danish (NPU03631)
          "NPU03631",
          "p_asat","s_asat","aspartat_aminotransferase",
          ## Finnish (FinnGen)
          "aspartaattiaminotransferaasi",
          ## German (NAKO, KORA)
          "GOT_ASAT","Aspartat_Aminotransferase",
          ## LOINC
          "LOINC_1920_8","loinc_1920_8"),
  ALP = c("ALP","alp","ap","AP","alk_phos","alkaline_phosphatase",
          "alk_phosphatase","ALP_UL","alp_ul",
          ## UKB
          "alkaline_phosphatase_0_0","alkaline_phosphatase_1_0",
          ## NHANES
          "LBXSAPSI","lbxsapsi","LBDSAPSI","lbdsapsi",
          ## Danish (NPU01408 = P-Alkalisk fosfatase)
          "NPU01408",
          "alkalisk_fosfatase","p_alkalisk_fosfatase"),
  GGT = c("GGT","ggt","gamma_gt","gamma_glutamyltransferase",
          "gammaGT","gamma_GT","GGT_UL","ggt_ul",
          ## UKB (gamma_glutamyltransferase_0_0 caught by Layer 3)
          "gamma_glutamyltransferase_0_0","gamma_glutamyltransferase_1_0",
          ## NHANES
          "LBXSGTSI","lbxsgtsi","LBDSGTSI","lbdsgtsi",
          ## Danish (NPU01817)
          "NPU01817",
          "p_ggt","gamma_glutamyl_transferase",
          ## Finnish (FinnGen)
          "gammaglutamyylitransferaasi",
          ## German (NAKO, KORA)
          "Gamma_GT","GGT_gamma_GT",
          ## LOINC
          "LOINC_2324_2","loinc_2324_2"),
  bilirubin = c("bilirubin","bili","tbili","total_bilirubin","BILI",
                "Bilirubin","bilirubin_total","tbil","direct_bilirubin",
                ## UKB (total_bilirubin_0_0 caught by Layer 3)
                "total_bilirubin_0_0","total_bilirubin_1_0",
                ## NHANES
                "LBXSTB","lbxstb","LBDSTBSI","lbdstbsi",
                ## Danish (NPU01556 = P-Bilirubin; bilirubin = same in Danish)
                "NPU01556",
                "p_bilirubin","total_bilirubin_umol"),
  albumin   = c("albumin","Albumin","ALBUMIN","Alb","alb","ALB",
                "alb_gdl","serum_albumin","alb_s","plasma_albumin",
                "alb_serum","s_albumin",
                ## UKB
                "albumin_0_0","albumin_1_0",
                ## NHANES
                "LBXSAL","lbxsal","LBDSAL","lbdsal",
                ## Danish (NPU04998)
                "NPU04998",
                "p_albumin","s_albumin_gl","albumin_gl",
                ## Finnish (FinnGen)
                "albumiini","p_albumiini",
                ## Estonian (EstBB)
                "albumiin",
                ## Dutch (LifeLines) -- albumine (with e)
                "albumine",
                ## LOINC
                "LOINC_1751_7","loinc_1751_7"),
  total_protein = c("total_protein","total_prot","tot_protein","TP","tp",
                    ## UKB
                    "total_protein_0_0","total_protein_1_0",
                    ## NHANES
                    "LBXSTP","lbxstp","LBDSTP","lbdstp"),
  sev_liver = c("sev_liver","mild_liver"),

  ## =========================================================
  ## Kidney / renal (serum)
  ## =========================================================
  creatinine = c("creatinine","Creatinine","CREATININE","crea","crea_s",
                 "Cr","cr","SCr","sCr","serum_creatinine","creatinine_s",
                 "creatinine_serum","creat","scr","s_creatinine",
                 "p_creatinine","cr_mgdl","crea_umol",
                 ## UKB
                 "creatinine_0_0","creatinine_1_0",
                 ## NHANES
                 "LBXSCR","lbxscr","LBDSCR","lbdscr","LBDSCRSI","lbdscrsi",
                 ## Danish (NPU01994; kreatinin)
                 "NPU01994","NPU18016",
                 "kreatinin","p_kreatinin","s_kreatinin","kreatinin_umol",
                 ## Finnish (FinnGen) -- note double i: kreatiniini
                 "kreatiniini","p_kreatiniini",
                 ## Estonian (EstBB) -- kreatiniin
                 "kreatiniin",
                 ## German (NAKO, KORA)
                 "Kreatinin","Serumkreatinin",
                 ## Dutch (LifeLines, Rotterdam)
                 "creatinine_serum",
                 ## LOINC
                 "LOINC_2160_0","loinc_2160_0","LOINC_38483_4","loinc_38483_4"),
  BUN        = c("BUN","bun","blood_urea_nitrogen","urea_plasma",
                 ## NHANES
                 "LBXSBU","lbxsbu","LBDSBUSI","lbdsbusi",
                 ## Danish/Norwegian (carbamid = Scandinavian term for urea)
                 "carbamid","p_carbamid",
                 ## Dutch (LifeLines) -- ureum is the Dutch/German clinical term
                 "ureum","serum_ureum",
                 ## German (NAKO, KORA)
                 "Harnstoff","harnstoff",
                 ## LOINC
                 "LOINC_3094_0","loinc_3094_0","LOINC_6299_2","loinc_6299_2"),
  urea_serum = c("urea_serum","urea_s","serum_urea","urea",
                 ## UKB
                 "urea_0_0","urea_1_0",
                 ## Danish/Norwegian
                 "NPU01927",
                 "carbamid","p_carbamid","s_carbamid","carbamid_mmol",
                 ## Norwegian (HUNT)
                 "karbamid","p_karbamid",
                 ## Dutch (LifeLines, Rotterdam) -- ureum
                 "ureum","serum_ureum","plasma_ureum",
                 ## German (NAKO, KORA)
                 "Harnstoff","harnstoff","Serumharnstoff",
                 ## LOINC
                 "LOINC_22664_7","loinc_22664_7"),
  cystatin_C = c("cystatin_C","cystatinC","cystatin_c","CysC","cysc",
                 ## UKB (cystatin_c_0_0 caught by Layer 3)
                 "cystatin_c_0_0","cystatin_c_1_0",
                 ## NHANES
                 "LBXCYSTAT","lbxcystat","LBDCYTAT","lbdcytat",
                 ## Danish (NPU18454 = P-Cystatin C)
                 "NPU18454",
                 "p_cystatin_c","cystatin_c_mgl"),
  eGFR       = c("eGFR","egfr","gfr","estimated_gfr","GFR","mdrd_gfr",
                 "ckd_epi_gfr"),
  uric_acid  = c("uric_acid","uricacid","serum_urate","urate","UA","ua",
                 ## UKB
                 "urate_0_0","urate_1_0",
                 ## NHANES
                 "LBXSUA","lbxsua","LBDSUASI","lbdsuasi",
                 ## Danish/Norwegian (NPU01937; urinsyre)
                 "NPU01937",
                 "urinsyre","p_urinsyre","s_urinsyre",
                 ## Swedish (SCAPIS) -- urinsyra (note -a not -e)
                 "urinsyra","p_urinsyra",
                 ## Finnish (FinnGen)
                 "virtsahappo","uraatti",
                 ## Estonian (EstBB)
                 "kusihape",
                 ## German (NAKO, KORA)
                 "Harns\u00e4ure","Harnsaeure","harnsaeure",
                 ## Dutch (LifeLines, Rotterdam)
                 "urinezuur","serum_urinezuur",
                 ## LOINC
                 "LOINC_3084_1","loinc_3084_1"),

  ## Derived renal
  BUN_Cr_ratio = c("BUN_Cr_ratio","BUN_Cr_Ratio"),
  CKD_stage    = c("ckd_stage","kidney_failure_risk","kidney_kfre"),

  ## =========================================================
  ## Urine markers
  ## =========================================================
  u_albumin    = c("u_albumin","ualb","urine_albumin","u_albumin_mgL",
                   "urinary_albumin","alb_urine",
                   ## UKB
                   "microalbumin_in_urine_0_0","microalbumin_in_urine_1_0",
                   ## NHANES
                   "URXUMA","urxuma","URDUMA","urduma","URXUMASI","urxumasi",
                   ## Danish (NPU17550; mikroalbumin)
                   "NPU17550",
                   "u_albumin_mgl","urin_albumin","mikroalbumin","u_mikroalbumin",
                   ## Finnish (FinnGen)
                   "virtsa_albumiini","mikroalbuminuria",
                   ## Dutch (LifeLines)
                   "albumine_urine","micro_albumine",
                   ## LOINC
                   "LOINC_1754_1","loinc_1754_1"),
  u_creatinine = c("u_creatinine","ucrea","u_crea_mgdl","urine_creatinine",
                   "urinary_creatinine","uNMR_CREA",
                   ## UKB
                   "creatinine_enzymatic_in_urine_0_0",
                   "creatinine_enzymatic_in_urine_1_0",
                   ## NHANES
                   "URXUCR","urxucr","URDCR","urdcr","URXUCRSI","urxucrsi",
                   ## Danish (NPU17997; u_kreatinin)
                   "NPU17997",
                   "u_kreatinin","urin_kreatinin","u_kreatinin_mmol",
                   ## Finnish (FinnGen)
                   "virtsa_kreatiniini",
                   ## Dutch (LifeLines)
                   "creatinine_urine",
                   ## LOINC
                   "LOINC_2161_8","loinc_2161_8"),
  UACR         = c("UACR","ualbcrea","ualb_ucrea","UA_Cr_Ratio",
                   "albumin_creatinine_ratio","alb_cre_ratio",
                   "UACR_creatinine","urinary_ACR","uACR"),
  urine_protein = c("urine_protein","u_protein","urinary_protein","uprotein"),
  urine_Na      = c("urine_Na","uNa","u_Na","urinary_Na","u_sodium",
                    "urinary_sodium","uNMR_CREA",
                    ## UKB
                    "sodium_in_urine_0_0","sodium_in_urine_1_0",
                    ## NHANES
                    "URXUNA","urxuna","URDUNA","urduna"),
  urine_K       = c("urine_K","uK","u_K","urinary_K","u_potassium",
                    "urinary_potassium",
                    ## UKB
                    "potassium_in_urine_0_0","potassium_in_urine_1_0",
                    ## NHANES
                    "URXUPHS","urxuphs"),
  urine_Ca      = c("urine_Ca","uCa","u_Ca","urinary_Ca","u_calcium",
                    "urine_calcium","urinary_calcium"),
  urine_phos    = c("urine_phos","uPhos","u_phos","urinary_phosphate",
                    "u_phosphate","urine_phosphate"),
  urine         = c("urine_markers"),

  ## NMR urine metabolomics (Inter99/ADDITION naming)
  uNMR_CRTI    = c("uNMR_CRTI"),
  uNMR_GLUC    = c("uNMR_GLUC"),
  uNMR_ALAN    = c("uNMR_ALAN"),
  uNMR_LACT    = c("uNMR_LACT"),
  uNMR_ACTA    = c("uNMR_ACTA"),
  uNMR_SUCC    = c("uNMR_SUCC"),
  uNMR_CITR    = c("uNMR_CITR"),
  uNMR_DIME    = c("uNMR_DIME"),
  uNMR_TRIM    = c("uNMR_TRIM"),
  uNMR_BETA    = c("uNMR_BETA"),
  uNMR_GLYI    = c("uNMR_GLYI"),
  uNMR_FUMA    = c("uNMR_FUMA"),
  uNMR_FORM    = c("uNMR_FORM"),
  uNMR_X1MN    = c("uNMR_X1MN"),
  uNMR_NNDI    = c("uNMR_NNDI"),
  uNMR_HIPP    = c("uNMR_HIPP"),

  ## =========================================================
  ## Renal tubular injury markers (urine +/- gCr normalized)
  ## =========================================================
  A1M      = c("a1_micro","A1M_gCr","a1_microglobulin"),
  B2M      = c("beta2_micro","B2M_gCr","beta2_microglobulin","B2M"),
  KIM1     = c("KIM1","KIM1_gCr","kim1","kidney_injury_molecule_1"),
  KIM1_gCr = c("KIM1_gCr"),
  NGAL     = c("NGAL","NGAL_gCr","ngal","neutrophil_gelatinase"),
  NGAL_gCr = c("NGAL_gCr"),
  NAG      = c("NAG","nag"),
  NAG_gCr  = c("NAG_gCr"),
  L_FABP   = c("L_FABP","l_fabp","liver_fabp"),
  L_FABP_gCr = c("L_FABP_gCr"),
  IL18     = c("IL18","il18","IL_18","interleukin_18"),
  IL18_gCr = c("IL18_gCr"),

  ## =========================================================
  ## Neurology / neurofilament
  ## =========================================================
  nfl      = c("nfl","NfL","NFL","neurofilament_light","neurofilament_light_chain",
               "nfl_pgml","NfL_pgml"),

  ## =========================================================
  ## Endocrine / GI hormones
  ## =========================================================
  glucagon = c("glucagon","Glucagon","GLUCAGON","glucagon_pgml"),
  GH       = c("GH","gh","growth_hormone","GH_ngml","somatotropin"),
  PIVKA_II = c("PIVKA_II","PIVKA2","pivka_ii","des_gamma_carboxyprothrombin","DCP"),

  ## =========================================================
  ## Electrolytes & minerals
  ## =========================================================
  sodium    = c("sodium","Na","na","natrium","Na_plasma","serum_Na","plasma_sodium",
                ## UKB
                "sodium_0_0","sodium_1_0",
                ## NHANES
                "LBXSNASI","lbxsnasi","LBDSNASI","lbdsnasi",
                ## Danish/Norwegian (NPU01960; natrium)
                "NPU01960",
                "p_natrium","s_natrium","natrium_mmol",
                ## Estonian (EstBB) -- naatrium (extra 'a')
                "naatrium","p_naatrium",
                ## German (NAKO, KORA)
                "Natrium","Serumnatrium",
                ## Dutch (LifeLines)
                "natrium_serum",
                ## LOINC
                "LOINC_2951_2","loinc_2951_2"),
  potassium = c("potassium","K","k","Kalium","K_plasma","serum_K","plasma_potassium",
                ## UKB
                "potassium_0_0","potassium_1_0",
                ## NHANES
                "LBXSKSI","lbxsksi","LBDSKSI","lbdsksi",
                ## Danish/Norwegian (NPU01961; kalium)
                "NPU01961",
                "p_kalium","s_kalium","kalium_mmol",
                ## Estonian (EstBB) -- kaalium
                "kaalium","p_kaalium",
                ## German (NAKO, KORA)
                "Kalium","Serumkalium",
                ## Dutch (LifeLines)
                "kalium_serum",
                ## LOINC
                "LOINC_2823_3","loinc_2823_3"),
  chloride  = c("chloride","Cl","cl","serum_chloride",
                ## NHANES
                "LBXSCLSI","lbxsclsi","LBDSCLSI","lbdsclsi"),
  bicarbonate = c("bicarbonate","HCO3","hco3","bicarb",
                  ## NHANES
                  "LBXSC3SI","lbxsc3si","LBDSC3SI","lbdsc3si"),
  sodium_potassium_ratio = c("Na_K_ratio","U_Na_K_ratio_denK"),
  calcium   = c("calcium","Calcium","CALCIUM","ca","Ca","ca_mgdl",
                "serum_calcium","plasma_Ca",
                ## UKB
                "calcium_0_0","calcium_1_0",
                ## NHANES
                "LBXSCA","lbxsca","LBDSCASI","lbdscasi",
                ## Danish (NPU02497)
                "NPU02497",
                "p_calcium","s_calcium","calcium_mmol",
                ## Finnish (FinnGen)
                "kalsium","p_kalsium",
                ## German (NAKO, KORA)
                "Kalzium","Serumcalcium",
                ## LOINC
                "LOINC_17861_6","loinc_17861_6"),
  phosphate = c("phosphate","Phosphate","phos","PHOS","phosphorus",
                "inorganic_phosphate","serum_phos","p_phos",
                ## UKB
                "phosphate_0_0","phosphate_1_0",
                ## NHANES
                "LBXSPH","lbxsph","LBDSPHSI","lbdsphsi",
                ## Danish (NPU02319; fosfat)
                "NPU02319",
                "fosfat","p_fosfat","s_fosfat","fosfat_mmol",
                ## German (NAKO, KORA)
                "Phosphat","Serumphosphat","anorganisches_Phosphat",
                ## LOINC
                "LOINC_2777_1","loinc_2777_1"),
  corrected_calcium = c("corrected_calcium","calcium_corrected"),
  magnesium = c("magnesium","Magnesium","Mg","mg","serum_Mg","plasma_Mg",
                ## NHANES
                "LBXSMG","lbxsmg","LBDSMGSI","lbdsmgsi",
                ## Danish (NPU03792)
                "NPU03792",
                "p_magnesium","s_magnesium","magnesium_mmol",
                ## Finnish (FinnGen)
                "magnesium_seerumissa",
                ## German (NAKO, KORA)
                "Magnesium","Serummagnesium",
                ## LOINC
                "LOINC_19123_9","loinc_19123_9"),
  zinc      = c("zinc","Zinc","Zn","zn","serum_zinc","plasma_Zn"),
  copper    = c("copper","Copper","Cu","cu","serum_copper"),
  Mg_Zn_den = c("Mg_Zn_den"),
  Cu_Zn_den = c("Cu_Zn_den"),

  ## =========================================================
  ## Inflammatory markers & hematology
  ## =========================================================
  CRP = c("CRP","crp","crp_hs","hs_crp","hsCRP","hs_CRP",
          "CRP_tethys","high_sensitivity_crp","CRP_category",
          "CRP_mgL","crp_mgL","crp_mg_L","CRP_mg_L",
          "C_reactive_protein","c_reactive_protein","hscrp",
          "CRP_hs","CRP_high_sens",
          ## UKB
          "c_reactive_protein_0_0","c_reactive_protein_1_0",
          ## NHANES
          "LBXHSCRP","lbxhscrp","LBDHRP","lbdhrp",
          "LBXCRP","lbxcrp",
          ## Danish (NPU10438)
          "NPU10438",
          "p_crp","s_crp","hs_crp_mgl","crp_mgl",
          ## Finnish (FinnGen)
          "c_reaktiivinen_proteiini",
          ## German (NAKO, KORA)
          "CRP_hsCRP","hochsensitives_CRP","hs_CRP_mgl",
          ## Dutch (LifeLines)
          "c_reactief_proteine",
          ## LOINC
          "LOINC_30522_7","loinc_30522_7","LOINC_1988_5","loinc_1988_5"),
  IL6  = c("IL6","il6","IL_6","interleukin_6","il6_pgml","IL6_pgml"),
  TNFa = c("TNFa","tnfa","TNF_alpha","tnf_alpha","TNFalpha"),
  ESR  = c("ESR","esr","erythrocyte_sedimentation_rate"),
  dNLR = c("dNLR","dnlr"),
  NLR  = c("NLR","nlr","neutrophil_lymphocyte_ratio"),
  PLR  = c("PLR","plr","platelet_lymphocyte_ratio"),
  SII  = c("SII","sii"),
  SIRI = c("SIRI","siri"),
  WBC  = c("WBC","wbc","leukocytes","white_blood_cells","leucocytes",
           ## UKB
           "white_blood_cell_leucocyte_count_0_0",
           "white_blood_cell_leucocyte_count_1_0",
           ## NHANES
           "LBXWBCSI","lbxwbcsi","LBDWBCSI","lbdwbcsi",
           ## Danish (NPU02593; leukocytter)
           "NPU02593",
           "leukocytter","hvide_blodlegemer","b_leukocytter",
           ## Norwegian (HUNT) -- same as Danish
           ## Swedish (SCAPIS) -- leukocyter (single t)
           "leukocyter","b_leukocyter",
           ## Finnish (FinnGen)
           "leukosyytit","valkosolujen_lukumaara",
           ## Estonian (EstBB)
           "leukotsyydid",
           ## German (NAKO, KORA)
           "Leukozyten","Leukozytenzahl",
           ## Dutch (LifeLines)
           "leukocyten",
           ## LOINC
           "LOINC_6690_2","loinc_6690_2"),
  neutrophils = c("neutrophils","Neutrophils","NEUT","neut","neutro",
                  ## singular (UKB: neutrophil_count_0_0)
                  "neutrophil","neutrophil_count_0_0","neutrophil_count_1_0",
                  ## NHANES
                  "LBDNENO","lbdneno","LBXNEPCT","lbxnepct",
                  ## Danish (NPU04578; neutrofile = Danish plural)
                  "NPU04578",
                  "neutrofile","neutrofil","b_neutrofile"),
  lymphocytes = c("lymphocytes","Lymphocytes","LYMPH","lymph",
                  ## singular (UKB: lymphocyte_count_0_0)
                  "lymphocyte","lymphocyte_count_0_0","lymphocyte_count_1_0",
                  ## NHANES
                  "LBDLYMNO","lbdlymno","LBXLYPCT","lbxlypct",
                  ## Danish (NPU01455; lymfocytter = Danish)
                  "NPU01455",
                  "lymfocytter","lymfocyt","b_lymfocytter"),
  eosinophils = c("eosinophils","Eosinophils","eos","EOS","EOSIN",
                  ## singular (UKB: eosinophil_count_0_0)
                  "eosinophil","eosinophil_count_0_0","eosinophil_count_1_0",
                  ## NHANES
                  "LBDEOSNNO","lbdeosnno","LBXEOPCT","lbxeopct",
                  ## Danish (eosinofile = Danish)
                  "eosinofile","eosinofil","b_eosinofile"),
  monocytes   = c("monocytes","Monocytes","MONO","mono","monocyte_count",
                  ## UKB (monocyte_count_0_0 caught by Layer 3 via "monocyte_count")
                  "monocyte_count_0_0","monocyte_count_1_0",
                  ## NHANES
                  "LBDMONO","lbdmono","LBXMOPCT","lbxmopct",
                  ## Danish (monocytter = Danish)
                  "monocytter","monocyt","b_monocytter"),
  platelets   = c("platelets","Platelets","PLT","plt","thrombocytes","platelet_count","Platelet_Count","PLATELET",
                  ## UKB
                  "platelet_count_0_0","platelet_count_1_0",
                  ## NHANES
                  "LBXPLTSI","lbxpltsi","LBDPLTSI","lbdpltsi",
                  ## Danish (NPU03568; trombocytter)
                  "NPU03568",
                  "trombocytter","trombocyt","thrombocytter","b_trombocytter",
                  ## Swedish (SCAPIS) -- trombocyter
                  "trombocyter","b_trombocyter",
                  "TPK","tpk",
                  ## Finnish (FinnGen)
                  "trombosyytit",
                  ## Estonian (EstBB)
                  "trombotsyydid",
                  ## German (NAKO, KORA)
                  "Thrombozyten","Thrombozytenzahl",
                  ## Dutch (LifeLines)
                  "trombocyten","bloedplaatjes",
                  ## LOINC
                  "LOINC_777_3","loinc_777_3"),

  Hgb         = c("Hgb","hgb","Hb","hb","hemoglobin","haemoglobin","HGB","HB",
                  ## UKB
                  "haemoglobin_concentration_0_0","haemoglobin_concentration_1_0",
                  ## NHANES
                  "LBXHGB","lbxhgb","LBDHGB","lbdhgb",
                  ## Danish (NPU03609)
                  "NPU03609",
                  "b_haemoglobin","b_hgb","hb_konc",
                  ## Finnish (FinnGen)
                  "hemoglobiini","b_hemoglobiini",
                  ## Estonian (EstBB)
                  "hemoglobiin",
                  ## German (NAKO, KORA)
                  "H\u00e4moglobin","Haemoglobin","haemoglobin_konz",
                  ## Dutch (LifeLines) -- hemoglobine
                  "hemoglobine",
                  ## LOINC
                  "LOINC_718_7","loinc_718_7"),
  inflammatory_age     = c("inflammatory_age","iAge_raw","inf_age",
                           "immune_age","inflammaging_score"),
  inflammatory_markers = c("inflammatory","inflammatory_markers"),

  ## Cytokines / proteins from multiplex panel (ADDITION)
  ADIPOQ   = c("ADIPOQ","adiponectin","Adiponectin"),
  LEP      = c("LEP","leptin","Leptin"),
  RETN     = c("RETN","resistin","Resistin"),
  IGFBP1   = c("IGFBP1","igfbp1","IGFBP_1"),
  IGFBP2   = c("IGFBP2","igfbp2","IGFBP_2"),
  IGFBP3   = c("IGFBP3","igfbp3","IGFBP_3"),
  FTH1     = c("FTH1","fth1"),
  HSPA1B   = c("HSPA1B","hspa1b","HSP70"),
  DPP4     = c("DPP4","dpp4","DPP_4","dipeptidyl_peptidase_4"),
  GH1      = c("GH1","gh1","GH","gh","growth_hormone"),
  Proinsulin = c("Proinsulin","proinsulin","pro_insulin"),
  ntproBNP   = c("ntproBNP","NT_proBNP","nt_probnp","BNP","proBNP"),

  ## =========================================================
  ## Vitamins, micronutrients & iron
  ## =========================================================
  vitaminD = c(
    "vitd","VitD","VITD","vitd25","VitD25","vitd25_immu","vitd_level",
    "25OHD","25OHD3","25(OH)D","25_OHD","25OH_D","25OH_D3",
    "vitamin_d","vitamin_D","VITAMIN_D","oh25d","serum_25OHD",
    "25OHD_nmol","vitd_nmol","calcidiol","vitamin_d_25oh",
    "VitD_ref_mean","VitD_ref_sd","vitamin_d_status",
    "vit_d","vit_d25","s_vitd","plasma_vitd",
    ## UKB
    "vitamin_d_0_0","vitamin_d_1_0",
    ## NHANES
    "LBXVD2","lbxvd2","LBDVD2","lbdvd2",
    "LBDVDMSN","lbdvdmsn",
    ## Danish (NPU10501; d_vitamin)
    "NPU10501",
    "d_vitamin","dvitamin","25_oh_d_vitamin","p_25ohd",
    "d_vit","vitd_nmolL",
    ## Swedish (SCAPIS)
    "d_vitamin",
    ## Finnish (FinnGen)
    "D_vitamiini","d_vitamiini","kolekalsiferoli",
    ## German (NAKO, KORA)
    "Vitamin_D","25_OH_Vitamin_D","Calcidiol",
    ## Dutch (LifeLines)
    "vitamine_D","25_hydroxy_vitamine_D",
    ## LOINC
    "LOINC_62292_8","loinc_62292_8","LOINC_1989_3","loinc_1989_3"),
  vitaminB12 = c("vitb12","vitb12_immu","vitamin_b12","B12","b12","b12_level",
                 "vitaminB12","cobalamin","Cobalamin","cyanocobalamin",
                 ## NHANES
                 "LBXB12","lbxb12","LBDB12","lbdb12",
                 ## Danish (NPU01454 = P-Cobalamin; kobalamin = Danish spelling)
                 "NPU01454",
                 "kobalamin","p_kobalamin","p_b12","s_b12"),
  MMA        = c("MMA","mma","methylmalonic_acid"),
  folate     = c("folate","Folate","folate_immu","folic_acid","vitb9",
                 "folate_serum","serum_folate","folicacid",
                 ## NHANES
                 "LBXFOLSI","lbxfolsi","LBDFOLSI","lbdfolsi",
                 ## Danish (NPU01894 = P-Folat; folsyre = Danish for folic acid)
                 "NPU01894",
                 "folsyre","p_folat","s_folat","p_folsyre"),
  ferritin   = c("ferritin","Ferritin","ferri","ferri_immu","ferritin_s",
                 "serum_ferritin","FerritinTS","s_ferritin",
                 ## NHANES
                 "LBXFER","lbxfer","LBDFER","lbdfer",
                 ## Danish (NPU04698)
                 "NPU04698",
                 "p_ferritin","ferritin_mgl","ferritin_ngl",
                 ## Finnish (FinnGen)
                 "ferritiini","p_ferritiini",
                 ## Estonian (EstBB)
                 "ferritiin",
                 ## German (NAKO, KORA)
                 "Ferritin","Serumferritin",
                 ## Dutch (LifeLines)
                 "ferritine",
                 ## LOINC
                 "LOINC_2276_4","loinc_2276_4"),
  iron       = c("iron","Iron","iron_s","serum_iron","s_iron","Fe","fe",
                 ## NHANES
                 "LBXIRN","lbxirn","LBDIRNSI","lbdirnsi",
                 ## Danish (NPU01581 = P-Jern; jern = Danish for iron)
                 "NPU01581",
                 "jern","p_jern","s_jern","jern_umol"),
  transferrin     = c("transferrin","Transferrin","tf","transferrin_s",
                      ## NHANES
                      "LBXTFN","lbxtfn"),
  transferrin_sat = c("TSat","transferrin_sat","Ferr_TSat_den",
                      "transferrin_saturation","iron_saturation"),
  Retinol    = c("Retinol","retinol","vitamin_A","Retinol_ref_mean",
                 "Retinol_ref_sd"),
  VitC       = c("VitC","vitc","vitamin_c","Vitamin_C","ascorbate",
                 "ascorbic_acid","vit_c"),
  Tocopherol = c("Tocopherol","tocopherol","vitamin_E","alpha_tocopherol"),
  DHA        = c("DHA","dha","docosahexaenoic_acid"),
  EPA        = c("EPA","epa","eicosapentaenoic_acid"),
  FFA        = c("FFA","ffa","free_fatty_acids","NEFA","nefa"),
  Homocysteine = c("Homocysteine","homocysteine","hcy","HCY","tHcy",
                   ## NHANES
                   "LBXHCY","lbxhcy","LBDHCYSI","lbdhcysi",
                   ## Danish (NPU19404; homocystein)
                   "NPU19404",
                   "homocystein","p_homocystein","s_homocystein",
                   ## Finnish (FinnGen)
                   "homokysteiini",
                   ## German (NAKO, KORA)
                   "Homozystein","Homocystein",
                   ## Dutch (LifeLines)
                   "homocysteine_plasma",
                   ## LOINC
                   "LOINC_13965_9","loinc_13965_9"),
  Total_lipids = c("Total_lipids","total_lipids"),

  ## =========================================================
  ## Hormones
  ## =========================================================
  PTH          = c("pth","PTH","pth_immu","parathyroid_hormone","iPTH",
                   ## NHANES
                   "LBXPTH","lbxpth","LBDPTHSI","lbdpthsi"),
  TSH          = c("tsh","TSH","tsh_immu","thyroid_stimulating_hormone",
                   "thyrotropin","TSH_mIU",
                   ## UKB
                   "thyroid_stimulating_hormone_tsh_0_0",
                   ## Danish (NPU01407)
                   "NPU01407",
                   "p_tsh","s_tsh","tsh_miu_l",
                   ## Finnish (FinnGen)
                   "tyreotropiini","TSH_arvo",
                   ## German (NAKO, KORA)
                   "TSH_basal","Thyreotropin",
                   ## Dutch (LifeLines)
                   "thyreotroop_hormoon",
                   ## LOINC
                   "LOINC_3016_3","loinc_3016_3"),
  FT4          = c("ft4","FT4","free_t4","free_T4","free_thyroxine",
                   "fT4","FT4_pmol"),
  free_T3      = c("free_T3","fT3","ft3","FT3","free_triiodothyronine",
                   "FT3_pmol"),
  testosterone = c("testosterone","Testosterone","testo",
                   "total_testosterone","TT","tt_nmol",
                   ## UKB
                   "testosterone_0_0","testosterone_1_0",
                   ## Danish (NPU27281; testosteron)
                   "NPU27281",
                   "testosteron","p_testosteron","s_testosteron",
                   ## Finnish (FinnGen)
                   "testosteroni","p_testosteroni",
                   ## German (NAKO, KORA)
                   "Testosteron","Gesamttestosteron",
                   ## Dutch (LifeLines)
                   "testosteron_totaal",
                   ## LOINC
                   "LOINC_2986_3","loinc_2986_3"),
  estradiol    = c("estradiol","Estradiol","e2","E2","estrad",
                   "oestradiol","estradiol_pmol",
                   ## UKB (oestradiol_0_0 caught by Layer 3 via "oestradiol")
                   "oestradiol_0_0","oestradiol_1_0"),
  progesterone = c("progesterone","Progesterone","PROG","prog"),
  prolactin    = c("prolactin","Prolactin","PRL","prl"),
  FSH          = c("FSH","fsh","follicle_stimulating_hormone"),
  LH           = c("LH","lh","luteinising_hormone","luteinizing_hormone"),
  DHEAS        = c("DHEAS","dheas","dhaes","dehydroepiandrosterone_s",
                   "dhea_s","DHEA_S"),
  Cortisol     = c("Cortisol","cortisol","cort1","cort2","cort3",
                   "cortisol_0","cortisol_30","cortisol_am",
                   "morning_cortisol","cortisol_nmol"),
  cortisol_0   = c("cortisol_0","cort1","cort_wake","saliva_cort1"),
  cortisol_30  = c("cortisol_30","cort2","cort_30min","saliva_cort2"),
  renin        = c("renin","Renin","plasma_renin","renin_activity","PRA"),
  aldosterone  = c("aldosterone","Aldosterone","plasma_aldosterone"),
  SHBG         = c("SHBG","shbg","sex_hormone_binding_globulin","SHBG_nmol",
                   ## UKB
                   "sex_hormone_binding_globulin_0_0",
                   "sex_hormone_binding_globulin_1_0"),
  IGF1         = c("IGF1","igf1","IGF_1","insulin_growth_factor",
                   "insulin_like_growth_factor","IGF1_ngml",
                   ## UKB
                   "insulin_like_growth_factor_igf_1_0_0",
                   "insulin_like_growth_factor_igf_1_1_0"),
  tpoab        = c("tpoab","TPOAB","TPO_Ab","anti_TPO","anti_thyroid_peroxidase"),

  ## Hormone ratios (derived)
  Cort_DHEAS_den = c("Cort_DHEAS_den"),
  T_E2_den       = c("T_E2_den"),
  TSH_fT4_den    = c("TSH_fT4_den"),

  ## =========================================================
  ## Pulmonary / spirometry
  ## =========================================================
  FEV1     = c("FEV1","fev1","FEV1_pred","fev1_pred","fev1_post","FEV1_L",
               ## UKB
               "forced_expiratory_volume_in_1_second_fev1_0_0",
               "forced_expiratory_volume_in_1_second_fev1_0_2"),
  FEV1pct  = c("FEV1pct","fev1_pct","fev1_pp","FEV1FVCratio"),
  FVC      = c("FVC","fvc","fvc_post","FVC_L",
               ## UKB
               "forced_vital_capacity_fvc_0_0",
               "forced_vital_capacity_fvc_0_2"),
  FEV1FVC  = c("FEV1FVC","FEV1FVCratio","fev1_fvc","fev1fvc",
               ## UKB
               "fev1_fvc_ratio_0_0","fev1_fvc_ratio_1_0"),
  mmrc     = c("mmrc","mMRC","MMRC","mrc_dyspnoea","mrc_score"),
  sixmwd   = c("sixmwd","six_minute_walk","6mwd","6MWD","walk_6min"),
  COPD     = c("copd","COPD","chronic_obstructive_pulmonary"),
  pulmo_markers = c("pulmo","pulmo_markers"),

  ## =========================================================
  ## Cardiovascular / ECG
  ## =========================================================
  qtcf     = c("qtcf","QTcF","qtc","QTc","QTcf_ms"),
  qt_interval = c("qt_interval","QT","qt","QT_ms"),
  qrs_duration = c("qrs_duration","QRS","qrs","QRS_ms"),
  PR_interval  = c("PR_Interval","pr_interval","PR","pr"),
  ntproBNP_cv  = c("ntproBNP","BNP","NT_proBNP"),

  ## =========================================================
  ## Bone markers
  ## =========================================================
  BSAP      = c("BSAP","bsap","bone_specific_alkaline_phosphatase"),
  CTX       = c("CTX","ctx","CTX_I","C_telopeptide"),
  PINP      = c("PINP","pinp","procollagen_type1","P1NP"),
  Osteocalcin = c("Osteocalcin","osteocalcin","OC","oc","bone_gla_protein"),
  TBS       = c("TBS","tbs","trabecular_bone_score"),
  parent_fracture = c("parent_fracture","parental_fracture",
                      "family_fracture","hip_fracture_parent"),
  prior_fracture  = c("prior_fracture","previous_fracture",
                      "fracture_history","fracture_hx"),

  ## =========================================================
  ## Frailty / sarcopenia
  ## =========================================================
  frailty_index = c("frailty_index","frailty","fi_score","rockwood"),
  sarc_f   = c("sarc_f","sarc_f_score","SarcF","sarcf","SARC_F"),
  strength = c("strength","Strength","grip_strength","handgrip",
               "handgripMax","handgrip_kg","grip"),
  chair    = c("chair","Chair","chair_stand","five_chair_stands",
               "chair_stand_time"),
  stairs   = c("stairs","Stairs","stair_climb"),
  walking  = c("walking","Walking","Walk_m","gait_speed","walk_speed",
               "usual_gait_speed"),
  falls    = c("falls","Falls","fall_history","number_of_falls"),

  ## =========================================================
  ## Lifestyle & comorbidities
  ## =========================================================
  smoking  = c("smoking","smoke","smoker","Smoking","smoke_daily_gr",
               "smoke_packyrs","smoking_heavy","current_smoker","ever_smoker"),
  alcohol  = c("alcohol","alko_unit0","alko_class0","alko_binge",
               "alcohol_units","drinks_per_week","alcohol_consumption"),
  physical_activity = c("FYSAKT0","physical_activity","PA","pa",
                        "exercise","stepKondi","stepIlt"),
  diabetes  = c("diabetes","T2D_NGT","T2D_NFG","glu_tol","diabetes1",
                "diabetes2","dm","DM","T2D","t2d"),
  hypertension = c("hypertension","HT_NT","HT_NT2","hyptreat",
                   "blood_pressure_treatment","bp_treated"),
  liptreat  = c("liptreat","lipid_treatment","statin","cholesterol_treatment"),
  instreat  = c("instreat","insulin_treatment","insulin_therapy"),
  pulsetreat = c("pulsetreat","pulse_treatment","beta_blocker"),

  ## =========================================================
  ## Sweat biomarkers
  ## =========================================================
  sweat           = c("sweat","sweat_markers"),
  sweat_chloride  = c("sweat_chloride"),
  sweat_Na        = c("sweat_Na"),
  sweat_K         = c("sweat_K"),
  sweat_lactate   = c("sweat_lactate"),
  sweat_rate_bsa       = c("sweat_rate_bsa"),
  sweat_rate_duration  = c("sweat_rate_duration"),

  ## =========================================================
  ## Saliva biomarkers
  ## =========================================================
  saliva        = c("saliva","saliva_markers"),
  saliva_amylase = c("saliva_amylase","amylase","salivary_amylase"),
  saliva_glucose = c("saliva_glucose"),
  saliva_cort1   = c("saliva_cort1","cort1"),
  saliva_cort2   = c("saliva_cort2","cort2"),
  saliva_cort3   = c("saliva_cort3","cort3"),

  ## =========================================================
  ## Tracer / metabolic flux markers
  ## =========================================================
  tracer_dxa_is  = c("tracer_dxa_is","insulin_tracer_dxa"),
  rate_glycerol  = c("rate_glycerol","glycerol_fm"),
  rate_palmitate = c("rate_palmitate","palmitate_fm"),

  ## =========================================================
  ## Tryptophan-kynurenine pathway
  ## =========================================================
  tryptophan    = c("tryptophan","Trp_uM","tryptophan_umolL","Trp","trp"),
  kynurenine    = c("kynurenine","Kyn_nM","kynurenine_nmolL","Kyn","kyn"),
  kyn_trp_ratio = c("kyn_trp","kyn_trp_ratio","KTR","ktr"),
  Tyr           = c("Tyr","tyr","tyrosine","Tyrosine"),
  Phe           = c("Phe","phe","phenylalanine","Phenylalanine"),
  Tyr_Phe_Ratio = c("Tyr_Phe_Ratio","tyr_phe","TyrPhe"),

  ## =========================================================
  ## Metabolic / cardio risk scores (derived columns)
  ## =========================================================
  ASCVD        = c("ASCVD","cvd_risk_ascvd","ascvd_10yr"),
  QRISK3       = c("QRISK3","qrisk3","QRISK3_score","q_risk3"),
  PooledCohort = c("PooledCohort","pooled_cohort"),
  CVrisk       = c("CVrisk","RiskScorescvd","score2","SCORE2"),

  ## =========================================================
  ## Allostatic load & composite indices
  ## =========================================================
  allostatic_load = c("allostatic_load","AllostaticLoad","AL_score"),

  ## =========================================================
  ## iAge / inflammatory clock
  ## =========================================================
  iAge = c("iAge","iage","inflammatory_age_clock","iAge_score")
)
}
