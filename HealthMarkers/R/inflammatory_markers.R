#' Compute inflammatory indices (classic, eosinophil, or both)
#'
#' Panels:
#' - classic: NLR, PLR, LMR, dNLR, SII, SIRI, AISI, CRP_category
#' - eos:     NLR, PLR, LMR, NER, SII, SIRI, PIV, CLR, CAR, PCR, mGPS, ESR (if mapped)
#' - both:    union of classic and eos panels
#'
#' Derived markers:
#' - NLR  = neutrophils / lymphocytes
#' - PLR  = platelets / lymphocytes
#' - LMR  = lymphocytes / monocytes
#' - dNLR = neutrophils / (WBC - neutrophils) when WBC available
#' - SII  = platelets * neutrophils / lymphocytes
#' - SIRI = neutrophils * monocytes / lymphocytes
#' - AISI = neutrophils * monocytes * platelets / lymphocytes
#' - CRP_category: "low" (<1 mg/L), "moderate" (1-3 mg/L), "high" (>3 mg/L) when CRP available
#' - Eosinophil-panel extras: NER = neutrophils / eosinophils; PIV = platelets * neutrophils * monocytes / lymphocytes;
#'   CLR = CRP/lymphocytes; CAR = CRP/albumin; PCR = platelets/CRP; mGPS (CRP, albumin); ESR passthrough.
#'
#' Note:
#' - These outputs are deterministic algebraic indices computed from the mapped
#'   laboratory variables. They are intended for feature engineering and descriptive
#'   analyses, not as standalone diagnosis/prognosis tools.
#' - References below document commonly used index definitions or interpretation
#'   conventions directly used in this implementation.
#'
#' @param data data.frame or tibble
#' @param col_map named list mapping keys to column names in `data`.
#'   Keys: neutrophils, lymphocytes, monocytes, platelets, WBC, CRP, albumin, eosinophils, ESR.
#' @param panel one of c("auto","classic","eos","both"). "auto" uses presence of eosinophils key.
#' @param na_action one of c("keep","omit","error"). Default "keep" propagates NA
#'   in outputs where inputs are missing.
#' @param verbose logical; if `TRUE` (default), prints column mapping, optional input
#'   availability, physiological range information (informational only), the list of
#'   markers being computed, and a results summary.
#' @return tibble with selected inflammatory indices, with ID column prepended if
#'   detected (e.g. `id`, `IID`, `participant_id`).
#'
#' @examples
#' # Quick smoke-test
#' df <- data.frame(neutrophils = 4, lymphocytes = 2, monocytes = 0.5,
#'                  platelets = 200, WBC = 7, CRP = 2.5)
#' inflammatory_markers(df, panel = "classic", na_action = "keep", verbose = FALSE)
#'
#' \donttest{
#' df <- data.frame(
#'   neutrophils = c(4, 2),
#'   lymphocytes = c(2, 0),
#'   monocytes   = c(0.5, 0.3),
#'   platelets   = c(200, 150),
#'   WBC         = c(7, 4.5),
#'   CRP         = c(2.5, 0.8),
#'   albumin     = c(40, 42),
#'   eosinophils = c(0.2, 0.1),
#'   ESR         = c(12, 15)
#' )
#' cm <- list(
#'   neutrophils = "neutrophils", lymphocytes = "lymphocytes", monocytes = "monocytes",
#'   platelets = "platelets", WBC = "WBC", CRP = "CRP", albumin = "albumin",
#'   eosinophils = "eosinophils", ESR = "ESR"
#' )
#' classic_cm <- cm; classic_cm$eosinophils <- NULL; classic_cm$ESR <- NULL
#' inflammatory_markers(df, classic_cm, panel = "classic", na_action = "keep")
#' inflammatory_markers(df, cm, panel = "eos", na_action = "keep", verbose = TRUE)
#' }
#'
#' @references
#' \insertRef{zahorec2001}{HealthMarkers}
#' \insertRef{hu2014sii}{HealthMarkers}
#' \insertRef{qi2016siri}{HealthMarkers}
#' \insertRef{proctor2011mgps}{HealthMarkers}
#' \insertRef{pearson2003markers}{HealthMarkers}
#' @export
inflammatory_markers <- function(data, col_map = NULL,
                                 panel      = c("auto", "classic", "eos", "both"),
                                 na_action  = c("keep", "omit", "error"),
                                 verbose    = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "inflammatory_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  panel     <- match.arg(panel)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    rlang::abort(sprintf("%s(): `data` must be a data.frame or tibble.", fn_name),
                 class = "healthmarkers_inflammatory_markers_error_data_type")
  }

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)

  all_im_keys <- c("neutrophils","lymphocytes","monocytes","platelets","WBC",
    "CRP","albumin","eosinophils","ESR")
  cm      <- .hm_build_col_map(data, col_map, all_im_keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map
  if (is.null(col_map) || !is.list(col_map)) col_map <- list()

  # --- Verbose: col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  if (!is.list(col_map) || (!is.null(names(col_map)) && any(!nzchar(names(col_map))))) {
    rlang::abort("inflammatory_markers(): `col_map` must be a named list.",
                 class = "healthmarkers_inflammatory_markers_error_map_type")
  }

  has_eos_key <- "eosinophils" %in% names(col_map)
  if (panel == "auto") panel <- if (has_eos_key) "eos" else "classic"

  req_keys <- switch(panel,
    classic = c("neutrophils","lymphocytes"),
    eos     = c("neutrophils","lymphocytes","monocytes","platelets","CRP"),
    both    = c("neutrophils","lymphocytes"),
    c("neutrophils","lymphocytes")
  )
  missing_keys <- setdiff(req_keys, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("inflammatory_markers(): missing col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_inflammatory_markers_error_missing_map"
    )
  }
  is_empty_map <- function(v) {
    is.null(v) || length(v) == 0L ||
      (is.atomic(v) && length(v) == 1L && (is.na(v) || identical(v, "") || !nzchar(as.character(v))))
  }
  empty_keys <- names(Filter(is_empty_map, col_map))
  if (length(empty_keys)) {
    rlang::abort(
      paste0("inflammatory_markers(): `col_map` has empty mapping for: ",
             paste(empty_keys, collapse = ", ")),
      class = "healthmarkers_inflammatory_markers_error_missing_map"
    )
  }
  if (panel == "eos" && ("CRP" %in% names(col_map)) && !("albumin" %in% names(col_map))) {
    rlang::abort("inflammatory_markers(): missing col_map entries for: albumin",
                 class = "healthmarkers_inflammatory_markers_error_missing_map")
  }

  # --- Verbose: optional inputs
  if (isTRUE(verbose)) {
    all_keys  <- c("neutrophils","lymphocytes","monocytes","platelets",
                   "WBC","CRP","albumin","eosinophils","ESR")
    get_col_v <- function(key) if (key %in% names(col_map)) as.character(col_map[[key]])[1] else NA_character_
    has_col_v <- function(k) { cn <- get_col_v(k); is.character(cn) && !is.na(cn) && cn %in% names(data) }
    present_all <- all_keys[vapply(all_keys, has_col_v, logical(1))]
    missing_all <- setdiff(all_keys, present_all)
    lines <- sprintf("%s(): optional inputs (panel = %s)", fn_name, panel)
    if (length(present_all))
      lines <- c(lines, sprintf("  present:  %s", paste(present_all, collapse = ", ")))
    if (length(missing_all))
      lines <- c(lines, sprintf("  missing:  %s", paste(missing_all, collapse = ", ")))
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  supported_keys <- c("neutrophils","lymphocytes","monocytes","platelets","WBC","CRP","albumin","eosinophils","ESR")
  get_col <- function(key) if (key %in% names(col_map)) as.character(col_map[[key]])[1] else NA_character_
  map_cols <- vapply(supported_keys, get_col, character(1)); names(map_cols) <- supported_keys
  has_col <- function(cn) is.character(cn) && length(cn) == 1L && !is.na(cn) && cn %in% names(data)
  avail <- vapply(map_cols, has_col, logical(1))

  # Coerce available columns to numeric; non-finite -> NA
  for (k in names(avail)[avail]) {
    cn <- map_cols[[k]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("inflammatory_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # --- Verbose: physiological range check (informational, values not altered)
  if (isTRUE(verbose)) {
    limits_info <- list(
      neutrophils = c(0, 30), lymphocytes = c(0.2, 15), monocytes = c(0, 5),
      eosinophils = c(0, 3),  platelets   = c(0, 1000), WBC = c(0, 50),
      CRP = c(0, 300),        albumin     = c(10, 60)
    )
    ex_details <- character(0)
    for (k in names(limits_info)) {
      if (!avail[[k]]) next
      x   <- data[[map_cols[[k]]]]
      rng <- limits_info[[k]]
      nb  <- sum(is.finite(x) & (x < rng[1] | x > rng[2]), na.rm = TRUE)
      if (nb > 0L)
        ex_details <- c(ex_details, sprintf("  %s: %d value(s) outside plausible range", k, nb))
    }
    if (length(ex_details))
      hm_inform(
        paste(c(sprintf("%s(): range note (informational, values not altered):", fn_name),
                ex_details), collapse = "\n"),
        level = "inform"
      )
  }

  # --- Verbose: computing markers
  if (isTRUE(verbose)) {
    panel_markers <- switch(panel,
      classic = "NLR, PLR, LMR, dNLR, SII, SIRI, AISI, CRP_category",
      eos     = "NLR, PLR, LMR, NER, SII, SIRI, PIV, CLR, CAR, PCR, mGPS, ESR",
      both    = "NLR, PLR, LMR, NER, SII, SIRI, PIV, CLR, CAR, PCR, mGPS, ESR, dNLR, AISI, CRP_category"
    )
    hm_inform(
      sprintf("%s(): computing markers: %s", fn_name, panel_markers),
      level = "inform"
    )
 }

  g <- function(key) data[[map_cols[[key]]]]
  n <- nrow(data)
  out <- tibble::tibble(.rows = n)
  dz_count <- 0L
  safe_div <- function(num, den) {
    res <- num / den
    dz_count <<- dz_count + sum(is.finite(den) & den == 0, na.rm = TRUE)
    res
  }
  used_keys <- character(0)

  add_classic <- function() {
    out$NLR  <<- if (all(avail[c("neutrophils","lymphocytes")])) safe_div(g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$PLR  <<- if (all(avail[c("platelets","lymphocytes")]))   safe_div(g("platelets"), g("lymphocytes"))   else rep(NA_real_, n)
    out$LMR  <<- if (all(avail[c("lymphocytes","monocytes")]))   safe_div(g("lymphocytes"), g("monocytes"))   else rep(NA_real_, n)
    out$dNLR <<- if (all(avail[c("neutrophils","WBC")]))         safe_div(g("neutrophils"), (g("WBC") - g("neutrophils"))) else rep(NA_real_, n)
    out$SII  <<- if (all(avail[c("platelets","neutrophils","lymphocytes")])) safe_div(g("platelets") * g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$SIRI <<- if (all(avail[c("neutrophils","monocytes","lymphocytes")])) safe_div(g("neutrophils") * g("monocytes"), g("lymphocytes")) else rep(NA_real_, n)
    out$AISI <<- if (all(avail[c("neutrophils","monocytes","platelets","lymphocytes")])) safe_div(g("neutrophils") * g("monocytes") * g("platelets"), g("lymphocytes")) else rep(NA_real_, n)
    used_keys <<- union(used_keys, c("neutrophils","lymphocytes","platelets","monocytes","WBC"))
    crp_cat <- rep(NA_character_, n)
    if (avail[["CRP"]]) {
      crp <- g("CRP")
      crp_cat <- ifelse(is.na(crp), NA_character_,
                 ifelse(crp < 1, "low", ifelse(crp <= 3, "moderate", "high")))
      used_keys <<- union(used_keys, "CRP")
    }
    out$CRP_category <<- factor(crp_cat, levels = c("low","moderate","high"), ordered = TRUE)
  }

  add_eos <- function() {
    out$NLR  <<- if (all(avail[c("neutrophils","lymphocytes")])) safe_div(g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$PLR  <<- if (all(avail[c("platelets","lymphocytes")]))   safe_div(g("platelets"), g("lymphocytes"))   else rep(NA_real_, n)
    out$LMR  <<- if (all(avail[c("lymphocytes","monocytes")]))   safe_div(g("lymphocytes"), g("monocytes"))   else rep(NA_real_, n)
    out$NER  <<- if (all(avail[c("neutrophils","eosinophils")])) safe_div(g("neutrophils"), g("eosinophils")) else rep(NA_real_, n)
    out$SII  <<- if (all(avail[c("platelets","neutrophils","lymphocytes")])) safe_div(g("platelets") * g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$SIRI <<- if (all(avail[c("neutrophils","monocytes","lymphocytes")])) safe_div(g("neutrophils") * g("monocytes"), g("lymphocytes")) else rep(NA_real_, n)
    out$PIV  <<- if (all(avail[c("platelets","neutrophils","monocytes","lymphocytes")])) safe_div(g("platelets") * g("neutrophils") * g("monocytes"), g("lymphocytes")) else rep(NA_real_, n)
    out$CLR  <<- if (all(avail[c("CRP","lymphocytes")]))         safe_div(g("CRP"), g("lymphocytes")) else rep(NA_real_, n)
    out$CAR  <<- if (all(avail[c("CRP","albumin")]))             safe_div(g("CRP"), g("albumin")) else rep(NA_real_, n)
    out$PCR  <<- if (all(avail[c("platelets","CRP")]))           safe_div(g("platelets"), g("CRP")) else rep(NA_real_, n)
    out$mGPS <<- if (all(avail[c("CRP","albumin")])) {
                   crp <- g("CRP"); alb <- g("albumin")
                   as.integer(ifelse(is.na(crp), NA_integer_,
                              ifelse(crp <= 10, 0L, ifelse(alb >= 35, 1L, 2L))))
                 } else rep(NA_integer_, n)
    if (avail[["ESR"]]) out$ESR <<- as.numeric(g("ESR"))
    used_keys <<- union(used_keys, c("neutrophils","lymphocytes","platelets","monocytes","eosinophils","CRP","albumin","ESR"))
  }

  if (panel == "classic") {
    add_classic()
    out <- out[, c("NLR","PLR","LMR","dNLR","SII","SIRI","AISI","CRP_category"), drop = FALSE]
  } else if (panel == "eos") {
    add_eos()
    ord <- c("NLR","PLR","LMR","NER","SII","SIRI","PIV","CLR","CAR","PCR","mGPS", if ("ESR" %in% names(out)) "ESR")
    out <- out[, ord, drop = FALSE]
  } else { # both
    add_eos(); add_classic()
    ord <- c("NLR","PLR","LMR","NER","SII","SIRI","PIV","CLR","CAR","PCR","mGPS","ESR","dNLR","AISI","CRP_category")
    ord <- intersect(ord, names(out))
    out <- out[, ord, drop = FALSE]
  }

  # NA policy over used inputs
  used_cols <- map_cols[intersect(unique(used_keys), names(map_cols))]
  used_cols <- used_cols[!is.na(used_cols)]
  cc <- if (length(used_cols)) stats::complete.cases(data[, used_cols, drop = FALSE]) else rep(TRUE, n)

  na_action_eff <- na_action
  if (na_action_eff == "error" && any(!cc)) {
    rlang::abort("inflammatory_markers(): required columns contain missing or non-finite values (na_action='error').",
                 class = "healthmarkers_inflammatory_markers_error_missing_values")
  } else if (na_action_eff == "omit") {
    out <- out[cc, , drop = FALSE]
  }

  if (dz_count > 0L) rlang::warn("inflammatory_markers(): zero denominators detected.")
  out <- tibble::as_tibble(out)

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

