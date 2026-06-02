
#' Adipose insulin sensitivity indices (QUICKI, VAI, LAP, TyG, TG/HDL, Belfiore)
#'
#' @description
#' Computes adipose-related insulin sensitivity/resistance indices from fasting inputs.
#' Expected input units (converted internally):
#' - Glucose G0 mmol/L -> mg/dL (* 18)
#' - Insulin I0 pmol/L -> muU/mL (/ 6)
#' - TG mmol/L -> mg/dL (* 88.57)
#' - HDL mmol/L -> mg/dL (* 38.67)
#'
#' Reported indices (higher magnitude of negative "_inv" values implies worse adipose IR):
#' - Revised_QUICKI = 1 / (log10(I0 (muU/mL)) + log10(G0 (mg/dL)) + log10(FFA (mmol/L)))
#' - VAI (sex-specific; inverted as VAI_*_inv so larger negative = worse)
#' - TG_HDL_C_inv = -(TG/HDL) in mg/dL
#' - TyG_inv = -ln(TG (mg/dL) * G0 (mg/dL) / 2)
#' - LAP (sex-specific; inverted)
#' - McAuley_index = exp(2.63 - 0.28 ln(I0 (muU/mL)) - 0.31 ln(TG (mmol/L)))
#' - Adipo_inv = -(FFA * I0 (muU/mL))
#' - Belfiore_inv_FFA = - 2 / (I0 (muU/mL) * FFA + 1)
#'
#' **Inversion Note**: Most indices (VAI, LAP, TyG, TG/HDL, Adipo, Belfiore) are algebraically
#' inverted from their original insulin RESISTANCE definitions so that more negative values
#' consistently indicate worse adipose insulin sensitivity. Revised_QUICKI and McAuley_index
#' are retained in their original orientation (already sensitivity indices; higher = better).
#' See the vignette for detailed interpretation guidance.
#'
#' @param data Data frame or tibble with required columns mapped by `col_map`
#' @param col_map Named list mapping keys to columns: G0, I0, TG, HDL_c, FFA, waist, bmi
#' @param normalize One of c("none","z","inverse","range","robust"); default "none"
#' @param na_action One of c("keep","omit","error"); default "keep"
#' @param verbose Logical; if `TRUE` (default), prints column mapping, the list
#'   of indices being computed, and a per-column results summary.
#' @param ... Reserved
#'
#' @return A tibble with columns:
#' `Revised_QUICKI`, `VAI_Men_inv`, `VAI_Women_inv`, `TG_HDL_C_inv`, `TyG_inv`,
#' `LAP_Men_inv`, `LAP_Women_inv`, `McAuley_index`, `Adipo_inv`, `Belfiore_inv_FFA`.
#' If an ID column is detected in `data` (e.g. `id`, `IID`, `participant_id`), it
#' is prepended as the first output column.
#'
#' @references
#' \insertRef{katz2000quicki}{HealthMarkers}
#' \insertRef{amato2010vai}{HealthMarkers}
#' \insertRef{kahn2005lap}{HealthMarkers}
#' \insertRef{guerreroromero2018tyg}{HealthMarkers}
#' \insertRef{dobiasova2001atherogenic}{HealthMarkers}
#' \insertRef{belfiore1998insulin}{HealthMarkers}
#' \insertRef{raynaud1999fasting}{HealthMarkers}
#'
#' @examples
#' df <- tibble::tibble(
#'   G0 = c(5.2, 6.1),      # mmol/L
#'   I0 = c(60, 110),       # pmol/L
#'   TG = c(1.2, 1.8),      # mmol/L
#'   HDL_c = c(1.3, 1.0),   # mmol/L
#'   FFA = c(0.4, 0.6),     # mmol/L
#'   waist = c(85, 102),    # cm
#'   bmi = c(24, 31)        # kg/m^2
#' )
#' cm <- as.list(names(df)); names(cm) <- names(df)
#' out <- adipo_is(df, cm, verbose = FALSE, na_action = "keep")
#' @export
adipo_is <- function(data,
                     col_map = NULL,
                     normalize = "none",
                     na_action = c("keep","omit","error"),
                     verbose = TRUE,
                     ...) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "adipo_is"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)
  id_col    <- .hm_detect_id_col(data)

  # Validate normalize up front
  allowed_norm <- c("none","z","inverse","range","robust")
  if (!normalize %in% allowed_norm) {
    rlang::abort(
      sprintf("`normalize` must be one of: %s", paste(allowed_norm, collapse = ", ")),
      class = "healthmarkers_adipo_is_error_normalize_arg"
    )
  }

  # Centralized validation
  req <- c("G0","I0","TG","HDL_c","FFA","waist","bmi")

  cm      <- .hm_build_col_map(data, col_map, req, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # Emit test-aligned error before any generic helper
  nm <- names(col_map)
  if (is.null(nm)) nm <- character(0)
  missing_keys <- setdiff(req, nm)
  if (length(missing_keys)) {
    rlang::abort(
      paste0("adipo_is(): missing col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_adipo_is_error_missing_map"
    )
  }

  # Ensure mapped columns exist in data
  mapped_cols <- unname(unlist(col_map[req], use.names = FALSE))
  not_found <- setdiff(mapped_cols, names(data))
  if (length(not_found)) {
    rlang::abort(
      paste0("adipo_is(): columns not found in data: ", paste(not_found, collapse = ", ")),
      class = "healthmarkers_adipo_is_error_missing_columns"
    )
  }

  # Progress message — verbose col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: computing markers list
  if (isTRUE(verbose)) {
    indices <- list(
      c("Revised_QUICKI",   "G0, I0, FFA"),
      c("VAI_Men_inv",      "waist, bmi, TG, HDL_c"),
      c("VAI_Women_inv",    "waist, bmi, TG, HDL_c"),
      c("TG_HDL_C_inv",     "TG, HDL_c"),
      c("TyG_inv",          "TG, G0"),
      c("LAP_Men_inv",      "waist, TG"),
      c("LAP_Women_inv",    "waist, TG"),
      c("McAuley_index",    "I0, TG"),
      c("Adipo_inv",        "FFA, I0"),
      c("Belfiore_inv_FFA", "I0, FFA")
    )
    status <- vapply(indices, function(x) sprintf("  %-20s [%s]", x[1], x[2]), character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # Coerce to numeric when needed (warn once per column on NA introduction)
  for (key in req) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      intro <- sum(is.na(data[[cn]]) & !is.na(old))
      if (intro > 0) {
        rlang::warn(
          sprintf("adipo_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
          class = "healthmarkers_adipo_is_warn_na_coercion"
        )
      }
    }
  }

  # NA handling
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (any(any_na)) {
      rlang::abort(
        "adipo_is(): required inputs contain missing values (na_action='error').",
        class = "healthmarkers_adipo_is_error_missing_values"
      )
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep)),
                level = "inform")
    data <- data[keep, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      Revised_QUICKI = numeric(),
      VAI_Men_inv = numeric(),
      VAI_Women_inv = numeric(),
      TG_HDL_C_inv = numeric(),
      TyG_inv = numeric(),
      LAP_Men_inv = numeric(),
      LAP_Women_inv = numeric(),
      McAuley_index = numeric(),
      Adipo_inv = numeric(),
      Belfiore_inv_FFA = numeric()
    ))
  }

  # Extract and convert units
  # Raw mmol/L values kept separately for formulas that require them (VAI, LAP, McAuley)
  TG_raw  <- as.numeric(data[[col_map$TG]])      # mmol/L — for VAI, LAP, McAuley
  HDL_raw <- as.numeric(data[[col_map$HDL_c]])   # mmol/L — for VAI

  G0    <- as.numeric(data[[col_map$G0]]) * 18   # mmol/L -> mg/dL (TyG, Revised_QUICKI)
  I0    <- as.numeric(data[[col_map$I0]]) / 6    # pmol/L -> mU/L
  TG    <- TG_raw * 88.57                        # mmol/L -> mg/dL (TyG, TG_HDL_C_inv)
  HDL   <- HDL_raw * 38.67                       # mmol/L -> mg/dL (TG_HDL_C_inv)
  FFA   <- as.numeric(data[[col_map$FFA]])
  waist <- as.numeric(data[[col_map$waist]])
  bmi   <- as.numeric(data[[col_map$bmi]])

  # Safe helpers
  safe_div <- function(num, den) { out <- num / den; out[!is.finite(out)] <- NA_real_; out }
  safe_log <- function(x) { out <- rep(NA_real_, length(x)); ok <- is.finite(x) & x > 0; out[ok] <- log(x[ok]); out }
  safe_log10 <- function(x) { out <- rep(NA_real_, length(x)); ok <- is.finite(x) & x > 0; out[ok] <- log10(x[ok]); out }

  # Indices
  Revised_QUICKI <- safe_div(1, safe_log10(I0) + safe_log10(G0) + safe_log10(FFA))

  # VAI: reference constants (1.03, 1.31, 0.81, 1.52) are in mmol/L units (Amato 2010)
  VAI_Men_inv <- -(
    safe_div(waist, (39.68 + 1.88 * bmi)) *
      safe_div(TG_raw, 1.03) *
      safe_div(1.31, HDL_raw)
  )
  VAI_Women_inv <- -(
    safe_div(waist, (36.58 + 1.89 * bmi)) *
      safe_div(TG_raw, 0.81) *
      safe_div(1.52, HDL_raw)
  )

  TG_HDL_C_inv  <- -safe_div(TG, HDL)         # ratio: both converted to mg/dL, scale-invariant
  TyG_inv       <- -safe_log(TG * G0 / 2)     # TG and G0 in mg/dL (Guerrero-Romero 2018)
  LAP_Men_inv   <- -((waist - 65) * TG_raw)   # TG in mmol/L (Kahn 2005)
  LAP_Women_inv <- -((waist - 58) * TG_raw)   # TG in mmol/L
  McAuley_index <- exp(2.63 - 0.28 * safe_log(I0) - 0.31 * safe_log(TG_raw))  # TG in mmol/L (McAuley 2001)
  Adipo_inv <- -(FFA * I0)
  Belfiore_inv_FFA <- -safe_div(2, (I0 * FFA + 1))

  out <- tibble::tibble(
    Revised_QUICKI = as.numeric(Revised_QUICKI),
    VAI_Men_inv = as.numeric(VAI_Men_inv),
    VAI_Women_inv = as.numeric(VAI_Women_inv),
    TG_HDL_C_inv = as.numeric(TG_HDL_C_inv),
    TyG_inv = as.numeric(TyG_inv),
    LAP_Men_inv = as.numeric(LAP_Men_inv),
    LAP_Women_inv = as.numeric(LAP_Women_inv),
    McAuley_index = as.numeric(McAuley_index),
    Adipo_inv = as.numeric(Adipo_inv),
    Belfiore_inv_FFA = as.numeric(Belfiore_inv_FFA)
  )

  # Optional normalization
  if (normalize != "none" && exists("normalize_vec", where = asNamespace("HealthMarkers"))) {
    norm_fun <- getExportedValue("HealthMarkers", "normalize_vec")
    out[] <- lapply(out, function(x) do.call(norm_fun, list(x = x, method = normalize)))
  }

  # Results summary
  # --- Prepend ID column if detected
  if (!is.null(id_col)) {
    id_vec        <- data[[id_col]][seq_len(nrow(out))]
    out[[id_col]] <- id_vec
    out           <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out           <- tibble::as_tibble(out)
  }
  # --- Verbose: results summary
  hm_inform(
    level = if (isTRUE(verbose)) "inform" else "debug",
    msg   = hm_result_summary(out, fn_name)
  )
  out
}

