
#' Calculate fasting-based insulin sensitivity indices
#'
#' @description
#' Compute 10 fasting indices from glucose (mmol/L) and insulin (pmol/L):
#' Fasting_inv, Raynaud, HOMA_IR_inv, FIRI, QUICKI, Belfiore_basal, Ig_ratio_basal,
#' Isi_basal, Bennett, HOMA_IR_rev_inv. Units converted internally:
#' G0_mg = G0*18 (mg/dL), I0_u = I0/6 (muU/mL).
#'
#' @param data Data frame with required inputs.
#' @param col_map Named list mapping required keys:
#'   - G0: fasting glucose (mmol/L)
#'   - I0: fasting insulin (pmol/L)
#' @param normalize One of: "none","z","inverse","range","robust".
#' @param na_action One of:
#'   - "keep"  (retain all rows; indices become NA where inputs missing/non-finite)
#'   - "omit"  (drop rows with any missing/non-finite required inputs)
#'   - "error" (abort if any required input is missing/non-finite)
#'   - "warn"  (emit a warning for rows with missing inputs, then keep them)

#' @param verbose Logical; if `TRUE` (default), prints column mapping, the list
#'   of indices being computed, and a per-column results summary.
#'
#' @return Tibble with 10 columns (indices listed above). If an ID column is
#'   detected in `data` (e.g. `id`, `IID`, `participant_id`), it is prepended
#'   as the first output column.
#' @references
#' \insertRef{matthews1985homa}{HealthMarkers}
#' \insertRef{katz2000quicki}{HealthMarkers}
#' \insertRef{raynaud1999fasting}{HealthMarkers}
#' \insertRef{avignon1999ogtt}{HealthMarkers}
#' \insertRef{belfiore1998insulin}{HealthMarkers}
#' \insertRef{sluiter1976gtolerance}{HealthMarkers}
#' \insertRef{hanson2000evaluation}{HealthMarkers}
#' \insertRef{anderson1995exploration}{HealthMarkers}
#'
#' @examples
#' # Minimal example (units: G0 in mmol/L, I0 in pmol/L)
#' df <- data.frame(G0 = c(5.2, 6.1, 4.8), I0 = c(60, 120, 80))
#' res <- fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"))
#' head(res)
#'
#' # With NA handling
#' df2 <- data.frame(G0 = c(5.0, NA), I0 = c(90, 150))
#' fasting_is(df2, col_map = list(G0 = "G0", I0 = "I0"), na_action = "keep")
#' @export
fasting_is <- function(
  data,
  col_map     = NULL,
  normalize   = c("none", "z", "inverse", "range", "robust"),
  na_action   = c("keep", "omit", "error", "warn"),
  verbose     = TRUE
) {
  data_name    <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name     <- "fasting_is"
  .hm_log_input(data, data_name, fn_name, verbose)
  allowed_norm <- c("none", "z", "inverse", "range", "robust")
  if (length(normalize) == 1L && !normalize %in% allowed_norm) {
    rlang::abort(
      sprintf("`normalize` must be one of: %s", paste(allowed_norm, collapse = ", ")),
      class = "healthmarkers_fi_error_normalize"
    )
  }
  normalize <- match.arg(normalize, allowed_norm)
  na_action <- match.arg(na_action)

  # Explicit input validation
  if (!is.data.frame(data)) {
    rlang::abort("fasting_is(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_fi_error_data_type")
  }

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)
  req_keys <- c("G0","I0")
  cm      <- .hm_build_col_map(data, col_map, req_keys, fn = "fasting_is")
  data    <- cm$data
  col_map <- cm$col_map
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("fasting_is(): `col_map` must be a named list with entries for G0 and I0.",
                 class = "healthmarkers_fi_error_colmap_type")
  }
  missing_keys <- setdiff(req_keys, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("fasting_is(): missing col_map entries for: ", paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_fi_error_missing_map"
    )
  }
  req_cols <- unname(unlist(col_map[req_keys], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("fasting_is(): missing required columns in data: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_fi_error_missing_columns"
    )
  }

  # --- Verbose: col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: computing markers list
  if (isTRUE(verbose)) {
    indices <- c("Fasting_inv","Raynaud","HOMA_IR_inv","FIRI","QUICKI",
                 "Belfiore_basal","Ig_ratio_basal","Isi_basal","Bennett","HOMA_IR_rev_inv")
    status <- vapply(indices, function(m) sprintf("  %-20s [G0, I0]", m), character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }

  # Coerce to numeric; warn if NAs introduced; non-finite -> NA
  for (nm in req_keys) {
    cn <- col_map[[nm]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced.", cn),
                    class = "healthmarkers_fi_warn_na_coercion")
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  G0 <- data[[col_map$G0]]
  I0 <- data[[col_map$I0]]

  # NA policy
  rows_with_na <- is.na(G0) | is.na(I0)
  if (na_action == "error" && any(rows_with_na)) {
    rlang::abort("fasting_is(): missing/non-finite inputs with na_action='error'.",
                 class = "healthmarkers_fi_error_missing")
  } else if (na_action == "warn" && any(rows_with_na)) {
    rlang::warn(sprintf("fasting_is(): %d row(s) have missing G0 or I0; results will be NA for those rows.",
                        sum(rows_with_na)),
                class = "healthmarkers_fi_warn_na")
  } else if (na_action == "omit" && any(rows_with_na)) {
    keep <- !rows_with_na
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep)),
                level = "inform")
    G0 <- G0[keep]; I0 <- I0[keep]
  }

  hm_inform(sprintf("%s(): converting units (mmol/L->mg/dL; pmol/L->muU/mL)", fn_name), level = "debug")

  G0_mg <- G0 * 18
  I0_u  <- I0 / 6

  hm_inform(sprintf("%s(): computing indices", fn_name), level = "debug")

  lg <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log(y))
    out[!is.finite(out)] <- NA_real_
    out
  }
  sdiv <- function(a, b) {
    z <- a / b
    z[!is.finite(z)] <- NA_real_
    z
  }

  out <- tibble::tibble(
    Fasting_inv     = -I0_u,
    Raynaud         = sdiv(40, I0_u),
    HOMA_IR_inv     = -sdiv(G0_mg * I0_u, 22.5),
    FIRI            =  sdiv(G0_mg * I0_u, 25),
    QUICKI          = sdiv(1, lg(G0_mg) + lg(I0_u)),
    Belfiore_basal  = sdiv(2, (I0_u * G0_mg) + 1),
    Ig_ratio_basal  = -sdiv(I0_u, G0_mg),
    Isi_basal       = sdiv(10000, G0_mg * I0_u),
    Bennett         = sdiv(1, lg(I0_u) * lg(G0_mg)),
    HOMA_IR_rev_inv = -sdiv(I0_u * G0_mg, 405)
  )

  # Inline normalization
  if (normalize != "none") {
    normalize_vec <- function(x, method) {
      v <- as.numeric(x)
      nm <- is.na(v)
      v2 <- v[!nm]
      if (method == "z") {
        if (length(v2) < 2) return(rep(NA_real_, length(v)))
        mu <- mean(v2); sdv <- stats::sd(v2)
        if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(v)))
        (v - mu) / sdv
      } else if (method == "range") {
        if (length(v2) < 2) return(rep(NA_real_, length(v)))
        mn <- min(v2); mx <- max(v2)
        if (!is.finite(mx - mn) || mx - mn == 0) return(rep(NA_real_, length(v)))
        (v - mn) / (mx - mn)
      } else if (method == "inverse") {
        res <- 1 / v
        res[!is.finite(res)] <- NA_real_
        res
      } else if (method == "robust") {
        if (length(v2) < 2) return(rep(NA_real_, length(v)))
        med <- stats::median(v2, na.rm = TRUE)
        madv <- stats::mad(v2, center = med, constant = 1.4826, na.rm = TRUE)
        if (!is.finite(madv) || madv == 0) return(rep(NA_real_, length(v)))
        (v - med) / madv
      } else {
        v
      }
    }
    out[] <- lapply(out, normalize_vec, method = normalize)
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

