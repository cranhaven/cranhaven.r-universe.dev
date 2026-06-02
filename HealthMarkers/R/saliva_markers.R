
#' Calculate saliva-based stress & glycemic markers
#'
#' Computes:
#'  - log_cortisol_wake (log-transformed waking cortisol)
#'  - CAR_AUC           (Cortisol Awakening Response, trapezoidal AUC over 0-60 min by default)
#'  - log_amylase       (log-transformed salivary alpha-amylase)
#'  - saliva_glucose    (raw salivary glucose)
#'
#' Inputs are validated, missingness handled via `na_action`, logs made safe
#' (<= 0 -> NA), and optional extremes scan/cap is available.
#'
#' @param data A data.frame or tibble with salivary markers.
#' @param col_map Named list mapping required inputs. Defaults assume same names:
#'   - cort1   -> "saliva_cort1" (nmol/L at wake)
#'   - cort2   -> "saliva_cort2" (nmol/L ~30 min)
#'   - cort3   -> "saliva_cort3" (nmol/L ~60 min)
#'   - amylase -> "saliva_amylase" (U/mL)
#'   - glucose -> "saliva_glucose" (mg/dL)
#' @param verbose Logical; if `TRUE` (default), prints column mapping, input
#'   availability, physiological range information (informational only, values
#'   not altered), the list of markers being computed with their inputs, and a
#'   per-column results summary.
#' @param na_action One of `c("keep","omit","error")` for required-input NA handling. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness diagnostics (debug). Default 0.2.
#' @param times Numeric vector of sampling times (minutes) for CAR AUC. Must align with cort1/2/3. Default c(0,30,60).
#'
#' @return A tibble with columns:
#'   - `log_cortisol_wake`
#'   - `CAR_AUC`
#'   - `log_amylase`
#'   - `saliva_glucose`
#'
#'   If an ID column is detected in `data` (e.g. `id`, `IID`, `participant_id`),
#'   it is prepended as the first output column.
#'
#' @examples
#' df <- tibble::tibble(
#'   saliva_cort1    = 12.5,
#'   saliva_cort2    = 18.0,
#'   saliva_cort3    = 16.2,
#'   saliva_amylase  = 85,
#'   saliva_glucose  = 4.2
#' )
#' saliva_markers(df)  # uses default col_map
#'
#' @note
#' `log_cortisol_wake` and `log_amylase` use the **natural logarithm** (`log()`).
#' `CAR_AUC` is the trapezoidal area under the cortisol-time curve (Pruessner
#' et al. 2003, AUC with respect to ground). `saliva_glucose` is a
#' **pass-through** column; no formula is applied.
#'
#' @references
#' \insertRef{pruessner2003auc}{HealthMarkers}
#' \insertRef{kirschbaum1994cortisol}{HealthMarkers} (salivary cortisol methods; background)
#' \insertRef{clow2004car}{HealthMarkers} (CAR methodological review; background)
#' \insertRef{nater2009amylase}{HealthMarkers} (salivary alpha-amylase SNS biomarker; background)
#' \insertRef{scales1987salgluc}{HealthMarkers} (salivary glucose application; pass-through, no formula)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
saliva_markers <- function(data,
                           col_map      = NULL,
                           verbose      = TRUE,
                           na_action    = c("keep", "omit", "error"),
                           na_warn_prop = 0.2,
                           times        = c(0, 30, 60)) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "saliva_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)
  if (!is.data.frame(data)) {
    rlang::abort("saliva_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_saliva_error_data_type")
  }

  # --- Detect and preserve ID column
  id_col <- .hm_detect_id_col(data)

  # HM-CS v2: standardized validation
  required_keys <- c("cort1","cort2","cort3","amylase","glucose")
  cm      <- .hm_build_col_map(data, col_map, required_keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map
  missing_keys <- setdiff(required_keys, names(col_map))
  if (length(missing_keys)) {
    # Graceful NA only when nothing provided and inference found nothing
    if (length(cm$user_keys) == 0L && length(cm$inferred_keys) == 0L && length(col_map) == 0L) {
      return(tibble::tibble(
        log_cortisol_wake = rep(NA_real_, nrow(data)),
        CAR_AUC           = rep(NA_real_, nrow(data)),
        log_amylase       = rep(NA_real_, nrow(data)),
        saliva_glucose    = rep(NA_real_, nrow(data))
      ))
    }
    rlang::abort(
      paste0("saliva_markers(): missing required col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_saliva_error_missing_map"
    )
  }
  hm_validate_inputs(data, col_map, required_keys = required_keys, fn = fn_name)

  # Ensure mapped columns exist
  mapped <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("saliva_markers(): missing columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_saliva_error_missing_columns"
    )
  }

  # Coerce to numeric where needed and warn on NAs introduced; set non-finite -> NA
  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced <- sum(is.na(new) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("saliva_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug)
  for (cn in mapped) {
    x <- data[[cn]]; n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("saliva_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # --- Verbose: column mapping
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: optional inputs / availability
  if (isTRUE(verbose)) {
    avail_keys  <- required_keys[required_keys %in% names(col_map)]
    absent_keys <- setdiff(required_keys, avail_keys)
    lines <- sprintf("%s(): optional inputs", fn_name)
    if (length(avail_keys))
      lines <- c(lines, sprintf("  present:  %s", paste(avail_keys, collapse = ", ")))
    if (length(absent_keys))
      lines <- c(lines, sprintf("  missing:  %s", paste(absent_keys, collapse = ", ")))
    hm_inform(paste(lines, collapse = "\n"), level = "inform")
  }

  # NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("saliva_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_saliva_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose))
      hm_inform(sprintf("%s(): omitting %d rows with NA in required inputs", fn_name, sum(!keep)),
                level = "inform")
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      log_cortisol_wake = numeric(),
      CAR_AUC           = numeric(),
      log_amylase       = numeric(),
      saliva_glucose    = numeric()
    ))
  }

  # --- Verbose: physiological range check (informational, values not altered)
  if (isTRUE(verbose)) {
    default_ranges <- list(
      cort1   = c(0, 2000), cort2 = c(0, 2000), cort3 = c(0, 2000),
      amylase = c(0, 50000), glucose = c(0, 1000)
    )
    flagged_details <- character(0)
    for (key in required_keys) {
      rng <- default_ranges[[key]]
      cn  <- col_map[[key]]
      if (is.null(rng) || is.null(cn) || !(cn %in% names(data))) next
      bad <- sum(is.finite(data[[cn]]) & (data[[cn]] < rng[1] | data[[cn]] > rng[2]), na.rm = TRUE)
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
      log_cortisol_wake = c("cort1"),
      CAR_AUC           = c("cort1", "cort2", "cort3"),
      log_amylase       = c("amylase"),
      saliva_glucose    = c("glucose")
    )
    avail_keys2 <- names(col_map)
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

  # Helpers
  safe_log <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log(y))
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Extract mapped inputs
  c1 <- data[[col_map$cort1]]
  c2 <- data[[col_map$cort2]]
  c3 <- data[[col_map$cort3]]
  amy <- data[[col_map$amylase]]
  glu <- data[[col_map$glucose]]

  # Compute outputs
  log_cortisol_wake <- safe_log(c1)
  log_amylase       <- safe_log(amy)

  # Validate times
  if (!(is.numeric(times) && length(times) == 3L && isTRUE(all(diff(times) >= 0)))) {
    rlang::abort("saliva_markers(): `times` must be a numeric vector of length 3 in non-decreasing order.",
                 class = "healthmarkers_saliva_error_times")
  }
  cort_mat <- cbind(c1, c2, c3)
  dt <- diff(times)
  CAR_AUC <- apply(cort_mat, 1, function(x) {
    if (any(!is.finite(x))) return(NA_real_)
    sum((x[-length(x)] + x[-1]) / 2 * dt)
  })

  out <- tibble::tibble(
    log_cortisol_wake = as.numeric(log_cortisol_wake),
    CAR_AUC           = as.numeric(CAR_AUC),
    log_amylase       = as.numeric(log_amylase),
    saliva_glucose    = as.numeric(glu)
  )

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

