#' Compute atherogenic indices
#'
#' Calculates:
#' - AIP: Atherogenic Index of Plasma = log10(TG / HDL_c)
#' - CRI_I: Castelli Risk Index I = TC / HDL_c
#' - CRI_II: Castelli Risk Index II = LDL_c / HDL_c
#'
#' Behavior:
#' - Required keys: TG, HDL_c. Optional: TC, LDL_c.
#' - NA policy via `na_action`: "keep" (default), "omit" (drop rows with any NA in used lipids), "error".
#' - Extreme screening via `check_extreme` and `extreme_action` ("warn","cap","error","ignore","NA").
#'   Default bounds (mg/dL) used only for screening: TG (0, 10000), HDL_c (0, 1000), LDL_c (0, 10000), TC (0, 10000).
#'   Note: All indices are unitless ratios; units cancel in computations.
#' - Emits progress via `hm_inform()` when `verbose = TRUE` or when package option enables logs.
#'
#' @param data data.frame/tibble with lipid columns.
#' @param col_map named list mapping keys to columns, e.g. list(TG="TG", HDL_c="HDL_c", TC="TC", LDL_c="LDL_c").
#' @param na_action one of c("keep","omit","error").
#' @param normalize one of c("none","log10"). Reserved; AIP always uses log10(TG/HDL_c).
#' @param verbose Logical; if `TRUE` (default), prints column mapping, the list
#'   of indices being computed, and a per-column results summary.
#'
#' @return tibble with columns AIP, CRI_I, CRI_II. If an ID column is detected
#'   in `data` (e.g. `id`, `IID`, `participant_id`), it is prepended.
#'
#' @references
#' \insertRef{dobiasova2004aip}{HealthMarkers}
#' \insertRef{castelli1977framingham}{HealthMarkers}
#'
#' @examples
#' df <- tibble::tibble(
#'   TG = c(150, 200),
#'   HDL_c = c(50, 40),
#'   TC = c(200, 220),
#'   LDL_c = c(120, 150)
#' )
#' cm <- list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c")
#' atherogenic_indices(df, col_map = cm)
#'
#' @export
atherogenic_indices <- function(data,
                                col_map = NULL,
                                na_action = c("keep","omit","error"),
                                normalize = c("none","log10"),
                                verbose = TRUE) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name   <- "atherogenic_indices"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)
  id_col    <- .hm_detect_id_col(data)

  allowed_norm <- c("none","log10")
  if (length(normalize) == 0L) {
    rlang::abort("`normalize` must be one of: 'none', 'log10'",
                 class = "healthmarkers_atherogenic_indices_error_normalize")
  }
  normalize <- normalize[1L]
  if (!(normalize %in% allowed_norm)) {
    rlang::abort("`normalize` must be one of: 'none', 'log10'",
                 class = "healthmarkers_atherogenic_indices_error_normalize")
  }

  req <- c("TG", "HDL_c")
  opt <- c("TC", "LDL_c")

  cm      <- .hm_build_col_map(data, col_map, c(req, opt), fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map
  hm_validate_inputs(data, col_map, required_keys = req, fn = "atherogenic_indices")

  # Required columns presence
  req_cols <- unname(unlist(col_map[req], use.names = FALSE))

  # --- Verbose: col_map
  .hm_log_cols(cm, col_map, fn_name, verbose)

  # --- Verbose: computing markers list
  if (isTRUE(verbose)) {
    indices <- list(
      c("AIP",    "log10(TG / HDL_c)"),
      c("CRI_I",  "TC / HDL_c [if TC available]"),
      c("CRI_II", "LDL_c / HDL_c [if LDL_c available]")
    )
    status <- vapply(indices, function(x) sprintf("  %-10s [%s]", x[1], x[2]), character(1))
    hm_inform(
      paste(c(sprintf("%s(): computing markers:", fn_name), status), collapse = "\n"),
      level = "inform"
    )
  }
  missing_req <- setdiff(req_cols, names(data))
  if (length(missing_req)) {
    rlang::abort(
      paste0("atherogenic_indices(): missing required columns in data: ", paste(missing_req, collapse = ", ")),
      class = "healthmarkers_atherogenic_indices_error_missing_columns"
    )
  }

  # Optional columns present in data
  opt_cols <- unname(unlist(col_map[intersect(opt, names(col_map))], use.names = FALSE))
  opt_cols <- intersect(opt_cols, names(data))

  # Coerce used columns to numeric; NAs introduced are warned
  used_cols <- unique(c(req_cols, opt_cols))
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("atherogenic_indices(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced),
                    class = "healthmarkers_atherogenic_indices_warn_na_coercion")
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # NA handling across all used lipids
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("atherogenic_indices(): missing values in required inputs (na_action='error').",
                   class = "healthmarkers_atherogenic_indices_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    data <- data[keep, , drop = FALSE]
  }

  # Early empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(AIP = numeric(), CRI_I = numeric(), CRI_II = numeric()))
  }

  hm_inform(level = "debug", msg = "atherogenic_indices(): computing indices")

  # Accessor and safe division
  g <- function(key) data[[col_map[[key]]]]
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Compute indices
  AIP   <- log10(safe_div(g("TG"), g("HDL_c"), "AIP_denHDL"))
  CRI_I <- if ("TC" %in% names(col_map) && col_map[["TC"]] %in% names(data)) {
    safe_div(g("TC"), g("HDL_c"), "CRI_I_denHDL")
  } else rep(NA_real_, nrow(data))
  CRI_II <- if ("LDL_c" %in% names(col_map) && col_map[["LDL_c"]] %in% names(data)) {
    safe_div(g("LDL_c"), g("HDL_c"), "CRI_II_denHDL")
  } else rep(NA_real_, nrow(data))

  out <- tibble::tibble(
    AIP = as.numeric(AIP),
    CRI_I = as.numeric(CRI_I),
    CRI_II = as.numeric(CRI_II)
  )

  # Zero denominators warning
  dz <- dz_env$counts
  if (length(dz)) {
    dz_total <- sum(unlist(dz), na.rm = TRUE)
    if (dz_total > 0L) {
      nz <- unlist(dz); nz <- nz[nz > 0]
      lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
      rlang::warn(sprintf("atherogenic_indices(): zero denominators detected in %d cases (%s).", dz_total, lbl),
                  class = "healthmarkers_atherogenic_indices_warn_zero_denominator")
    }
  }

  # --- Prepend ID column if detected
  if (!is.null(id_col)) {
    id_vec        <- data[[id_col]][seq_len(nrow(out))]
    out[[id_col]] <- id_vec
    out           <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out           <- tibble::as_tibble(out)
  }

  hm_inform(
    level = if (isTRUE(verbose)) "inform" else "debug",
    msg   = hm_result_summary(out, fn_name)
  )

  out
}

