
#' Validate that col_map is fully specified and data has the mapped columns
#'
#' Utility used across HealthMarkers to validate a column mapping against a data
#' frame. It checks that:
#' - `data` is a data.frame/tibble.
#' - `col_map` is a named list or named character vector.
#' - All required keys in `required_keys` have non-NULL mappings.
#' - All mapped columns exist in `data`.
#' - Optionally flags duplicate mappings and unknown keys.
#' - Emits informative errors or warnings depending on action settings.
#' - Optional verbose summary of what was checked.
#'
#' Backward compatibility:
#' - Defaults keep prior behavior (error on missing map entries and missing columns;
#'   allow extra keys and duplicate columns).
#'
#' @param data A data.frame or tibble
#' @param col_map Named list (or named character vector) of expected keys -> column names
#' @param fun_name Character; name of the calling function for messages (e.g., "fasting_is")
#' @param required_keys Character vector; names in col_map that are required
#' @param optional_keys Character vector; optional keys allowed in col_map (informational)
#' @param allow_extra_keys Logical; allow keys in col_map beyond required/optional. Default TRUE.
#' @param duplicate_action One of c("ignore","warn","error") controlling behavior when the same
#'   source column is mapped to multiple keys. Default "ignore".
#' @param missing_action One of c("error","warn","ignore") for missing map entries among required_keys.
#'   Default "error".
#' @param missing_cols_action One of c("error","warn","ignore") for mapped columns not present in data.
#'   Default "error".
#' @param normalize_map Logical; if TRUE, coerce col_map entries to single character strings or NULL,
#'   trim whitespace, and convert NA_character_ to NULL. Default TRUE.
#' @param trim_ws Logical; trim leading/trailing whitespace when normalize_map = TRUE. Default TRUE.
#' @param return_map Logical; if TRUE, invisibly return the normalized map (named character vector/NULLs).
#'   If FALSE, invisibly return TRUE (backward compatible). Default FALSE.
#' @param verbose Logical; if TRUE, prints a short summary of checks. Default FALSE.
#'
#' @return
#' - If return_map = FALSE (default): invisibly returns TRUE on success.
#' - If return_map = TRUE: invisibly returns the normalized mapping (list with character or NULL entries).
#'
#' @examples
#' \dontrun{
#' # Legacy internal col_map validator (full-featured version)
#' df <- tibble::tibble(G0 = c(5.5, 6.1), I0 = c(60, 88))
#' .hm_validate_col_map(df, list(G0 = "G0", I0 = "I0"), fun_name = "fasting_is")
#' }
#'
#' # Return normalized map
#' .hm_validate_col_map(df, list(G0 = " G0 ", I0 = "I0"), fun_name = "fasting_is", return_map = TRUE)
#'
#' # Will error: missing mapping for I0
#' \dontrun{
#' .hm_validate_col_map(df, list(G0 = "G0", I0 = NULL), fun_name = "fasting_is")
#' }
#'
#' @keywords internal
#' @noRd
.hm_validate_col_map <- function(data,
                            col_map,
                            fun_name = "",
                            required_keys = names(col_map),
                            optional_keys = NULL,
                            allow_extra_keys = TRUE,
                            duplicate_action = c("ignore", "warn", "error"),
                            missing_action = c("error", "warn", "ignore"),
                            missing_cols_action = c("error", "warn", "ignore"),
                            normalize_map = TRUE,
                            trim_ws = TRUE,
                            return_map = FALSE,
                            verbose = FALSE) {
  duplicate_action   <- match.arg(duplicate_action)
  missing_action     <- match.arg(missing_action)
  missing_cols_action <- match.arg(missing_cols_action)

  # ---- Basic input checks ----
  if (!is.data.frame(data)) {
    stop(sprintf("%s(): `data` must be a data.frame or tibble.", fun_name), call. = FALSE)
  }
  if (is.null(col_map)) {
    stop(sprintf("%s(): `col_map` must be a named list or named character vector.", fun_name), call. = FALSE)
  }
  # Accept named character vector too
  if (is.character(col_map) && !is.null(names(col_map))) {
    col_map <- as.list(col_map)
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    stop(sprintf("%s(): `col_map` must be a named list (or named character vector).", fun_name), call. = FALSE)
  }

  # Normalize map entries if requested
  if (isTRUE(normalize_map)) {
    col_map <- .vld_normalize_map(col_map, trim_ws = trim_ws)
  }

  # ---- Keys checks ----
  map_keys <- names(col_map)
  if (!isTRUE(allow_extra_keys)) {
    allowed <- unique(c(required_keys %||% character(0), optional_keys %||% character(0)))
    extra <- setdiff(map_keys, allowed)
    if (length(extra)) {
      stop(sprintf("%s(): unexpected keys in col_map: %s", fun_name, paste(extra, collapse = ", ")), call. = FALSE)
    }
  }

  # 1) ensure the user provided a non-NULL name for every key in required_keys
  missing_map <- required_keys[vapply(required_keys, function(k) {
    is.null(col_map[[k]]) || identical(col_map[[k]], "")
  }, logical(1))]
  if (length(missing_map)) {
    msg <- sprintf("%s(): you must supply col_map entries for: %s", fun_name, paste(missing_map, collapse = ", "))
    if (missing_action == "error") stop(msg, call. = FALSE)
    if (missing_action == "warn")  warning(msg, call. = FALSE)
  }

  # 2) check that data contains every mapped column for required_keys
  # Only evaluate mapped (non-NULL, non-empty) entries
  req_vals <- unlist(col_map[required_keys], use.names = FALSE)
  req_vals <- req_vals[!is.na(req_vals) & nzchar(req_vals)]
  miss_cols_req <- setdiff(req_vals, names(data))
  if (length(miss_cols_req)) {
    msg <- sprintf("%s(): missing required columns: %s", fun_name, paste(miss_cols_req, collapse = ", "))
    if (missing_cols_action == "error") stop(msg, call. = FALSE)
    if (missing_cols_action == "warn")  warning(msg, call. = FALSE)
  }

  # 3) For optional keys provided (non-NULL), check existence too
  if (!is.null(optional_keys) && length(optional_keys)) {
    opt_vals <- unlist(col_map[optional_keys], use.names = FALSE)
    opt_vals <- opt_vals[!is.na(opt_vals) & nzchar(opt_vals)]
    miss_cols_opt <- setdiff(opt_vals, names(data))
    if (length(miss_cols_opt)) {
      msg <- sprintf("%s(): missing optional columns (provided in col_map): %s", fun_name, paste(miss_cols_opt, collapse = ", "))
      if (missing_cols_action == "error") stop(msg, call. = FALSE)
      if (missing_cols_action == "warn")  warning(msg, call. = FALSE)
    }
  }

  # 4) Duplicate mapping check (same source column used multiple times)
  mapped_vals <- unlist(col_map, use.names = FALSE)
  mapped_vals <- mapped_vals[!is.na(mapped_vals) & nzchar(mapped_vals)]
  if (length(mapped_vals)) {
    dups <- mapped_vals[duplicated(tolower(mapped_vals))]
    if (length(dups)) {
      dup_str <- paste(unique(dups), collapse = ", ")
      if (duplicate_action == "error") {
        stop(sprintf("%s(): duplicate column mappings detected: %s", fun_name, dup_str), call. = FALSE)
      } else if (duplicate_action == "warn") {
        warning(sprintf("%s(): duplicate column mappings detected: %s", fun_name, dup_str), call. = FALSE)
      }
    }
  }

  # Verbose summary
  if (isTRUE(verbose)) {
    n_req <- length(required_keys)
    n_opt <- length(optional_keys %||% character(0))
    n_map <- sum(!vapply(col_map[required_keys], is.null, logical(1)))
    message(sprintf("%s(): validated col_map - required keys: %d, mapped: %d, optional keys: %d",
                    fun_name, n_req, n_map, n_opt))
  }

  if (isTRUE(return_map)) {
    return(invisible(col_map))
  }

  invisible(TRUE)
}

# ---- internal helpers --------------------------------------------------------

# Normalize col_map entries to single character strings or NULL (trim whitespace)
.vld_normalize_map <- function(col_map, trim_ws = TRUE) {
  for (k in names(col_map)) {
    val <- col_map[[k]]
    if (is.null(val)) next
    # Accept character vectors; use first non-NA value
    if (is.character(val)) {
      if (length(val) == 0L) {
        col_map[[k]] <- NULL
        next
      }
      # drop NAs, keep first
      v <- val[!is.na(val)]
      v <- if (length(v)) v[1L] else NA_character_
      if (isTRUE(trim_ws) && !is.na(v)) v <- trimws(v)
      if (is.na(v) || identical(v, "")) {
        col_map[[k]] <- NULL
      } else {
        col_map[[k]] <- v
      }
    } else {
      # Coerce other types to character if possible; otherwise set NULL
      v <- tryCatch(as.character(val)[1L], error = function(e) NA_character_)
      if (isTRUE(trim_ws) && !is.na(v)) v <- trimws(v)
      if (is.na(v) || identical(v, "")) col_map[[k]] <- NULL else col_map[[k]] <- v
    }
  }
  col_map
}

# null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

# HM-VS v2: check that mapped columns exist in data (common across functions)
hm_require_columns <- function(data, col_map, required_keys, fn) {
  req_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0(fn, "(): missing required columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = sprintf("healthmarkers_%s_error_missing_columns", fn)
    )
  }
  req_cols
}

# HM-VS v2: high-missingness diagnostics (debug-level)
hm_warn_high_missing <- function(data, cols, prop = 0.2, fn = "healthmarkers") {
  for (cn in intersect(cols, names(data))) {
    x <- data[[cn]]
    n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("%s(): column '%s' has high missingness (%.1f%%).", fn, cn, 100 * pna))
    }
  }
  invisible(TRUE)
}
