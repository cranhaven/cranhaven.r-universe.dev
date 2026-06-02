
#' Impute missing values in a data.frame or tibble (simple, column-wise)
#'
#' Performs deterministic, per-column imputation for numeric variables:
#' - "mean": replace NAs with the column mean
#' - "median": replace NAs with the column median
#' - "zero": replace NAs with 0
#' - "constant": replace NAs with the single value given in `constant`
#'
#' Non-numeric columns are left untouched. If `cols = NULL`, all numeric columns
#' that have at least one NA are selected automatically. NA positions are the only
#' values modified; non-NA entries are preserved as-is.
#'
#' Quality checks:
#' - Warns for high-missingness columns (>= `na_warn_prop`).
#' - Warns and skips imputation when a column has no non-NA values (mean/median undefined).
#' - Coerces only numeric columns; non-numerics in `cols` are skipped with a warning.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param method Character; one of c("mean","median","zero","constant").
#' @param cols Optional character vector of column names to impute. Defaults to all
#'   numeric columns in `data` that contain at least one NA.
#' @param constant Numeric; single value to use when `method = "constant"`.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; threshold for high-missingness warnings per column.
#'   Default 0.2.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#'
#' @return A data.frame/tibble of the same dimensions as `data`, with the specified
#'   columns' missing values imputed.
#' @export
#'
#' @examples
#' df <- tibble::tibble(a = c(1, NA, 3), b = c(NA, NA, 2), c = letters[1:3])
#' impute_missing(df, method = "mean")
#' impute_missing(df, method = "median", verbose = TRUE)
#' impute_missing(df, method = "constant", constant = -1, cols = "a")
impute_missing <- function(data,
                           method   = c("mean", "median", "zero", "constant"),
                           cols     = NULL,
                           constant = NULL,
                           na_warn_prop = 0.2,
                           verbose  = FALSE) {
  # ---- validations ----
  if (!is.data.frame(data)) {
    rlang::abort("impute_missing(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_impute_error_data_type")
  }
  method <- match.arg(method)
  if (!is.numeric(na_warn_prop) || length(na_warn_prop) != 1L ||
      !is.finite(na_warn_prop) || na_warn_prop < 0 || na_warn_prop > 1) {
    rlang::abort("impute_missing(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_impute_error_na_warn_prop")
  }
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      rlang::abort(
        paste("impute_missing(): Some `cols` not in data:", paste(missing_cols, collapse = ", ")),
        class = "healthmarkers_impute_error_missing_cols"
      )
    }
  }
  if (method == "constant") {
    if (!is.numeric(constant) || length(constant) != 1L || !is.finite(constant)) {
      rlang::abort("impute_missing(): `constant` must be a single finite numeric when method = 'constant'.",
                   class = "healthmarkers_impute_error_constant_type")
    }
  }

  # Determine which columns to impute
  if (is.null(cols)) {
    is_num <- vapply(data, is.numeric, logical(1))
    has_na <- vapply(data, anyNA, logical(1))
    cols   <- names(data)[is_num & has_na]
  }

  if (length(cols) == 0L) {
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = "impute_missing(): nothing to impute (no numeric columns with NA)")
    return(data)
  }

  if (isTRUE(verbose)) {
    hm_inform(sprintf("impute_missing(): preparing inputs (%d rows, %d column(s), method='%s')",
                      nrow(data), length(cols), method), level = "inform")
  } else {
    hm_inform("impute_missing(): preparing inputs", level = "debug")
  }

  # High-missingness scan
  .imp_warn_high_missing(data, cols, na_warn_prop = na_warn_prop)

  out <- data
  imputed_counts <- integer(length(cols))
  names(imputed_counts) <- cols

  for (i in seq_along(cols)) {
    col <- cols[i]
    vec <- out[[col]]

    if (!is.numeric(vec)) {
      rlang::warn(sprintf("impute_missing(): column '%s' is not numeric; skipping.", col),
                  class = "healthmarkers_impute_warn_non_numeric_skip")
      next
    }
    nas <- is.na(vec)
    n_nas <- sum(nas)
    if (n_nas == 0L) next

    replacement <- switch(
      method,
      mean     = mean(vec, na.rm = TRUE),
      median   = stats::median(vec, na.rm = TRUE),
      zero     = 0,
      constant = constant
    )

    if (!is.finite(replacement)) {
      rlang::warn(sprintf("impute_missing(): column '%s' has no non-missing values; cannot compute %s. Skipping.",
                          col, method),
                  class = "healthmarkers_impute_warn_no_non_missing")
      next
    }

    if (isTRUE(verbose)) {
      hm_inform(sprintf("impute_missing(): column '%s' -> replacing %d NA(s) with %s",
                        col, n_nas, format(replacement)), level = "debug")
    }

    vec[nas]   <- replacement
    out[[col]] <- vec
    imputed_counts[i] <- n_nas
  }

  total_imp <- sum(imputed_counts)
  by_col <- paste(names(imputed_counts[imputed_counts > 0]),
                  imputed_counts[imputed_counts > 0], sep = "=")
  hm_inform(sprintf("impute_missing(): results: imputed %d values across %d columns%s",
                    total_imp, sum(imputed_counts > 0),
                    if (length(by_col)) paste0(" [", paste(by_col, collapse = ", "), "]") else ""),
            level = if (isTRUE(verbose)) "inform" else "debug")

  return(out)
}

#' Impute missing values via Multiple Imputation (mice)
#'
#' Wraps mice to impute only numeric columns; non-numeric columns are untouched.
#' Requires the mice package to be installed (Suggests). If no numeric columns
#' contain NAs, `data` is returned unchanged.
#'
#' Notes:
#' - At least two numeric columns are typically needed by mice to borrow strength.
#' - This function runs `m` imputations and returns the first completed dataset.
#' - Messages from mice are suppressed; use `verbose = TRUE` here for high-level progress.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param m Integer; number of imputations to run (passed to mice). Default 5.
#' @param cols Optional character vector of numeric columns to impute.
#'   Defaults to all numeric columns with at least one NA.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#' @param ... Additional arguments passed to mice::mice().
#'
#' @return A data.frame/tibble with numeric columns imputed by mice.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("mice", quietly = TRUE)) {
#'   df <- tibble::tibble(a = c(1, NA, 3), b = c(2, 4, NA), c = 5)
#'   impute_mice(df, m = 2, verbose = TRUE)
#' }
#' }
#'
#' @references
#' \insertRef{rubin1987mi}{HealthMarkers}
#' \insertRef{vanbuuren2011mice}{HealthMarkers}
impute_mice <- function(data,
                        m    = 5,
                        cols = NULL,
                        verbose = FALSE,
                        ...) {
  if (!is.data.frame(data)) {
    rlang::abort("impute_mice(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_impute_error_data_type")
  }
  if (!(is.numeric(m) && length(m) == 1L && is.finite(m) && m >= 1)) {
    rlang::abort("impute_mice(): `m` must be a single numeric >= 1.",
                 class = "healthmarkers_impute_error_m_param")
  }
  if (!requireNamespace("mice", quietly = TRUE)) {
    rlang::abort("impute_mice(): package 'mice' is required but not installed. Please install 'mice'.",
                 class = "healthmarkers_impute_error_pkg_missing_mice")
  }

  # Determine numeric columns to impute
  is_num <- vapply(data, is.numeric, logical(1))
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      rlang::abort(
        paste("impute_mice(): Some `cols` not in data:", paste(missing_cols, collapse = ", ")),
        class = "healthmarkers_impute_error_missing_cols"
      )
    }
    is_num <- names(data) %in% cols & is_num
  }
  num_cols <- names(data)[is_num]
  if (length(num_cols) == 0L) {
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = "impute_mice(): no numeric columns selected; returning input unchanged")
    return(data)
  }

  # Subset numeric columns with any NA
  has_na <- vapply(data[num_cols], anyNA, logical(1))
  target_cols <- num_cols[has_na]
  if (length(target_cols) == 0L) {
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = "impute_mice(): no NAs in selected numeric columns; returning input unchanged")
    return(data)
  }
  if (length(target_cols) < 2L) {
    rlang::abort("impute_mice(): need at least two numeric columns with missing values for mice().",
                 class = "healthmarkers_impute_error_min_cols_mice")
  }

    if (isTRUE(verbose)) hm_inform(sprintf("impute_mice(): preparing inputs (%d rows, %d columns)", nrow(data), length(target_cols)), level = "inform") else hm_inform("impute_mice(): preparing inputs", level = "debug")

  num_data <- data[, target_cols, drop = FALSE]

  # suppress messages & warnings from mice
  mids <- suppressWarnings(
    suppressMessages(
      mice::mice(num_data, m = m, printFlag = FALSE, ...)
    )
  )
  out_num <- suppressWarnings(
    suppressMessages(
      mice::complete(mids, 1)
    )
  )

  out <- data
  out[, names(out_num)] <- out_num

  total_imp <- sum(vapply(target_cols, function(cn) sum(is.na(data[[cn]])), integer(1)))
  hm_inform(sprintf("impute_mice(): results: imputed %d values across %d columns.", total_imp, length(target_cols)),
            level = if (isTRUE(verbose)) "inform" else "debug")

  return(out)
}

#' Impute missing values via random forest (missForest)
#'
#' Wraps missForest to impute numeric columns using non-parametric random forests.
#' Requires the missForest package to be installed (Suggests). Non-numeric columns
#' are untouched. If no numeric columns contain NAs, `data` is returned unchanged.
#'
#' Notes:
#' - missForest uses iterative RF training; it can be slow on wide/high-NA data.
#' - Errors (e.g., insufficient unique values) are caught and a deterministic mean
#'   imputation fallback is applied to the selected numeric columns.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param ntree Integer; number of trees to grow in each forest (passed to missForest). Default 100.
#' @param cols Optional character vector of numeric columns to impute.
#'   Defaults to all numeric columns with at least one NA.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#' @param ... Additional arguments passed to missForest::missForest().
#'
#' @return A data.frame/tibble with numeric columns imputed by missForest (or mean fallback).
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("missForest", quietly = TRUE)) {
#'   df <- tibble::tibble(a = c(1, NA, 3), b = c(2, 4, NA), c = 5)
#'   impute_missforest(df, ntree = 50, verbose = TRUE)
#' }
#' }
#'
#' @references \insertRef{stekhoven2012missforest}{HealthMarkers}
impute_missforest <- function(data,
                              ntree = 100,
                              cols  = NULL,
                              verbose = FALSE,
                              ...) {
  if (!is.data.frame(data)) {
    rlang::abort("impute_missforest(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_impute_error_data_type")
  }
  if (!(is.numeric(ntree) && length(ntree) == 1L && is.finite(ntree) && ntree >= 1)) {
    rlang::abort("impute_missforest(): `ntree` must be a single numeric >= 1.",
                 class = "healthmarkers_impute_error_ntree_param")
  }
  if (!requireNamespace("missForest", quietly = TRUE)) {
    rlang::abort("impute_missforest(): package 'missForest' is required but not installed. Please install 'missForest'.",
                 class = "healthmarkers_impute_error_pkg_missing_missforest")
  }

  # Determine numeric columns to impute
  is_num <- vapply(data, is.numeric, logical(1))
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      rlang::abort(
        paste("impute_missforest(): Some `cols` not in data:", paste(missing_cols, collapse = ", ")),
        class = "healthmarkers_impute_error_missing_cols")
    }
    is_num <- names(data) %in% cols & is_num
  }
  num_cols <- names(data)[is_num]
  if (length(num_cols) == 0L) {
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = "impute_missforest(): no numeric columns selected; returning input unchanged")
    return(data)
  }

  # Subset numeric columns with any NA
  has_na <- vapply(data[num_cols], anyNA, logical(1))
  target_cols <- num_cols[has_na]
  if (length(target_cols) == 0L) {
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = "impute_missforest(): no NAs in selected numeric columns; returning input unchanged")
    return(data)
  }
  if (length(target_cols) < 2L) {
    rlang::abort("impute_missforest(): need at least two numeric columns with missing values for missForest().",
                 class = "healthmarkers_impute_error_min_cols_missforest")
  }

    if (isTRUE(verbose)) hm_inform(sprintf("impute_missforest(): preparing inputs (%d rows, %d columns)", nrow(data), length(target_cols)), level = "inform") else hm_inform("impute_missforest(): preparing inputs", level = "debug")

  num_data <- data[, target_cols, drop = FALSE]

  # suppress warnings (e.g., few unique levels) and catch errors
  res <- tryCatch({
    suppressWarnings(
      missForest::missForest(num_data, ntree = ntree, verbose = FALSE, ...)
    )
  }, error = function(e) {
    rlang::warn("impute_missforest(): missForest failed; falling back to mean imputation.",
                class = "healthmarkers_impute_warn_missforest_failed")
    return(NULL)
  })

  out <- data
  if (is.null(res)) {
    out <- impute_missing(out, method = "mean", cols = target_cols, verbose = FALSE)
    if (isTRUE(verbose)) {
      total_imp <- sum(vapply(target_cols, function(cn) sum(is.na(data[[cn]])), integer(1)))
      hm_inform(sprintf("impute_missforest(): results: fallback mean imputation; imputed %d values across %d columns.",
                        total_imp, length(target_cols)), level = "inform")
    } else {
      total_imp <- sum(vapply(target_cols, function(cn) sum(is.na(data[[cn]])), integer(1)))
      hm_inform(sprintf("impute_missforest(): results: fallback mean imputation; imputed %d values across %d columns.",
                        total_imp, length(target_cols)), level = "debug")
    }
    return(out)
  }

  out[, names(res$ximp)] <- res$ximp

  total_imp <- sum(vapply(target_cols, function(cn) sum(is.na(data[[cn]])), integer(1)))
  hm_inform(sprintf("impute_missforest(): results: imputed %d values across %d columns.", total_imp, length(target_cols)),
            level = if (isTRUE(verbose)) "inform" else "debug")

  return(out)
}

# ---- internal helpers (not exported) ----------------------------------------

.imp_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]
    if (!is.numeric(x)) next
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("impute_missing(): column '%s' has high missingness (%.1f%%).",
                          cn, 100 * pna), class = "healthmarkers_impute_warn_high_missingness")
    }
  }
  invisible(TRUE)
}
