#' Prettify numeric values for printing/display
#'
#' Internal utility: format numbers with fixed decimal places, optional p-value format.
#'
#' @param x Numeric vector.
#' @param digits Integer ≥ 0: number of decimal places (for non-pvalue case).
#' @param pvalue Logical: if TRUE, format as p-value, with threshold at 0.001.
#' @return Character vector of same length as x.
#' @noRd
#' @keywords internal
.fnum <- function(x, digits = 2L, pvalue = FALSE) {
  # argument checking
  if (!is.numeric(x)) stop("`x` must be numeric")
  if (! (is.numeric(digits) && length(digits) == 1 && digits >= 0 && digits == as.integer(digits)) ) {
    stop("`digits` must be a single non-negative integer")
  }
  if (!is.logical(pvalue) || length(pvalue) != 1L) {
    stop("`pvalue` must be a single logical (TRUE or FALSE)")
  }

  # helper: replace leading minus (hyphen or default) with en-dash “–”
  replace_minus <- function(s) {
    # replace leading "-" (hyphen-minus U+002D) with U+2013 en dash
    # Only for negative numbers; leave minus sign for zero?
    # Using sub (first match) and anchoring
    sub("^\\-", "\u2013", s)
  }

  # actual formatting
  out <- character(length(x))
  na_idx <- is.na(x)
  finite_idx <- is.finite(x) & !na_idx
  # Others: infinite?
  inf_idx <- is.infinite(x) & !na_idx

  # Handle NA
  out[na_idx] <- NA_character_

  if (!pvalue) {
    # non-pvalue formatting
    # round, then format with exactly 'digits' decimal places
    rd <- round(x[finite_idx], digits = digits)
    # format: ensure nsmall = digits, trim leading/trailing spaces
    fmt <- format(rd, nsmall = digits, trim = TRUE, scientific = FALSE)
    # replace minus
    fmt2 <- replace_minus(fmt)
    out[finite_idx] <- fmt2

    # infinite values: represent as "Inf", "-Inf" (replace minus if needed)
    out[inf_idx] <- ifelse(x[inf_idx] < 0, replace_minus("Inf"), "Inf")
  } else {
    # pvalue formatting
    # for values < .001 → "< 0.001"
    # else format like non-pvalue but with 3 decimals, prefix "= "
    # Decide decimals: here 3
    pv <- x[finite_idx]
    lt_idx <- pv < .001
    # For those "less than"
    out[finite_idx][lt_idx] <- "< 0.001"
    # Others
    other_idx <- !lt_idx
    if (any(other_idx)) {
      rd2 <- round(pv[other_idx], digits = 3)
      fmt2 <- format(rd2, nsmall = 3, trim = TRUE, scientific = FALSE)
      fmt2 <- replace_minus(fmt2)
      out[finite_idx][other_idx] <- paste0("= ", fmt2)
    }
    # infinite: treat similarly: maybe "> 1" or represent as "Inf"?
    out[inf_idx] <- ifelse(x[inf_idx] < 0, replace_minus("Inf"), "Inf")
  }

  out
}
