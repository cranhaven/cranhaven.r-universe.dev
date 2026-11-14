#' Rounds and formats in the same function
#'
#' \code{round_and_format} rounds and formats a numeric vector. This is useful for
#'  providing output for tables or plots in a standardized format.
#'
#' @param x A numeric vector.
#' @param digits Number of decimal places.
#' @param sign_digits Number of significant digits (if given this overrides
#'  \code{digits}).
#' @param scientific logical: whether encoding should be in scientific notation or
#'  not.
#' @param trim logical: if leading blanks for justification to common width should
#'  be excluded or not.
#' @return Rounded and formatted values as characters.
#' @author Geir H. Bolstad
#' @export
round_and_format <- function(x, digits = 2, sign_digits = NULL, scientific = FALSE,
                             trim = TRUE) {
  if (is.null(sign_digits)) {
    format(round(x, digits = digits),
      nsmall = digits, scientific = scientific,
      trim = trim
    )
  } else {
    x <- signif(x, digits = sign_digits)
    x_list <- strsplit(c(as.character(format(abs(x), scientific = FALSE))), "\\.")
    nchar_x <- nchar(x_list[[1]])
    if (abs(x) < 1) {
      sign_digits_displayed <- nchar(x * 10^nchar_x[2])
      nsmall <- nchar_x[2] + sign_digits - sign_digits_displayed
    } else {
      nsmall <- sign_digits - nchar_x[1]
      if (nsmall < 0) nsmall <- 0
    }
    format(x, nsmall = nsmall, scientific = scientific, trim = trim)
  }
}
