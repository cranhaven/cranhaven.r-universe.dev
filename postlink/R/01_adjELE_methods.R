#' Print Method for `adjELE` Objects
#'
#' Provides a concise summary of the adjustment object created by \code{\link{adjELE}},
#' including linkage error assumptions, blocking structure, and weight estimation settings.
#'
#' @param x An object of class \code{adjELE}.
#' @param digits Integer; the number of significant digits to use when printing
#'   numeric values. Defaults to 3.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' This method inspects the internal structure of the adjustment object.
#' It calculates summaries for mismatch rates and audit sizes (e.g., means/ranges)
#' if they vary across blocks, providing a snapshot of the error assumption complexity.
#' It safely handles cases where the reference data is missing or empty.
#'
#' @examples
#' data(brfss, package = "postlink")
#'
#' adj_object <- adjELE(linked.data = brfss,
#'                     m.rate = unique(brfss$m.rate),
#'                     blocks = imonth,
#'                     weight.matrix = "BLUE")
#' print(adj_object)
#'
#' @export
print.adjELE <- function(x, digits = 3, ...) {
 cat("\n Adjustment Object: Exchangeable Linkage Errors \n")

 NextMethod("print")

 cat("\n* Specification:")
 w_mat <- if (!is.null(x$weight.matrix)) x$weight.matrix else "Unknown"
 cat("\n    Weight Matrix:  ", w_mat)

 if (!is.null(x$blocks)) {
  n_unique_blocks <- length(unique(x$blocks[!is.na(x$blocks)]))
  if (n_unique_blocks == 1) {
   cat("\n    Blocks:         Single Block (Global assumption)")
  } else {
   cat("\n    Blocks:         ", format(n_unique_blocks, big.mark = ","), " distinct blocks", sep = "")
  }
 } else {
  cat("\n    Blocks:         None specified")
 }

 cat("\n    Mismatch Rate:  ")
 if (!is.null(x$m.rate)) {
  rates <- x$m.rate
  if (length(unique(rates)) == 1) {
   cat(format(rates[1], digits = digits), "(Constant)")
  } else {
   mean_r <- mean(rates, na.rm = TRUE)
   min_r <- min(rates, na.rm = TRUE)
   max_r <- max(rates, na.rm = TRUE)
   cat(sprintf("Variable (Mean: %.*f, Range: %.*f - %.*f)",
               digits, mean_r, digits, min_r, digits, max_r))
  }
 } else {
  cat("None specified")
 }

 if (!is.null(x$audit.size)) {
  audits <- x$audit.size
  if (length(unique(audits)) == 1) {
   cat("\n    Audit Sample:   Same size", format(audits[1], big.mark = ","))
  } else {
   cat("\n    Audit Sample:   Varied sizes (Range:", min(audits), "-", max(audits), ")")
  }
 } else {
  cat("\n    Audit Sample:   None (Assuming correct match rate(s) are known)")
 }

 cat("\n\n")
 invisible(x)
}
