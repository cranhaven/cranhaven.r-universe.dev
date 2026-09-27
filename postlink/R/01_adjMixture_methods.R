#' Print Method for `adjMixture` Objects
#'
#' Provides a concise summary of the adjustment object created by \code{\link{adjMixture}},
#' including dataset dimensions, model specifications, and constraints.
#'
#' @param x An object of class \code{adjMixture}.
#' @param digits Integer; the number of significant digits to use when printing
#'   numeric values (e.g., mismatch rates). Defaults to 3.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' This method inspects the reference-based data environment to report the number
#' of linked records without copying the full dataset. It considers cases
#' where components (like constraints or safe matches) are unspecified.
#'
#' @examples
#' # Load the LIFE-M demo dataset
#' data(lifem)
#'
#' # Phase 1: Adjustment Specification
#' adj_object <- adjMixture(
#'  linked.data = lifem,
#'  m.formula = ~ commf + comml,
#'  m.rate = 0.05,
#'  safe.matches = hndlnk
#' )
#'
#' # Print specified adjustment
#' print(adj_object)
#'
#' @export
print.adjMixture <- function(x, digits = 3, ...) {
 cat("\n Adjustment Object: Mixture Model \n")

 NextMethod("print")

 cat("\n* Specification:")
 if (!is.null(x$m.formula)) {
  f_text <- deparse(x$m.formula)
  if (length(f_text) > 1) {
   f_text <- paste(f_text[1], "...")
  }
  cat("\n    Mismatch Model:       ", f_text)
 } else {
  cat("\n    Mismatch Model:        None specified (Set to Intercept-Only)")
 }

 if (!is.null(x$m.rate)) {
  cat("\n    Global Mismatch Rate: ", format(x$m.rate, digits = digits), " (Constrained)", sep = "")
 } else {
  cat("\n    Global Mismatch Rate: Unconstrained (Estimated from data)")
 }

 if (!is.null(x$safe.matches)) {
  n_safe <- sum(x$safe.matches, na.rm = TRUE)
  pct_safe <- if (!is.null(x$data_ref$data) && nrow(x$data_ref$data) > 0) (n_safe / nrow(x$data_ref$data)) * 100 else 0
  cat("\n    Safe Matches:         ", format(n_safe, big.mark = ","), sprintf(" (%.1f%%)", pct_safe), sep = "")
 } else {
  cat("\n    Safe Matches:         None specified")
 }

 cat("\n\n")
 invisible(x)
}
