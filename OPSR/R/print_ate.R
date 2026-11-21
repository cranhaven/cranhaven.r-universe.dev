#' Print Method for ATE Objects
#'
#' @param x an object of class `"ate"`.
#' @param digits minimum number of significant digits to be used for most numbers
#'   (passed to [`stats::printCoefmat`]).
#' @param signif.legend if `TRUE`, a legend for the 'significance stars' is printed.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints `x` in 'pretty' form and returns reformatted treatment effects
#'   invisibly.
#'
#' @method print ate
#' @export
print.ate <- function(x, digits = max(3L, getOption("digits") - 3L),
                      signif.legend = TRUE, ...) {
  p.vals <- to.signif.codes(attr(x, "p.vals"))
  legend <- attr(p.vals, "legend")
  te <- format(x, digits = digits)
  p.vals <- format(p.vals, digits = 3)
  mat <- matrix(nrow = 1, ncol = 2*length(x))
  fill.in.2nd <- function(mat, x, start) {
    j <- start
    for (i in 1:length(x)) {
      mat[, j] <- x[i]
      j <- j + 2
    }
    mat
  }
  mat <- fill.in.2nd(mat, te, 1)
  mat <- fill.in.2nd(mat, p.vals, 2)
  df <- as.data.frame(mat)
  make.col.names <- function(x, n) {
    nm <- lapply(seq_len(n), function(i) {
      c(x[i], "")
    })
    Reduce(c, nm)
  }
  colnames(df) <- make.col.names(names(x), length(x))
  print.data.frame(df)
  if (signif.legend) {
    cat("---\nSignif. codes:  ", legend, sep = "")
  }
  invisible(df)
}
